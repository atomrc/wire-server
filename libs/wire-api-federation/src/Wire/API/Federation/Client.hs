{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Federation.Client
  ( FederatorClientEnv (..),
    FederatorClientVersionedEnv (..),
    FederatorClient,
    runFederatorClient,
    runFederatorClientToCodensity,
    runVersionedFederatorClientToCodensity,
    performHTTP2Request,
    withHTTP2Request,
    streamingResponseStrictBody,
    headersFromTable,
  )
where

import qualified Control.Exception as E
import Control.Monad.Catch
import Control.Monad.Codensity
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Conversion (toByteString')
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Streaming.Network
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy.Encoding as LText
import Foreign.Marshal.Alloc
import Imports
import qualified Network.HPACK as HTTP2
import qualified Network.HPACK.Token as HTTP2
import qualified Network.HTTP.Media as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP2.Client as HTTP2
import qualified Network.Socket as NS
import qualified Network.Wai.Utilities.Error as Wai
import OpenSSL.Session (SSL, SSLContext)
import qualified OpenSSL.Session as SSL
import Servant.Client
import Servant.Client.Core
import Servant.Types.SourceT
import qualified System.TimeManager
import Util.Options (Endpoint (..))
import Wire.API.Federation.Component
import Wire.API.Federation.Domain (originDomainHeaderName)
import Wire.API.Federation.Error
import Wire.API.Federation.Version
import Wire.API.VersionInfo

data FederatorClientEnv = FederatorClientEnv
  { ceOriginDomain :: Domain,
    ceTargetDomain :: Domain,
    ceFederator :: Endpoint
  }

data FederatorClientVersionedEnv = FederatorClientVersionedEnv
  { cveEnv :: FederatorClientEnv,
    cveVersion :: Maybe Version
  }

-- | A request to a remote backend. The API version of the remote backend is in
-- the environment. The 'MaybeT' layer is used to match endpoint versions (via
-- the 'Alternative' and 'VersionedMonad' instances).
newtype FederatorClient (c :: Component) a = FederatorClient
  { unFederatorClient ::
      MaybeT
        ( ReaderT
            FederatorClientVersionedEnv
            (ExceptT FederatorClientError (Codensity IO))
        )
        a
  }
  deriving newtype
    ( Functor,
      Alternative,
      Applicative,
      Monad,
      MonadReader FederatorClientVersionedEnv,
      MonadError FederatorClientError,
      MonadIO
    )

instance VersionedMonad Version (FederatorClient c) where
  guardVersion p = do
    v <- asks cveVersion
    guard (maybe True p v)

liftCodensity :: Codensity IO a -> FederatorClient c a
liftCodensity = FederatorClient . lift . lift . lift

headersFromTable :: HTTP2.HeaderTable -> [HTTP.Header]
headersFromTable (headerList, _) = flip map headerList $ first HTTP2.tokenKey

connectSocket :: ByteString -> Int -> IO NS.Socket
connectSocket hostname port =
  handle (E.throw . FederatorClientConnectionError)
    . fmap fst
    $ getSocketFamilyTCP hostname port NS.AF_UNSPEC

performHTTP2Request ::
  Maybe SSLContext ->
  HTTP2.Request ->
  ByteString ->
  Int ->
  IO (Either FederatorClientHTTP2Error (ResponseF Builder))
performHTTP2Request mtlsConfig req hostname port = try $ do
  withHTTP2Request mtlsConfig req hostname port $ \resp -> do
    b <-
      fmap (fromRight mempty)
        . runExceptT
        . runSourceT
        . responseBody
        $ resp
    pure $ resp $> foldMap byteString b

withHTTP2Request ::
  Maybe SSLContext ->
  HTTP2.Request ->
  ByteString ->
  Int ->
  (StreamingResponse -> IO a) ->
  IO a
withHTTP2Request mSSLCtx req hostname port k = do
  let clientConfig =
        HTTP2.ClientConfig
          { HTTP2.scheme = "https",
            HTTP2.authority = hostname,
            HTTP2.cacheLimit = 20
          }
  E.handle (E.throw . FederatorClientHTTP2Exception) $
    bracket (connectSocket hostname port) NS.close $ \sock -> do
      let withHTTP2Config k' = case mSSLCtx of
            Nothing -> bracket (HTTP2.allocSimpleConfig sock 4096) HTTP2.freeSimpleConfig k'
            Just sslCtx -> do
              ssl <- E.handle (E.throw . FederatorClientTLSException) $ do
                ssl <- SSL.connection sslCtx sock
                -- We need to strip trailing dot because openssl doesn't ignore
                -- it. https://github.com/openssl/openssl/issues/11560
                let hostnameStr =
                      Text.unpack $ case Text.decodeUtf8 hostname of
                        (Text.stripSuffix "." -> Just withoutTrailingDot) -> withoutTrailingDot
                        noTrailingDot -> noTrailingDot
                SSL.setTlsextHostName ssl hostnameStr
                SSL.enableHostnameValidation ssl hostnameStr
                SSL.connect ssl
                pure ssl
              bracket (allocTLSConfig ssl 4096) freeTLSConfig k'
      withHTTP2Config $ \conf -> do
        HTTP2.run clientConfig conf $ \sendRequest ->
          sendRequest req $ \resp -> do
            let headers = headersFromTable (HTTP2.responseHeaders resp)
                result = fromAction BS.null $ HTTP2.getResponseBodyChunk resp
            case HTTP2.responseStatus resp of
              Nothing -> E.throw FederatorClientNoStatusCode
              Just status ->
                k
                  Response
                    { responseStatusCode = status,
                      responseHeaders = Seq.fromList headers,
                      responseHttpVersion = HTTP.http20,
                      responseBody = result
                    }

instance KnownComponent c => RunClient (FederatorClient c) where
  runRequestAcceptStatus expectedStatuses req = do
    let successfulStatus status =
          maybe
            (HTTP.statusIsSuccessful status)
            (elem status)
            expectedStatuses
    withHTTP2StreamingRequest successfulStatus req $ \resp -> do
      bdy <-
        fmap (either (const mempty) (toLazyByteString . foldMap byteString))
          . runExceptT
          . runSourceT
          . responseBody
          $ resp
      pure $ resp $> bdy

  throwClientError = throwError . FederatorClientServantError

instance KnownComponent c => RunStreamingClient (FederatorClient c) where
  withStreamingRequest = withHTTP2StreamingRequest HTTP.statusIsSuccessful

streamingResponseStrictBody :: StreamingResponse -> IO Builder
streamingResponseStrictBody =
  fmap (either stringUtf8 (foldMap byteString))
    . runExceptT
    . runSourceT
    . responseBody

-- Perform a streaming request to the local federator.
withHTTP2StreamingRequest ::
  forall c a.
  KnownComponent c =>
  (HTTP.Status -> Bool) ->
  Request ->
  (StreamingResponse -> IO a) ->
  FederatorClient c a
withHTTP2StreamingRequest successfulStatus req handleResponse = do
  env <- asks cveEnv
  let baseUrlPath =
        HTTP.encodePathSegments
          [ "rpc",
            domainText (ceTargetDomain env),
            componentName (componentVal @c)
          ]
  let path = baseUrlPath <> requestPath req

  body <- do
    case requestBody req of
      Just (RequestBodyLBS lbs, _) -> pure lbs
      Just (RequestBodyBS bs, _) -> pure (LBS.fromStrict bs)
      Just (RequestBodySource _, _) -> throwError FederatorClientStreamingNotSupported
      Nothing -> pure mempty
  let req' =
        HTTP2.requestBuilder
          (requestMethod req)
          (LBS.toStrict (toLazyByteString path))
          (toList (requestHeaders req) <> [(originDomainHeaderName, toByteString' (ceOriginDomain env))])
          (lazyByteString body)
  let Endpoint (Text.encodeUtf8 -> hostname) (fromIntegral -> port) = ceFederator env
  resp <-
    either throwError pure <=< liftCodensity $
      Codensity $ \k ->
        E.catch
          (withHTTP2Request Nothing req' hostname port (k . Right))
          (k . Left . FederatorClientHTTP2Error)

  if successfulStatus (responseStatusCode resp)
    then liftIO $ handleResponse resp
    else do
      -- in case of an error status code, read the whole body to construct the error
      bdy <- liftIO $ streamingResponseStrictBody resp
      throwError $
        FederatorClientError
          ( mkFailureResponse
              (responseStatusCode resp)
              (ceTargetDomain env)
              (toLazyByteString (requestPath req))
              (toLazyByteString bdy)
          )

mkFailureResponse :: HTTP.Status -> Domain -> LByteString -> LByteString -> Wai.Error
mkFailureResponse status domain path body
  -- If the outward federator fails with 403, that means that there was an
  -- error at the level of the local federator (most likely due to a bug somewhere
  -- in wire-server). It does not make sense to return this error directly to the
  -- client, since it is always due to a server issue, so we map it to a 500
  -- error.
  | HTTP.statusCode status == 403 =
      Wai.mkError
        HTTP.status500
        "federation-local-error"
        ( "Local federator failure: "
            <> LText.decodeUtf8With Text.lenientDecode body
        )
  -- Any other error is interpreted as a correctly formatted wai error, and
  -- returned to the client.
  | otherwise =
      (fromMaybe defaultError (Aeson.decode body))
        { Wai.errorData =
            Just
              Wai.FederationErrorData
                { Wai.federrDomain = domain,
                  Wai.federrPath =
                    "/federation"
                      <> Text.decodeUtf8With Text.lenientDecode (LBS.toStrict path)
                }
        }
  where
    defaultError =
      Wai.mkError
        status
        "unknown-federation-error"
        (LText.decodeUtf8With Text.lenientDecode body)

-- | Run federator client synchronously.
runFederatorClient ::
  FederatorClientEnv ->
  FederatorClient c a ->
  IO (Either FederatorClientError a)
runFederatorClient env =
  lowerCodensity
    . runFederatorClientToCodensity env

runFederatorClientToCodensity ::
  forall c a.
  FederatorClientEnv ->
  FederatorClient c a ->
  Codensity IO (Either FederatorClientError a)
runFederatorClientToCodensity env action = runExceptT $ do
  v <-
    runVersionedFederatorClientToCodensity
      (FederatorClientVersionedEnv env Nothing)
      versionNegotiation
  runVersionedFederatorClientToCodensity @c
    (FederatorClientVersionedEnv env (Just v))
    action

runVersionedFederatorClientToCodensity ::
  FederatorClientVersionedEnv ->
  FederatorClient c a ->
  ExceptT FederatorClientError (Codensity IO) a
runVersionedFederatorClientToCodensity env =
  flip runReaderT env
    . unmaybe
    . runMaybeT
    . unFederatorClient
  where
    unmaybe = (maybe (E.throw FederatorClientVersionMismatch) pure =<<)

versionNegotiation :: FederatorClient 'Brig Version
versionNegotiation =
  let req =
        defaultRequest
          { requestPath = "/api-version",
            requestBody = Just (RequestBodyLBS (Aeson.encode ()), "application" HTTP.// "json"),
            requestHeaders = [],
            requestMethod = HTTP.methodPost
          }
   in withHTTP2StreamingRequest @'Brig HTTP.statusIsSuccessful req $ \resp -> do
        body <- toLazyByteString <$> streamingResponseStrictBody resp
        remoteVersions <- case Aeson.decode body of
          Nothing -> E.throw (FederatorClientVersionNegotiationError InvalidVersionInfo)
          Just info -> pure (Set.fromList (vinfoSupported info))
        case Set.lookupMax (Set.intersection remoteVersions supportedVersions) of
          Just v -> pure v
          Nothing ->
            E.throw . FederatorClientVersionNegotiationError $
              if Set.lookupMax supportedVersions > Set.lookupMax remoteVersions
                then RemoteTooOld
                else RemoteTooNew

freeTLSConfig :: HTTP2.Config -> IO ()
freeTLSConfig cfg = free (HTTP2.confWriteBuffer cfg)

allocTLSConfig :: SSL -> HTTP2.BufferSize -> IO HTTP2.Config
allocTLSConfig ssl bufsize = do
  buf <- mallocBytes bufsize
  timmgr <- System.TimeManager.initialize $ 30 * 1000000
  -- Sometimes the frame header says that the payload length is 0. Reading 0
  -- bytes multiple times seems to be causing errors in openssl. I cannot figure
  -- out why. The previous implementation didn't try to read from the socket
  -- when trying to read 0 bytes, so special handling for 0 maintains that
  -- behaviour.
  let readData prevChunk 0 = pure prevChunk
      readData prevChunk n = do
        -- Handling SSL.ConnectionAbruptlyTerminated as a stream end
        -- (some sites terminate SSL connection right after returning the data).
        chunk <- SSL.read ssl n `catch` \(_ :: SSL.ConnectionAbruptlyTerminated) -> pure mempty
        let chunkLen = BS.length chunk
        if
            | chunkLen == 0 || chunkLen == n ->
                pure (prevChunk <> chunk)
            | chunkLen > n ->
                error "openssl: SSL.read returned more bytes than asked for, this is probably a bug"
            | otherwise ->
                readData (prevChunk <> chunk) (n - chunkLen)
  pure
    HTTP2.Config
      { HTTP2.confWriteBuffer = buf,
        HTTP2.confBufferSize = bufsize,
        HTTP2.confSendAll = SSL.write ssl,
        HTTP2.confReadN = readData mempty,
        HTTP2.confPositionReadMaker = HTTP2.defaultPositionReadMaker,
        HTTP2.confTimeoutManager = timmgr
      }
