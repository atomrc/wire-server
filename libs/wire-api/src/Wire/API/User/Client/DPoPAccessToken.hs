{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Wire.API.User.Client.DPoPAccessToken where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..), fromByteString', toByteString')
import Data.Json.Util (base64Schema)
import Data.SOP
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import Data.Swagger.ParamSchema (ToParamSchema (..))
import Data.Text as T
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))

newtype Proof = Proof {unProof :: ByteString}
  deriving (Eq, Show, Generic)
  deriving newtype (FromByteString, ToByteString)

instance ToHttpApiData Proof where
  toQueryParam = cs . toByteString'

instance FromHttpApiData Proof where
  parseQueryParam = maybe (Left "Invalid Proof") Right . fromByteString' . cs

instance ToParamSchema Proof where
  toParamSchema _ = toParamSchema (Proxy @Text)

newtype DPoPAccessToken = DPoPAccessToken {unDPoPAccessToken :: ByteString}
  deriving (Eq, Show, Generic)
  deriving newtype (FromByteString, ToByteString)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema DPoPAccessToken)

instance ToSchema DPoPAccessToken where
  schema = named "DPoPAccessToken" $ unDPoPAccessToken .= fmap DPoPAccessToken base64Schema

instance ToParamSchema DPoPAccessToken where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToHttpApiData DPoPAccessToken where
  toQueryParam = cs . toByteString'

instance FromHttpApiData DPoPAccessToken where
  parseQueryParam = maybe (Left "Invalid DPoPAccessToken") Right . fromByteString' . cs

data AccessTokenType = DPoP
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AccessTokenType)

instance ToSchema AccessTokenType where
  schema =
    enum @Text "AccessTokenType" $
      mconcat
        [ element "DPoP" DPoP
        ]

data DPoPAccessTokenResponse = DPoPAccessTokenResponse
  { datrToken :: DPoPAccessToken,
    datrType :: AccessTokenType,
    datrExpiresIn :: Word64
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema DPoPAccessTokenResponse)

instance ToSchema DPoPAccessTokenResponse where
  schema =
    object "DPoPAccessTokenResponse" $
      DPoPAccessTokenResponse
        <$> datrToken .= field "token" schema
        <*> datrType .= field "type" schema
        <*> datrExpiresIn .= field "expires_in" schema

data DPoPTokenGenerationError
  = -- | DPoP token has an invalid syntax
    DpopSyntaxError
  | -- | DPoP header 'typ' is not 'dpop+jwt'
    DpopTypError
  | -- | DPoP signature algorithm (alg) in JWT header is not a supported algorithm (ES256, ES384, Ed25519)
    DpopUnsupportedAlgorithmError
  | -- | DPoP signature does not correspond to the public key (jwk) in the JWT header
    DpopInvalidSignatureError
  | -- | [client_id] does not correspond to the (sub) claim expressed as URI
    ClientIdMismatchError
  | -- | [backend_nonce] does not correspond to the (nonce) claim in DPoP token (base64url encoded)
    BackendNonceMismatchError
  | -- | [uri] does not correspond to the (htu) claim in DPoP token
    HtuMismatchError
  | -- | method does not correspond to the (htm) claim in DPoP token
    HtmMismatchError
  | -- | (jti) claim is absent in DPoP token
    MissingJtiError
  | -- | (chal) claim is absent in DPoP token
    MissingChallengeError
  | -- | (iat) claim is absent in DPoP token
    MissingIatError
  | -- | (iat) claim in DPoP token is not earlier of now (with [max_skew_secs] leeway)
    IatError
  | -- | (exp) claim is absent in DPoP token
    MissingExpError
  | -- | (exp) claim in DPoP token is larger than supplied [max_expiration]
    ExpMismatchError
  | -- | (exp) claim in DPoP token is sooner than now (with [max_skew_secs] leeway)
    ExpError
  | -- | the client id has an invalid syntax
    ClientIdSyntaxError
  | -- | Error at FFI boundary, probably related to raw pointer
    FfiError
  deriving (Eq, Show, Generic)
