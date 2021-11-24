{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Team.Feature
  ( TeamFeatureName (..),
    TeamFeatureStatus,
    TeamFeatureAppLockConfig (..),
    TeamFeatureSelfDeletingMessagesConfig (..),
    TeamFeatureClassifiedDomainsConfig (..),
    TeamFeatureStatusValue (..),
    FeatureHasNoConfig,
    EnforceAppLock (..),
    KnownTeamFeatureName (..),
    TeamFeatureStatusNoConfig (..),
    TeamFeatureStatusNoConfigAndPaymentStatus (..),
    TeamFeatureStatusWithConfig (..),
    TeamFeatureStatusWithConfigAndPaymentStatus (..),
    HasDeprecatedFeatureName (..),
    AllFeatureConfigs (..),
    PaymentStatus (..),
    PaymentStatusValue (..),
    IncludePaymentStatus (..),
    defaultAppLockStatus,
    defaultClassifiedDomains,
    defaultSelfDeletingMessagesStatus,
    defaultConferenceCalling,

    -- * Swagger
    typeTeamFeatureName,
    typeTeamFeatureStatusValue,
    modelTeamFeatureStatusNoConfig,
    modelTeamFeatureStatusWithConfig,
    modelTeamFeatureAppLockConfig,
    modelTeamFeatureClassifiedDomainsConfig,
    modelTeamFeatureSelfDeletingMessagesConfig,
    modelTeamFeatureStatusWithConfigAndPaymentStatus,
    modelTeamFeatureStatusNoConfigAndPaymentStatus,
    modelForTeamFeature,
    modelPaymentStatus,
  )
where

import qualified Cassandra.CQL as Cass
import Control.Lens.Combinators (dimap)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString as Parser
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..), fromByteString, toByteString')
import Data.Domain (Domain)
import Data.Either.Extra (maybeToEither)
import Data.Kind (Constraint)
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import Imports
import Servant (FromHttpApiData (..))
import Test.QuickCheck.Arbitrary (arbitrary)
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

----------------------------------------------------------------------
-- TeamFeatureName

-- | If you add a constructor here, you need extend multiple defintions, which
--   aren't checked by GHC.
--
--   Follow this Checklist:
--
-- * libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs
--   * add call to 'testRoundTrip'
-- * libs/wire-api/src/Wire/API/Routes/Public/Galley.hs
--   * add a GET (and possible PUT) route with name prefix teamFeature<FEATURE_NAME>
--   * add a GET route with name prefix featureConfig<FEATURE_NAME>
-- * services/galley/src/Galley/API/Internal.hs
--   * add a field to the 'InternalApi routes' record)
-- * libs/galley-types/src/Galley/Types/Teams.hs
--   * FeatureFlags for server config file
--   * roleHiddenPermissions ChangeTeamFeature and ViewTeamFeature
-- * services/galley/src/Galley/API/Teams/Features.hs
--   * extend getAllFeatureConfigs
--   * extend getAllFeatures
-- * services/galley/schema/src/
--   * add a migration like the one in "V43_TeamFeatureDigitalSignatures.hs"
-- * services/galley/test/integration/API/Teams/Feature.hs
--   * add an integration test for the feature
--   * extend testAllFeatures
-- * consider personal-account configurability (like for `conferenceCalling`, see
--     eg. https://github.com/wireapp/wire-server/pull/1811,
--     https://github.com/wireapp/wire-server/pull/1818)
--
-- An example of all the places to change (including compiler errors and failing tests) can be found
-- in eg. https://github.com/wireapp/wire-server/pull/1652.  (applock and conference calling also
-- add interesting aspects, though.)
--
-- Using something like '[minBound..]' on those expressions would require dependent types.  We
-- could generate exhaustive lists of those calls using TH, along the lines of:
--
-- @
-- forAllTeamFeatureNames ::
--   ExpQ {- [forall (a :: TeamFeatureName). b] -} ->
--   ExpQ {- [b] -}
-- forAllTeamFeatureNames =
--   error
--     "...  and then somehow turn the values from '[minBound..]' into \
--     \type applications in the syntax tree"
-- @
--
-- But that seems excessive.  Let's wait for dependent types to be ready in ghc!
data TeamFeatureName
  = TeamFeatureLegalHold
  | TeamFeatureSSO
  | TeamFeatureSearchVisibility
  | TeamFeatureValidateSAMLEmails
  | TeamFeatureDigitalSignatures
  | TeamFeatureAppLock
  | TeamFeatureFileSharing
  | TeamFeatureClassifiedDomains
  | TeamFeatureConferenceCalling
  | TeamFeatureSelfDeletingMessages
  deriving stock (Eq, Show, Ord, Generic, Enum, Bounded, Typeable)
  deriving (Arbitrary) via (GenericUniform TeamFeatureName)

class KnownTeamFeatureName (a :: TeamFeatureName) where
  knownTeamFeatureName :: TeamFeatureName
  type KnownTeamFeatureNameSymbol a :: Symbol

instance KnownTeamFeatureName 'TeamFeatureLegalHold where
  type KnownTeamFeatureNameSymbol 'TeamFeatureLegalHold = "legalhold"
  knownTeamFeatureName = TeamFeatureLegalHold

instance KnownTeamFeatureName 'TeamFeatureSSO where
  type KnownTeamFeatureNameSymbol 'TeamFeatureSSO = "sso"
  knownTeamFeatureName = TeamFeatureSSO

instance KnownTeamFeatureName 'TeamFeatureSearchVisibility where
  type KnownTeamFeatureNameSymbol 'TeamFeatureSearchVisibility = "searchVisibility"
  knownTeamFeatureName = TeamFeatureSearchVisibility

instance KnownTeamFeatureName 'TeamFeatureValidateSAMLEmails where
  type KnownTeamFeatureNameSymbol 'TeamFeatureValidateSAMLEmails = "validateSAMLemails"
  knownTeamFeatureName = TeamFeatureValidateSAMLEmails

instance KnownTeamFeatureName 'TeamFeatureDigitalSignatures where
  type KnownTeamFeatureNameSymbol 'TeamFeatureDigitalSignatures = "digitalSignatures"
  knownTeamFeatureName = TeamFeatureDigitalSignatures

instance KnownTeamFeatureName 'TeamFeatureAppLock where
  type KnownTeamFeatureNameSymbol 'TeamFeatureAppLock = "appLock"
  knownTeamFeatureName = TeamFeatureAppLock

instance KnownTeamFeatureName 'TeamFeatureFileSharing where
  type KnownTeamFeatureNameSymbol 'TeamFeatureFileSharing = "fileSharing"
  knownTeamFeatureName = TeamFeatureFileSharing

instance KnownTeamFeatureName 'TeamFeatureClassifiedDomains where
  type KnownTeamFeatureNameSymbol 'TeamFeatureClassifiedDomains = "classifiedDomains"
  knownTeamFeatureName = TeamFeatureClassifiedDomains

instance KnownTeamFeatureName 'TeamFeatureConferenceCalling where
  type KnownTeamFeatureNameSymbol 'TeamFeatureConferenceCalling = "conferenceCalling"
  knownTeamFeatureName = TeamFeatureConferenceCalling

instance KnownTeamFeatureName 'TeamFeatureSelfDeletingMessages where
  type KnownTeamFeatureNameSymbol 'TeamFeatureSelfDeletingMessages = "selfDeletingMessages"
  knownTeamFeatureName = TeamFeatureSelfDeletingMessages

instance FromByteString TeamFeatureName where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Left e -> fail $ "Invalid TeamFeatureName: " <> show e
        Right "legalhold" -> pure TeamFeatureLegalHold
        Right "sso" -> pure TeamFeatureSSO
        Right "searchVisibility" -> pure TeamFeatureSearchVisibility
        Right "search-visibility" -> pure TeamFeatureSearchVisibility
        Right "validateSAMLemails" -> pure TeamFeatureValidateSAMLEmails
        Right "validate-saml-emails" -> pure TeamFeatureValidateSAMLEmails
        Right "digitalSignatures" -> pure TeamFeatureDigitalSignatures
        Right "digital-signatures" -> pure TeamFeatureDigitalSignatures
        Right "appLock" -> pure TeamFeatureAppLock
        Right "fileSharing" -> pure TeamFeatureFileSharing
        Right "classifiedDomains" -> pure TeamFeatureClassifiedDomains
        Right "conferenceCalling" -> pure TeamFeatureConferenceCalling
        Right "selfDeletingMessages" -> pure TeamFeatureSelfDeletingMessages
        Right t -> fail $ "Invalid TeamFeatureName: " <> T.unpack t

-- TODO: how do we make this consistent with 'KnownTeamFeatureNameSymbol'?  add a test for
-- that?  anyway do we really need both?
instance ToByteString TeamFeatureName where
  builder TeamFeatureLegalHold = "legalhold"
  builder TeamFeatureSSO = "sso"
  builder TeamFeatureSearchVisibility = "searchVisibility"
  builder TeamFeatureValidateSAMLEmails = "validateSAMLemails"
  builder TeamFeatureDigitalSignatures = "digitalSignatures"
  builder TeamFeatureAppLock = "appLock"
  builder TeamFeatureFileSharing = "fileSharing"
  builder TeamFeatureClassifiedDomains = "classifiedDomains"
  builder TeamFeatureConferenceCalling = "conferenceCalling"
  builder TeamFeatureSelfDeletingMessages = "selfDeletingMessages"

instance ToSchema TeamFeatureName where
  schema =
    enum @Text
      "TeamFeatureName"
      $ mconcat
        (map (\feat -> element (cs . toByteString' $ feat) feat) [minBound .. maxBound])

class HasDeprecatedFeatureName (a :: TeamFeatureName) where
  type DeprecatedFeatureName a :: Symbol

instance HasDeprecatedFeatureName 'TeamFeatureSearchVisibility where
  type DeprecatedFeatureName 'TeamFeatureSearchVisibility = "search-visibility"

instance HasDeprecatedFeatureName 'TeamFeatureValidateSAMLEmails where
  type DeprecatedFeatureName 'TeamFeatureValidateSAMLEmails = "validate-saml-emails"

instance HasDeprecatedFeatureName 'TeamFeatureDigitalSignatures where
  type DeprecatedFeatureName 'TeamFeatureDigitalSignatures = "digital-signatures"

typeTeamFeatureName :: Doc.DataType
typeTeamFeatureName = Doc.string . Doc.enum $ cs . toByteString' <$> [(minBound :: TeamFeatureName) ..]

----------------------------------------------------------------------
-- TeamFeatureStatusValue

data TeamFeatureStatusValue
  = TeamFeatureEnabled
  | TeamFeatureDisabled
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamFeatureStatusValue)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamFeatureStatusValue)

typeTeamFeatureStatusValue :: Doc.DataType
typeTeamFeatureStatusValue =
  Doc.string $
    Doc.enum
      [ "enabled",
        "disabled"
      ]

instance ToSchema TeamFeatureStatusValue where
  schema =
    enum @Text "TeamFeatureStatusValue" $
      mconcat
        [ element "enabled" TeamFeatureEnabled,
          element "disabled" TeamFeatureDisabled
        ]

instance ToByteString TeamFeatureStatusValue where
  builder TeamFeatureEnabled = "enabled"
  builder TeamFeatureDisabled = "disabled"

instance FromByteString TeamFeatureStatusValue where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Right "enabled" -> pure TeamFeatureEnabled
        Right "disabled" -> pure TeamFeatureDisabled
        Right t -> fail $ "Invalid TeamFeatureStatusValue: " <> T.unpack t
        Left e -> fail $ "Invalid TeamFeatureStatusValue: " <> show e

instance Cass.Cql TeamFeatureStatusValue where
  ctype = Cass.Tagged Cass.IntColumn

  fromCql (Cass.CqlInt n) = case n of
    0 -> pure TeamFeatureDisabled
    1 -> pure TeamFeatureEnabled
    _ -> Left "fromCql: Invalid TeamFeatureStatusValue"
  fromCql _ = Left "fromCql: TeamFeatureStatusValue: CqlInt expected"

  toCql TeamFeatureDisabled = Cass.CqlInt 0
  toCql TeamFeatureEnabled = Cass.CqlInt 1

----------------------------------------------------------------------
-- TeamFeatureStatus

data IncludePaymentStatus = WithPaymentStatus | WithoutPaymentStatus

type family TeamFeatureStatus (ps :: IncludePaymentStatus) (a :: TeamFeatureName) :: * where
  TeamFeatureStatus _ 'TeamFeatureLegalHold = TeamFeatureStatusNoConfig
  TeamFeatureStatus _ 'TeamFeatureSSO = TeamFeatureStatusNoConfig
  TeamFeatureStatus _ 'TeamFeatureSearchVisibility = TeamFeatureStatusNoConfig
  TeamFeatureStatus _ 'TeamFeatureValidateSAMLEmails = TeamFeatureStatusNoConfig
  TeamFeatureStatus _ 'TeamFeatureDigitalSignatures = TeamFeatureStatusNoConfig
  TeamFeatureStatus _ 'TeamFeatureAppLock = TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
  TeamFeatureStatus _ 'TeamFeatureFileSharing = TeamFeatureStatusNoConfig
  TeamFeatureStatus _ 'TeamFeatureClassifiedDomains = TeamFeatureStatusWithConfig TeamFeatureClassifiedDomainsConfig
  TeamFeatureStatus 'WithoutPaymentStatus 'TeamFeatureConferenceCalling = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'WithPaymentStatus 'TeamFeatureConferenceCalling = TeamFeatureStatusNoConfigAndPaymentStatus
  TeamFeatureStatus 'WithoutPaymentStatus 'TeamFeatureSelfDeletingMessages = TeamFeatureStatusWithConfig TeamFeatureSelfDeletingMessagesConfig
  TeamFeatureStatus 'WithPaymentStatus 'TeamFeatureSelfDeletingMessages = TeamFeatureStatusWithConfigAndPaymentStatus TeamFeatureSelfDeletingMessagesConfig

type family FeatureHasNoConfig (ps :: IncludePaymentStatus) (a :: TeamFeatureName) :: Constraint where
  FeatureHasNoConfig 'WithPaymentStatus a = (TeamFeatureStatus 'WithPaymentStatus a ~ TeamFeatureStatusNoConfigAndPaymentStatus)
  FeatureHasNoConfig 'WithoutPaymentStatus a = (TeamFeatureStatus 'WithoutPaymentStatus a ~ TeamFeatureStatusNoConfig)

-- if you add a new constructor here, don't forget to add it to the swagger (1.2) docs in "Wire.API.Swagger"!
modelForTeamFeature :: TeamFeatureName -> Doc.Model
modelForTeamFeature TeamFeatureLegalHold = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureSSO = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureSearchVisibility = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureValidateSAMLEmails = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureDigitalSignatures = modelTeamFeatureStatusNoConfig
modelForTeamFeature name@TeamFeatureAppLock = modelTeamFeatureStatusWithConfig name modelTeamFeatureAppLockConfig
modelForTeamFeature TeamFeatureFileSharing = modelTeamFeatureStatusNoConfig
modelForTeamFeature name@TeamFeatureClassifiedDomains = modelTeamFeatureStatusWithConfig name modelTeamFeatureClassifiedDomainsConfig
modelForTeamFeature TeamFeatureConferenceCalling = modelTeamFeatureStatusNoConfig
modelForTeamFeature name@TeamFeatureSelfDeletingMessages = modelTeamFeatureStatusWithConfig name modelTeamFeatureSelfDeletingMessagesConfig

----------------------------------------------------------------------
-- TeamFeatureStatusNoConfig

newtype TeamFeatureStatusNoConfig = TeamFeatureStatusNoConfig
  { tfwoStatus :: TeamFeatureStatusValue
  }
  deriving newtype (Eq, Show, Generic, Typeable, Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamFeatureStatusNoConfig)

modelTeamFeatureStatusNoConfig :: Doc.Model
modelTeamFeatureStatusNoConfig = Doc.defineModel "TeamFeatureStatusNoConfig" $ do
  Doc.description "Team feature that has no configuration beyond the boolean on/off switch."
  Doc.property "status" typeTeamFeatureStatusValue $ Doc.description "status"

instance ToSchema TeamFeatureStatusNoConfig where
  schema =
    object "TeamFeatureStatusNoConfig" $
      TeamFeatureStatusNoConfig
        <$> tfwoStatus .= field "status" schema

data TeamFeatureStatusNoConfigAndPaymentStatus = TeamFeatureStatusNoConfigAndPaymentStatus
  { tfwoapsStatus :: TeamFeatureStatusValue,
    tfwoapsPaymentStatus :: PaymentStatusValue
  }
  deriving stock (Eq, Show, Generic, Typeable)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamFeatureStatusNoConfigAndPaymentStatus)

instance Arbitrary TeamFeatureStatusNoConfigAndPaymentStatus where
  arbitrary = TeamFeatureStatusNoConfigAndPaymentStatus <$> arbitrary <*> arbitrary

modelTeamFeatureStatusNoConfigAndPaymentStatus :: Doc.Model
modelTeamFeatureStatusNoConfigAndPaymentStatus = Doc.defineModel "TeamFeatureStatusNoConfigAndPaymentStatus" $ do
  Doc.description "Team feature that has no configuration beyond the boolean on/off switch and a payment status"
  Doc.property "status" typeTeamFeatureStatusValue $ Doc.description ""
  Doc.property "paymentStatus" typePaymentStatusValue $ Doc.description ""

instance ToSchema TeamFeatureStatusNoConfigAndPaymentStatus where
  schema =
    object "TeamFeatureStatusNoConfigAndPaymentStatus" $
      TeamFeatureStatusNoConfigAndPaymentStatus
        <$> tfwoapsStatus .= field "status" schema
        <*> tfwoapsPaymentStatus .= field "paymentStatus" schema

----------------------------------------------------------------------
-- TeamFeatureStatusWithConfig

-- | The support for disabled features with configs is intentional:
-- for instance, we want to be able to keep the config of a feature
-- that is turned on and off occasionally, and so not force the admin
-- to recreate the config every time it's turned on.
data TeamFeatureStatusWithConfig (cfg :: *) = TeamFeatureStatusWithConfig
  { tfwcStatus :: TeamFeatureStatusValue,
    tfwcConfig :: cfg
  }
  deriving stock (Eq, Show, Generic, Typeable)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema (TeamFeatureStatusWithConfig cfg))

instance Arbitrary cfg => Arbitrary (TeamFeatureStatusWithConfig cfg) where
  arbitrary = TeamFeatureStatusWithConfig <$> arbitrary <*> arbitrary

modelTeamFeatureStatusWithConfig :: TeamFeatureName -> Doc.Model -> Doc.Model
modelTeamFeatureStatusWithConfig name cfgModel = Doc.defineModel (cs $ show name) $ do
  Doc.description $ "Status and config of " <> cs (show name)
  Doc.property "status" typeTeamFeatureStatusValue $ Doc.description "status"
  Doc.property "config" (Doc.ref cfgModel) $ Doc.description "config"

instance ToSchema cfg => ToSchema (TeamFeatureStatusWithConfig cfg) where
  schema =
    object "TeamFeatureStatusWithConfig" $
      TeamFeatureStatusWithConfig
        <$> tfwcStatus .= field "status" schema
        <*> tfwcConfig .= field "config" schema

data TeamFeatureStatusWithConfigAndPaymentStatus (cfg :: *) = TeamFeatureStatusWithConfigAndPaymentStatus
  { tfwcapsStatus :: TeamFeatureStatusValue,
    tfwcapsConfig :: cfg,
    tfwcapsPaymentStatus :: PaymentStatusValue
  }
  deriving stock (Eq, Show, Generic, Typeable)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema (TeamFeatureStatusWithConfigAndPaymentStatus cfg))

instance Arbitrary cfg => Arbitrary (TeamFeatureStatusWithConfigAndPaymentStatus cfg) where
  arbitrary = TeamFeatureStatusWithConfigAndPaymentStatus <$> arbitrary <*> arbitrary <*> arbitrary

modelTeamFeatureStatusWithConfigAndPaymentStatus :: TeamFeatureName -> Doc.Model -> Doc.Model
modelTeamFeatureStatusWithConfigAndPaymentStatus name cfgModel = Doc.defineModel (cs $ show name) $ do
  Doc.description $ "Status and config of " <> cs (show name)
  Doc.property "status" typeTeamFeatureStatusValue $ Doc.description "status"
  Doc.property "config" (Doc.ref cfgModel) $ Doc.description "config"
  Doc.property "paymentStatus" typePaymentStatusValue $ Doc.description "config"

instance ToSchema cfg => ToSchema (TeamFeatureStatusWithConfigAndPaymentStatus cfg) where
  schema =
    object "TeamFeatureStatusWithConfigAndPaymentStatus" $
      TeamFeatureStatusWithConfigAndPaymentStatus
        <$> tfwcapsStatus .= field "status" schema
        <*> tfwcapsConfig .= field "config" schema
        <*> tfwcapsPaymentStatus .= field "paymentStatus" schema

----------------------------------------------------------------------
-- TeamFeatureClassifiedDomainsConfig

newtype TeamFeatureClassifiedDomainsConfig = TeamFeatureClassifiedDomainsConfig
  { classifiedDomainsDomains :: [Domain]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamFeatureClassifiedDomainsConfig)

deriving via (GenericUniform TeamFeatureClassifiedDomainsConfig) instance Arbitrary TeamFeatureClassifiedDomainsConfig

instance ToSchema TeamFeatureClassifiedDomainsConfig where
  schema =
    object "TeamFeatureClassifiedDomainsConfig" $
      TeamFeatureClassifiedDomainsConfig
        <$> classifiedDomainsDomains .= field "domains" (array schema)

modelTeamFeatureClassifiedDomainsConfig :: Doc.Model
modelTeamFeatureClassifiedDomainsConfig =
  Doc.defineModel "TeamFeatureClassifiedDomainsConfig" $ do
    Doc.property "domains" (Doc.array Doc.string') $ Doc.description "domains"

defaultClassifiedDomains :: TeamFeatureStatusWithConfig TeamFeatureClassifiedDomainsConfig
defaultClassifiedDomains =
  TeamFeatureStatusWithConfig
    TeamFeatureDisabled
    (TeamFeatureClassifiedDomainsConfig [])

----------------------------------------------------------------------
-- TeamFeatureAppLockConfig

data TeamFeatureAppLockConfig = TeamFeatureAppLockConfig
  { applockEnforceAppLock :: EnforceAppLock,
    applockInactivityTimeoutSecs :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema TeamFeatureAppLockConfig)

deriving via (GenericUniform TeamFeatureAppLockConfig) instance Arbitrary TeamFeatureAppLockConfig

instance ToSchema TeamFeatureAppLockConfig where
  schema =
    object "TeamFeatureAppLockConfig" $
      TeamFeatureAppLockConfig
        <$> applockEnforceAppLock .= field "enforceAppLock" schema
        <*> applockInactivityTimeoutSecs .= field "inactivityTimeoutSecs" schema

newtype EnforceAppLock = EnforceAppLock Bool
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON) via (Schema EnforceAppLock)

instance ToSchema EnforceAppLock where
  schema = EnforceAppLock <$> (\(EnforceAppLock v) -> v) .= schema

modelTeamFeatureAppLockConfig :: Doc.Model
modelTeamFeatureAppLockConfig =
  Doc.defineModel "TeamFeatureAppLockConfig" $ do
    Doc.property "enforceAppLock" Doc.bool' $ Doc.description "enforceAppLock"
    Doc.property "inactivityTimeoutSecs" Doc.int32' $ Doc.description ""

defaultAppLockStatus :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
defaultAppLockStatus =
  TeamFeatureStatusWithConfig
    TeamFeatureEnabled
    (TeamFeatureAppLockConfig (EnforceAppLock False) 60)

----------------------------------------------------------------------
-- TeamFeatureSelfDeletingMessagesConfig

newtype TeamFeatureSelfDeletingMessagesConfig = TeamFeatureSelfDeletingMessagesConfig
  { sdmEnforcedTimeoutSeconds :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema TeamFeatureSelfDeletingMessagesConfig)
  deriving (Arbitrary) via (GenericUniform TeamFeatureSelfDeletingMessagesConfig)

instance ToSchema TeamFeatureSelfDeletingMessagesConfig where
  schema =
    object "TeamFeatureSelfDeletingMessagesConfig" $
      TeamFeatureSelfDeletingMessagesConfig
        <$> sdmEnforcedTimeoutSeconds .= field "enforcedTimeoutSeconds" schema

modelTeamFeatureSelfDeletingMessagesConfig :: Doc.Model
modelTeamFeatureSelfDeletingMessagesConfig =
  Doc.defineModel "TeamFeatureSelfDeletingMessagesConfig" $ do
    Doc.property "enforcedTimeoutSeconds" Doc.int32' $ Doc.description "optional; default: `0` (no enforcement)"

defaultSelfDeletingMessagesStatus :: TeamFeatureStatusWithConfigAndPaymentStatus TeamFeatureSelfDeletingMessagesConfig
defaultSelfDeletingMessagesStatus =
  TeamFeatureStatusWithConfigAndPaymentStatus
    TeamFeatureEnabled
    (TeamFeatureSelfDeletingMessagesConfig 0)
    PaymentLocked

----------------------------------------------------------------------
-- TeamFeatureConferenceCalling

defaultConferenceCalling :: TeamFeatureStatusNoConfigAndPaymentStatus
defaultConferenceCalling = TeamFeatureStatusNoConfigAndPaymentStatus TeamFeatureEnabled PaymentLocked

----------------------------------------------------------------------
-- PaymentStatus

instance FromHttpApiData PaymentStatusValue where
  parseUrlPiece = maybeToEither "Invalid payment status" . fromByteString . cs

data PaymentStatusValue = PaymentLocked | PaymentUnlocked
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PaymentStatusValue)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema PaymentStatusValue)

newtype PaymentStatus = PaymentStatus
  { paymentStatus :: PaymentStatusValue
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema PaymentStatus)
  deriving (Arbitrary) via (GenericUniform PaymentStatus)

instance ToSchema PaymentStatus where
  schema =
    object "PaymentStatus" $
      PaymentStatus
        <$> paymentStatus .= field "paymentStatus" schema

modelPaymentStatus :: Doc.Model
modelPaymentStatus =
  Doc.defineModel "PaymentStatus" $ do
    Doc.property "paymentStatus" typePaymentStatusValue $ Doc.description ""

typePaymentStatusValue :: Doc.DataType
typePaymentStatusValue =
  Doc.string $
    Doc.enum
      [ "locked",
        "unlocked"
      ]

instance ToSchema PaymentStatusValue where
  schema =
    enum @Text "PaymentStatusValue" $
      mconcat
        [ element "locked" PaymentLocked,
          element "unlocked" PaymentUnlocked
        ]

instance ToByteString PaymentStatusValue where
  builder PaymentLocked = "locked"
  builder PaymentUnlocked = "unlocked"

instance FromByteString PaymentStatusValue where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Right "locked" -> pure PaymentLocked
        Right "unlocked" -> pure PaymentUnlocked
        Right t -> fail $ "Invalid PaymentStatusValue: " <> T.unpack t
        Left e -> fail $ "Invalid PaymentStatusValue: " <> show e

instance Cass.Cql PaymentStatusValue where
  ctype = Cass.Tagged Cass.IntColumn

  fromCql (Cass.CqlInt n) = case n of
    0 -> pure PaymentLocked
    1 -> pure PaymentUnlocked
    _ -> Left "fromCql: Invalid PaymentStatusValue"
  fromCql _ = Left "fromCql: PaymentStatusValue: CqlInt expected"

  toCql PaymentLocked = Cass.CqlInt 0
  toCql PaymentUnlocked = Cass.CqlInt 1

----------------------------------------------------------------------
-- internal

data LowerCaseFirst

instance StringModifier LowerCaseFirst where
  getStringModifier (x : xs) = toLower x : xs
  getStringModifier [] = []

newtype AllFeatureConfigs = AllFeatureConfigs {_allFeatureConfigs :: Aeson.Object}
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AllFeatureConfigs)

instance ToSchema AllFeatureConfigs where
  schema =
    named "AllFeatureConfigs" $
      dimap _allFeatureConfigs AllFeatureConfigs jsonObject
