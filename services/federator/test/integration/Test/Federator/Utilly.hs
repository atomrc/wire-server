module Test.Federator.Utilly where

import Control.Lens
import Data.Aeson
import Data.Char
import Imports

deriveJSONOptions :: Options
deriveJSONOptions = defaultOptions {fieldLabelModifier = labelmod}

labelmod :: String -> String
labelmod = (ix 0 %~ toLower) . dropWhile (not . isUpper)
