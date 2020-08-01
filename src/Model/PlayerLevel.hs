module Model.PlayerLevel where

import Data.Text (pack, unpack)
import Data.Aeson.Types (prependFailure, typeMismatch)
import Text.Read (readMaybe)
import Import (Value(String), FromJSON, ToJSON, parseJSON, toJSON)

data PlayerLevel = A | B | C | Cuauh deriving (Show, Read, Eq, Ord)

instance FromJSON PlayerLevel where
  parseJSON (String level) = case readMaybe (unpack level) of
                                  Just l -> return l
                                  Nothing -> fail $ "Unknown level: '" ++ unpack level ++ "'"
  parseJSON invalid = prependFailure "Parsing \"Level\" failed, " (typeMismatch "String" invalid)

instance ToJSON PlayerLevel where
  toJSON = String . pack . show

playerLevelGoalsQuota :: PlayerLevel -> Int
playerLevelGoalsQuota A = 5
playerLevelGoalsQuota B = 10
playerLevelGoalsQuota C = 15
playerLevelGoalsQuota Cuauh = 20
