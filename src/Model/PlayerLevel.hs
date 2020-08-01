module Model.PlayerLevel where

import Data.Text (pack, unpack)
import Data.Aeson (Value(String), FromJSON, ToJSON, parseJSON, toJSON)

data PlayerLevel = A | B | C | Cuauh deriving (Show, Read, Eq, Ord)

instance FromJSON PlayerLevel where
    parseJSON (String level) = return . read $ unpack level
    parseJSON _ = fail "expected a string"

instance ToJSON PlayerLevel where
    toJSON = String . pack . show

playerLevelGoalsQuota :: PlayerLevel -> Int
playerLevelGoalsQuota A = 5
playerLevelGoalsQuota B = 10
playerLevelGoalsQuota C = 15
playerLevelGoalsQuota Cuauh = 20
