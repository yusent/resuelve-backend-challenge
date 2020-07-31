module Model.Player where

import Import (Text)

data Player = Player
    { name :: Text
    , playerLevel :: PlayerLevel
    , goalsCount :: Int
    , salary :: Rational
    , bonus :: Rational
    , team :: Text
    }

data PlayerLevel = A | B | C | Cuauh

playerGoalsQuota :: Player -> Int
playerGoalsQuota = playerLevelGoalsQuota . playerLevel

playerLevelGoalsQuota :: PlayerLevel -> Int
playerLevelGoalsQuota A = 5
playerLevelGoalsQuota B = 10
playerLevelGoalsQuota C = 15
playerLevelGoalsQuota Cuauh = 20
