{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Player where

import Control.Monad (when)
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Aeson (Value(Object), FromJSON, ToJSON, (.=), (.:), object, parseJSON, toJSON)
import Numeric (fromRat, showFFloatAlt)
import Model.PlayerLevel

data Player = Player
    { playerName :: String
    , playerLevel :: PlayerLevel
    , playerGoalsCount :: Int
    , playerSalary :: Rational
    , playerBonus :: Rational
    , playerTeamName :: String
    , playerCompleteSalary :: Maybe Rational
    } deriving (Show, Eq, Ord)

instance FromJSON Player where
    parseJSON (Object v) = do
      playerName <- v .: "nombre"
      playerLevel <- v .: "nivel"
      playerGoalsCount <- v .: "goles"
      playerSalary <- v .: "sueldo"
      playerBonus <- v .: "bono"
      playerTeamName <- v .: "equipo"
      playerCompleteSalary <- v .: "sueldo_completo"

      when (playerGoalsCount < 0) $ fail "Expected a non-negative value for 'goles'"
      when (playerSalary < 0) $ fail "Expected a non-negative value for 'sueldo'"
      when (playerBonus < 0) $ fail "Expected a non-negative value for 'bono'"

      return Player{..}

    parseJSON invalid = prependFailure "Parsing Player failed, " (typeMismatch "Object" invalid)

instance ToJSON Player where
    toJSON Player{..} = object
      [ "nombre" .= playerName
      , "nivel" .= playerLevel
      , "goles" .= playerGoalsCount
      , "sueldo" .= rationalToFloat playerSalary
      , "bono" .= rationalToFloat playerBonus
      , "equipo" .= playerTeamName
      , "sueldo_completo" .= fmap rationalToFloat playerCompleteSalary
      ]

playerGoalsQuota :: Player -> Int
playerGoalsQuota = playerLevelGoalsQuota . playerLevel

rationalToFloat :: Rational -> Float
rationalToFloat r = read $ showFFloatAlt (Just 2) (fromRat r :: Double) ""
