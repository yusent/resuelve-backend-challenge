{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Player where

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
    parseJSON (Object v) = Player
      <$> v .: "nombre"
      <*> v .: "nivel"
      <*> v .: "goles"
      <*> v .: "sueldo"
      <*> v .: "bono"
      <*> v .: "equipo"
      <*> v .: "sueldo_completo"

    parseJSON _ = fail "expected an object"

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
