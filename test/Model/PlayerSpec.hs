{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.PlayerSpec (spec) where

import Data.Aeson (decode, encode)
import TestImport
import Test.QuickCheck (Arbitrary, NonNegative(..), arbitrary, elements, property)
import Model.Player
import Model.PlayerLevel (PlayerLevel(..), playerLevelGoalsQuota)

spec :: Spec
spec = do
  describe "Player" $ do
    let json = "{\
          \ \"nombre\": \"Juan\",\
          \ \"nivel\": \"C\",\
          \ \"goles\": 10,\
          \ \"sueldo\": 50000,\
          \ \"bono\": 25000,\
          \ \"sueldo_completo\":null,\
          \ \"equipo\":\"rojo\"\
        \}"
        player = Player "Juan" C 10 50000 25000 "rojo" Nothing

    it "should be decodable from JSON" $ do
      decode json `shouldBe` Just player

    it "should be isomorphic" $ do
      decode (encode player) `shouldBe` Just player

  describe "playerGoalsQuota" $ do
    it "should return the player's level quota" $ property $
      \p -> playerGoalsQuota p == playerLevelGoalsQuota (playerLevel p)

instance Arbitrary Player where
  arbitrary = do
    name <- arbitrary
    level <- elements [A, B, C, Cuauh]
    NonNegative goalsCount <- arbitrary
    NonNegative salary <- arbitrary
    NonNegative bonus <- arbitrary
    teamName' <- arbitrary

    return Player
      { playerName = name
      , playerLevel = level
      , playerGoalsCount = goalsCount
      , playerSalary = salary
      , playerBonus = bonus
      , playerTeamName = teamName'
      , playerCompleteSalary = Nothing
      }
