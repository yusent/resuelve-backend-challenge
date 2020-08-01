{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.PlayerSpec (spec) where

import Prelude (head)
import qualified Data.Set as S (fromList)
import Data.Aeson (decode, encode)
import TestImport hiding (head)
import Test.QuickCheck (Arbitrary, NonNegative(..), arbitrary, conjoin, elements, listOf1, property)
import Model.Player
import Model.PlayerLevel

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

  describe "newTeamFromPlayer" $ do
    it "should produce a team with the same name as the given player's teamName" $ property $
      \p -> teamName (newTeamFromPlayer p) == playerTeamName p

    it "should produce a team containing only the given player" $ conjoin
      [ property $ \p -> teamPlayers (newTeamFromPlayer p) == [p]
      , property $ \p -> teamGoalsCount (newTeamFromPlayer p) == playerGoalsCount p
      , property $ \p -> teamGoalsQuota (newTeamFromPlayer p) == playerGoalsQuota p
      ]

  describe "addPlayerToTeam" $ do
    it "should add player to the beginning of the player's list" $ property $
      \p t -> head (teamPlayers $ addPlayerToTeam p t) == p

    it "should increment team's goals count by the player's goals count" $ property $
      \p t -> teamGoalsCount (addPlayerToTeam p t) - playerGoalsCount p == teamGoalsCount t

    it "should increment team's goals quota by the player's goals quota" $ property $
      \p t -> teamGoalsQuota (addPlayerToTeam p t) - playerGoalsQuota p == teamGoalsQuota t

  describe "groupPlayersIntoTeams" $ do
    it "should produce a list of teams where no two teams have the same name" $ property $
      \ps -> let teamNames = map teamName $ groupPlayersIntoTeams ps
             in length teamNames == length (S.fromList teamNames)

    it "should produce a list of teams containing all the given players" $ property $
      \ps -> let teams = groupPlayersIntoTeams ps
             in S.fromList (concat $ teamPlayers <$> teams) == S.fromList ps

    it "should produce a list of teams where each contains the right players" $ property $
      \ps -> let teamCheck t = all ((== teamName t) . playerTeamName) $ teamPlayers t
             in all teamCheck $ groupPlayersIntoTeams ps

    it "should sum the correponding players' goals count" $ property $
      \ps -> let teamCheck t = sum (playerGoalsCount <$> filterPlayersByTeam ps t) == teamGoalsCount t
             in all teamCheck $ groupPlayersIntoTeams ps

    it "should sum the correponding players' goals quota" $ property $
      \ps -> let teamCheck t = sum (playerGoalsQuota <$> filterPlayersByTeam ps t) == teamGoalsQuota t
             in all teamCheck $ groupPlayersIntoTeams ps

  describe "fillCompleteSalaries" $ do
    it "should assign the right complete salary to all players" $ do
      let juan : pedro : martin : luis : perez : cuauh : cosme : rulo : _ = fillCompleteSalaries testPlayers
      playerCompleteSalary juan `shouldBe` Just 14950
      playerCompleteSalary pedro `shouldBe` Just 29150
      playerCompleteSalary martin `shouldBe` Just 44850
      playerCompleteSalary luis `shouldBe` Just 59550
      playerCompleteSalary perez `shouldBe` Just (203500 / 3)
      playerCompleteSalary cuauh `shouldBe` Just 130000
      playerCompleteSalary cosme `shouldBe` Just 30000
      playerCompleteSalary rulo `shouldBe` Just 42450

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

instance Arbitrary Team where
  arbitrary = do
    name <- arbitrary
    players <- listOf1 arbitrary

    return Team
      { teamName = name
      , teamPlayers = players
      , teamGoalsCount = sum $ map playerGoalsCount players
      , teamGoalsQuota = sum $ map playerGoalsQuota players
      }

filterPlayersByTeam :: [Player] -> Team -> [Player]
filterPlayersByTeam ps t = filter ((== teamName t) . playerTeamName) ps

testPlayers :: [Player]
testPlayers =
  [ Player
    { playerName = "Juan"
    , playerLevel = A
    , playerGoalsCount = 6
    , playerSalary = 12500
    , playerBonus = 2500
    , playerTeamName = "Resuelve FC"
    , playerCompleteSalary = Nothing
    }
  , Player
    { playerName = "Pedro"
    , playerLevel = B
    , playerGoalsCount = 7
    , playerSalary = 25000
    , playerBonus = 5000
    , playerTeamName = "Resuelve FC"
    , playerCompleteSalary = Nothing
    }
  , Player
    { playerName = "Martín"
    , playerLevel = C
    , playerGoalsCount = 16
    , playerSalary = 37500
    , playerBonus = 7500
    , playerTeamName = "Resuelve FC"
    , playerCompleteSalary = Nothing
    }
  , Player
    { playerName = "Luis"
    , playerLevel = Cuauh
    , playerGoalsCount = 19
    , playerSalary = 50000
    , playerBonus = 10000
    , playerTeamName = "Resuelve FC"
    , playerCompleteSalary = Nothing
    }
  , Player
    { playerName = "Juan Perez"
    , playerLevel = C
    , playerGoalsCount = 10
    , playerSalary = 50000
    , playerBonus = 25000
    , playerTeamName = "rojo"
    , playerCompleteSalary = Nothing
    }
  , Player
    { playerName = "EL Cuauh"
    , playerLevel = Cuauh
    , playerGoalsCount = 30
    , playerSalary = 100000
    , playerBonus = 30000
    , playerTeamName = "azul"
    , playerCompleteSalary = Nothing
    }
  , Player
    { playerName = "Cosme Fulanito"
    , playerLevel = A
    , playerGoalsCount = 7
    , playerSalary = 20000
    , playerBonus = 10000
    , playerTeamName = "azul"
    , playerCompleteSalary = Nothing
    }
  , Player
    { playerName = "El Rulo"
    , playerLevel = B
    , playerGoalsCount = 9
    , playerSalary = 30000
    , playerBonus = 15000
    , playerTeamName = "rojo"
    , playerCompleteSalary = Nothing
    }
  ]
