{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.TeamSpec (spec, testPlayers) where

import qualified Data.Map.Strict as M (member, elems)
import TestImport
import Test.QuickCheck (Arbitrary, arbitrary, conjoin, listOf1, property)
import Model.Team
import Model.Player
import Model.PlayerLevel (PlayerLevel(..))
import Model.PlayerSpec () -- We only need Player's instance of Arbitrary typeclass

spec :: Spec
spec = do
  describe "newTeamFromPlayer" $ do
    it "should produce a team with the same name as the given player's teamName" $ property $
      \p -> teamName (newTeamFromPlayer p) == playerTeamName p

    it "should produce a team containing only the given player data" $ conjoin
      [ property $ \p -> teamGoalsCount (newTeamFromPlayer p) == playerGoalsCount p
      , property $ \p -> teamGoalsQuota (newTeamFromPlayer p) == playerGoalsQuota p
      ]

  describe "addPlayerToTeam" $ do
    it "should increment team's goals count by the player's goals count" $ property $
      \p t -> teamGoalsCount (addPlayerToTeam p t) - playerGoalsCount p == teamGoalsCount t

    it "should increment team's goals quota by the player's goals quota" $ property $
      \p t -> teamGoalsQuota (addPlayerToTeam p t) - playerGoalsQuota p == teamGoalsQuota t

  describe "groupPlayersIntoTeams" $ do
    it "should produce a map of teams containing all the given players' team names" $ property $
      \ps -> all (\p -> playerTeamName p `M.member` groupPlayersIntoTeams ps) ps

    it "should sum the correponding players' goals count" $ property $
      \ps -> let teamCheck t = sum (playerGoalsCount <$> filterPlayersByTeam ps t) == teamGoalsCount t
             in all teamCheck . M.elems $ groupPlayersIntoTeams ps

    it "should sum the correponding players' goals quota" $ property $
      \ps -> let teamCheck t = sum (playerGoalsQuota <$> filterPlayersByTeam ps t) == teamGoalsQuota t
             in all teamCheck . M.elems $ groupPlayersIntoTeams ps

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

instance Arbitrary Team where
  arbitrary = do
    name <- arbitrary
    players <- listOf1 arbitrary

    return Team
      { teamName = name
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
