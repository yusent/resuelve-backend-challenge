{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.PlayerSpec (spec) where

import Prelude (head)
import qualified Data.Set as S (fromList)
import TestImport hiding (head)
import Test.QuickCheck (conjoin, property)
import Model.Player

spec :: Spec
spec = do
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


filterPlayersByTeam :: [Player] -> Team -> [Player]
filterPlayersByTeam ps t = filter ((== teamName t) . playerTeamName) ps
