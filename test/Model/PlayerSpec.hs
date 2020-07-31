{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.PlayerSpec (spec) where

import Prelude (head)
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
