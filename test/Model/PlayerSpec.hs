{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.PlayerSpec (spec) where

import Prelude (head)
import TestImport hiding (head)
import Test.QuickCheck (property)
import Model.Player

spec :: Spec
spec = do
  describe "playerGoalsQuota" $ do
    it "should return the player's level quota" $ property $
      \p -> playerGoalsQuota p == playerLevelGoalsQuota (playerLevel p)

  describe "addPlayerToTeam" $ do
    it "should add player to the beginning of the player's list" $ property $
      \p t -> head (teamPlayers $ addPlayerToTeam p t) == p

    it "should increment team's goals count by the player's goals count" $ property $
      \p t -> teamGoalsCount (addPlayerToTeam p t) - playerGoalsCount p == teamGoalsCount t

    it "should increment team's goals quota by the player's goals quota" $ property $
      \p t -> teamGoalsQuota (addPlayerToTeam p t) - playerGoalsQuota p == teamGoalsQuota t
