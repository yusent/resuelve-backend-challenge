{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.PlayerSpec (spec) where

import TestImport
import Test.QuickCheck (property)
import Model.Player

spec :: Spec
spec = describe "playerGoalsQuota" $ do
  it "is should return the player's level quota" $ property $
    \p -> playerGoalsQuota p == playerLevelGoalsQuota (playerLevel p)
