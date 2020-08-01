{-# LANGUAGE OverloadedStrings #-}

module Model.PlayerLevelSpec (spec) where

import Data.Aeson (decode, encode)
import TestImport
import Model.PlayerLevel

spec :: Spec
spec = do
  describe "PlayerLevel" $ do
    it "should be decodable from JSON" $ do
      decode "\"A\"" `shouldBe` Just A
      decode "\"B\"" `shouldBe` Just B
      decode "\"C\"" `shouldBe` Just C
      decode "\"Cuauh\"" `shouldBe` Just Cuauh

    it "should be encodable to JSON" $ do
      encode A `shouldBe` "\"A\""
      encode B `shouldBe` "\"B\""
      encode C `shouldBe` "\"C\""
      encode Cuauh `shouldBe` "\"Cuauh\""
