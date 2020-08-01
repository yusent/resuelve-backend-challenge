{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SalarySpec (spec) where

import TestImport hiding (length)
import Data.Aeson (encode)
import Data.List (length)
import Model.TeamSpec (testPlayers)
import Model.Player (Player, playerCompleteSalary)

spec :: Spec
spec = withApp $ do
  describe "postPlayersCompleteSalaryR" $ do
    it "Can make requests using JSON, and parse JSON responses" $ do
      submitTestRequest
      statusIs 200

    it "should return a body with the same length as the request body" $ do
      submitTestRequest
      players <- requireJSONResponse

      assertEq "Response body length should be the same as request body length"
        (length (players :: [Player])) (length testPlayers)

    it "should fill all the complete salaries fields" $ do
      submitTestRequest
      players <- requireJSONResponse

      liftIO . forM_ (players :: [Player])
        $ \player -> playerCompleteSalary player `shouldSatisfy` isJust

submitTestRequest :: YesodExample App ()
submitTestRequest = request $ do
  setRequestBody $ encode testPlayers
  addRequestHeader ("Accept", "application/json")
  addRequestHeader ("Content-Type", "application/json")
  setMethod "POST"
  setUrl PlayersCompleteSalaryR
