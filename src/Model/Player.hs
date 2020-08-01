{-# LANGUAGE RecordWildCards #-}

module Model.Player where

import qualified Data.Map as M (Map, empty, insert)

data Player = Player
    { playerName :: String
    , playerLevel :: PlayerLevel
    , playerGoalsCount :: Int
    , playerSalary :: Rational
    , playerBonus :: Rational
    , playerTeamName :: String
    } deriving (Show, Eq, Ord)

data PlayerLevel = A | B | C | Cuauh deriving (Show, Eq, Ord)

data Team = Team
    { teamName :: String
    , teamPlayers :: [Player]
    , teamGoalsCount :: Int
    , teamGoalsQuota :: Int
    } deriving Show

calculateCompleteSalaries :: [Player] -> M.Map Player Rational
calculateCompleteSalaries players = foldl teamsAccFunc M.empty teams
  where
    teamsAccFunc acc team = foldl (playersAccFunc team) acc $ teamPlayers team
    playersAccFunc team acc player = M.insert player (playerCompleteSalary team player) acc
    teams = groupPlayersIntoTeams players

playerCompleteSalary :: Team -> Player -> Rational
playerCompleteSalary Team{..} p@Player{..} = playerSalary + personalBonus + teamBonus
  where
    personalBonus = playerBonus * personalQuotaPercentage / 2
    personalQuotaPercentage = min 1 (toRational playerGoalsCount / toRational (playerGoalsQuota p))
    teamBonus = playerBonus * teamQuotaPercentage / 2
    teamQuotaPercentage = min 1 (toRational teamGoalsCount / toRational teamGoalsQuota)

playerGoalsQuota :: Player -> Int
playerGoalsQuota = playerLevelGoalsQuota . playerLevel

playerLevelGoalsQuota :: PlayerLevel -> Int
playerLevelGoalsQuota A = 5
playerLevelGoalsQuota B = 10
playerLevelGoalsQuota C = 15
playerLevelGoalsQuota Cuauh = 20

groupPlayersIntoTeams :: [Player] -> [Team]
groupPlayersIntoTeams = groupPlayersIntoTeamsRec []

groupPlayersIntoTeamsRec :: [Team] -> [Player] -> [Team]
groupPlayersIntoTeamsRec teams [] = teams
groupPlayersIntoTeamsRec teams (player : restOfPlayers) = groupPlayersIntoTeamsRec updatedTeams restOfPlayers
  where
    updatedTeams = insertPlayer teams
    insertPlayer [] = [newTeamFromPlayer player]
    insertPlayer (team : restOfTeams)
      | teamName team == playerTeamName player = addPlayerToTeam player team : restOfTeams
      | otherwise = team : insertPlayer restOfTeams

newTeamFromPlayer :: Player -> Team
newTeamFromPlayer player = Team
  { teamName = playerTeamName player
  , teamPlayers = [player]
  , teamGoalsCount = playerGoalsCount player
  , teamGoalsQuota = playerGoalsQuota player
  }

addPlayerToTeam :: Player -> Team -> Team
addPlayerToTeam player team = team
  { teamPlayers = player : teamPlayers team
  , teamGoalsCount = playerGoalsCount player + teamGoalsCount team
  , teamGoalsQuota = playerGoalsQuota player + teamGoalsQuota team
  }
