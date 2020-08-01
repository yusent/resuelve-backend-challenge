{-# LANGUAGE RecordWildCards #-}

module Model.Team where

import qualified Data.Map.Strict as M (Map, (!), (!?), empty, insert, update)
import Model.Player

data Team = Team
    { teamName :: String
    , teamGoalsCount :: Int
    , teamGoalsQuota :: Int
    } deriving Show

fillCompleteSalaries :: [Player] -> [Player]
fillCompleteSalaries players = fillCompleteSalary <$> players
  where
    fillCompleteSalary p = fillPlayerCompleteSalary (teamsMap M.! playerTeamName p) p
    teamsMap = groupPlayersIntoTeams players

fillPlayerCompleteSalary :: Team -> Player -> Player
fillPlayerCompleteSalary Team{..} player@Player{..} = player
  { playerCompleteSalary = Just $ playerSalary + personalBonus + teamBonus }
  where
    personalBonus = playerBonus * personalQuotaPercentage / 2
    personalQuotaPercentage = min 1 (toRational playerGoalsCount / toRational (playerGoalsQuota player))
    teamBonus = playerBonus * teamQuotaPercentage / 2
    teamQuotaPercentage = min 1 (toRational teamGoalsCount / toRational teamGoalsQuota)

groupPlayersIntoTeams :: [Player] -> M.Map String Team
groupPlayersIntoTeams = foldl insertIntoMap M.empty
  where
    insertIntoMap teamsMap player@Player{..} =
      case teamsMap M.!? playerTeamName of
           Just team -> let updatedTeam = addPlayerToTeam player team
                        in M.update (const $ Just updatedTeam) playerTeamName teamsMap
           Nothing -> M.insert playerTeamName (newTeamFromPlayer player) teamsMap

newTeamFromPlayer :: Player -> Team
newTeamFromPlayer player = Team
  { teamName = playerTeamName player
  , teamGoalsCount = playerGoalsCount player
  , teamGoalsQuota = playerGoalsQuota player
  }

addPlayerToTeam :: Player -> Team -> Team
addPlayerToTeam player team = team
  { teamGoalsCount = playerGoalsCount player + teamGoalsCount team
  , teamGoalsQuota = playerGoalsQuota player + teamGoalsQuota team
  }
