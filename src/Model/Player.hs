module Model.Player where

import Test.QuickCheck (Arbitrary, NonNegative(..), arbitrary, elements)

data Player = Player
    { playerName :: String
    , playerLevel :: PlayerLevel
    , playerGoalsCount :: Int
    , playerSalary :: Rational
    , playerBonus :: Rational
    , playerTeamName :: String
    } deriving Show

data PlayerLevel = A | B | C | Cuauh deriving Show

data Team = Team
    { teamName :: String
    , teamPlayers :: [Player]
    , teamGoalsCount :: Int
    , teamGoalsQuota :: Int
    }

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
      }

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
