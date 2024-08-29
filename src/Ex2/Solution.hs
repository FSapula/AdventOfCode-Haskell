module Ex2.Solution (solutionFuncGames) where

import Data.Char (isNumber)
import Data.List (maximumBy)
import Data.List.Split

data Game = Game
  { idg :: Int
  , turns :: [Turn]
  }
  deriving (Show)

data Turn = Turn
  { red :: Int
  , green :: Int
  , blue :: Int
  }
  deriving (Show)

parseLine :: String -> Game
parseLine str = Game idg turnslist
 where
  idg = read $ filter (isNumber) $ head (splitOn ":" str)
  turnslist = map (parseTurn) $ splitOn ";" (drop 7 str)

parseTurn :: String -> Turn
parseTurn str = Turn (getRedNum str) (getGreenNum str) (getBlueNum str)

getRedNum :: String -> Int
getRedNum str
  | redInfo /= [] = read $ filter (isNumber) $ head redInfo
  | otherwise = 0
 where
  redInfo = filter (\s -> 'd' `elem` s) (splitOn "," str)

getBlueNum :: String -> Int
getBlueNum str
  | blueInfo /= [] = read $ filter (isNumber) $ head blueInfo
  | otherwise = 0
 where
  blueInfo = filter (\s -> 'b' `elem` s) (splitOn "," str)

getGreenNum :: String -> Int
getGreenNum str
  | greenInfo /= [] = read $ filter (isNumber) $ head greenInfo
  | otherwise = 0
 where
  greenInfo = filter (\s -> 'g' `elem` s) (splitOn "," str)

isTurnPossible :: Turn -> Bool
isTurnPossible turn = (red turn <= 12) && (green turn <= 13) && (blue turn <= 14)

isGamePossible :: Game -> Bool
isGamePossible game = all (isTurnPossible) (turns game)

minimumRedInGame :: Game -> Int
minimumRedInGame game = maximum $ map red (turns game)

minimumBlueInGame :: Game -> Int
minimumBlueInGame game = maximum $ map blue (turns game)

minimumGreenInGame :: Game -> Int
minimumGreenInGame game = maximum $ map green (turns game)

powerMinimalGame :: Game -> Int
powerMinimalGame game = minimumBlueInGame game * minimumRedInGame game * minimumGreenInGame game

solutionFuncGames :: IO ()
solutionFuncGames = do
  let filename = "/Users/filip/haskell/advent/advent/src/Ex2/input.txt"
  filecontent <- readFile filename
  let linelist = lines filecontent
  let games = map (parseLine) linelist
  let possiblegames = filter (isGamePossible) games
  print $ sum $ map idg possiblegames
  print $ sum $ map powerMinimalGame games
