module Ex4.Solution (solutionFunc) where

import Data.Char (isNumber)
import Data.List.Split (splitOn)

data Card = Card
  { idc :: Int
  , winningnums :: [Int]
  , nums :: [Int]
  }
  deriving (Show)

getIdCard :: String -> Int
getIdCard str = read $ filter (isNumber) $ takeWhile (/= ':') str

getWinningNums :: String -> [Int]
getWinningNums str = map (read) $ words $ takeWhile (/= '|') $ (splitOn ":" str) !! 1

getCardNums :: String -> [Int]
getCardNums str = map (read) $ words $ (splitOn "|" str) !! 1

parseLineToCard :: String -> Card
parseLineToCard str = Card (getIdCard str) (getWinningNums str) (getCardNums str)

getMatchesNum :: [Int] -> [Int] -> Int
getMatchesNum [] winnums = 0
getMatchesNum (x : xs) winnnums
  | x `elem` winnnums = 1 + getMatchesNum xs winnnums
  | otherwise = getMatchesNum xs winnnums

getCardPoints :: Card -> Int
getCardPoints card
  | wins == 0 = 0
  | otherwise = 2 ^ (wins - 1)
 where
  wins = getMatchesNum (nums card) (winningnums card)

getWinsNum :: Card -> Int
getWinsNum card = getMatchesNum (nums card) (winningnums card)

idToWins :: [Card] -> Int -> Int
idToWins cardlist i = getWinsNum $ (filter (\c -> idc c == i) cardlist) !! 0

idToCard :: [Card] -> Int -> Card
idToCard cardlist i = (filter (\c -> idc c == i) cardlist) !! 0

getCardNum :: [Card] -> [Card] -> Int
getCardNum reflist [] = 0
getCardNum reflist (x : xs) = wins + getCardNum reflist (woncards ++ xs)
 where
  wins = idToWins reflist (idc x)
  woncards = map (idToCard reflist) $ take wins [idc x + 1 ..]

solutionFunc :: IO ()
solutionFunc = do
  let filepath = "./src/Ex4/input.txt"
  filecontent <- readFile filepath
  let lineslist = lines filecontent
  let cardslist = map (parseLineToCard) lineslist
  print $ sum $ map (getCardPoints) cardslist
  print $ show $ length (cardslist) + getCardNum cardslist cardslist
