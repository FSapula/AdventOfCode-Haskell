module Ex3.Solution (solutionFuncEngine) where

import Data.Char (isNumber)
import Data.List (group, groupBy)
import Data.List.Split

data PartNumber = PartNumber
  { part :: Int
  , row :: Int
  , col :: Int
  }
  deriving (Show)

data Symbol = Symbol
  { position :: (Int, Int)
  }
  deriving (Show)

filterPeriods :: String -> String
filterPeriods str
  | '.' `elem` str = 'p' : (show $ length str)
  | otherwise = str

parseLineParts :: Int -> Int -> String -> [PartNumber]
parseLineParts row col [] = []
parseLineParts row col (a : xs)
  | isNumber a = [PartNumber (read [a]) row col] ++ parseLineParts row (col + 1) xs
  | otherwise = [] ++ parseLineParts row (col + 1) xs

partGroup :: [PartNumber] -> [PartNumber]
partGroup [] = []
partGroup (a : []) = [a]
partGroup (a : b : xs)
  | col b - col a == 1 = (partGroup (newpart : xs))
  | otherwise = a : (partGroup (b : xs))
 where
  newpart = PartNumber (10 * part a + part b) (row a) (col b)

parseLine :: Int -> String -> [PartNumber]
parseLine linerow line = partGroup $ parseLineParts linerow 0 line

parseLineSymbols :: Int -> String -> [Symbol]
parseLineSymbols linerow line = map (\(i, c) -> Symbol (linerow, i)) $ filter (\(i, c) -> not (isNumber c || c == '.')) $ zip [0 ..] line

parseLineGears :: Int -> String -> [Symbol]
parseLineGears linerow line = map (\(i, c) -> Symbol (linerow, i)) $ filter (\(i, c) -> c == '*') $ zip [0 ..] line

getAdjecentParts :: [PartNumber] -> Symbol -> [PartNumber]
getAdjecentParts partslist symbol = filter (\s -> fst (position symbol) `elem` [(row s - 1) .. (row s + 1)] && snd (position symbol) `elem` [(col s - (length (show (part s)))) .. (col s + 1)]) partslist

getRatio :: [PartNumber] -> Symbol -> Int
getRatio partslist symbol
  | (length partsadj) == 2 = product $ map (part) partsadj
  | otherwise = 0
 where
  partsadj = getAdjecentParts partslist symbol

getAdjecentSymbols :: [Symbol] -> PartNumber -> [Symbol]
getAdjecentSymbols symbolslist partto = filter (\s -> position s `elem` availablepositions) symbolslist
 where
  availablepositions = [(i, j) | i <- [(row partto - 1) .. (row partto + 1)], j <- [(col partto - (length $ show (part partto))) .. (col partto + 1)]]

isPartValid :: [Symbol] -> PartNumber -> Bool
isPartValid symbolslist part = (length $ getAdjecentSymbols symbolslist part) /= 0

solutionFuncEngine :: IO ()
solutionFuncEngine = do
  let filename = "/Users/filip/haskell/advent/advent/src/Ex3/input.txt"
  filecontent <- readFile filename
  let filelines = lines filecontent
  let partslist = concat $ map (\(i, l) -> parseLine i l) (zip [0 ..] filelines)
  let symbolslist = concat $ map (\(i, l) -> parseLineSymbols i l) (zip [0 ..] filelines)
  let validparts = filter (isPartValid symbolslist) partslist
  print $ take 5 partslist
  print $ take 2 symbolslist
  print $ sum $ map (part) validparts
  print $ sum $ map (getRatio validparts) symbolslist
