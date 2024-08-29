module Ex1.Solution (
  solutionFunc,
  solutionFuncsnd,
) where

import Data.Char (isNumber)
import Data.List

parseLine :: String -> Int
parseLine line = read $ head nums : [last nums] where nums = filter (isNumber) line

numSpellings :: [(String, String)]
numSpellings =
  [ ("zero", "0")
  , ("one", "1")
  , ("two", "2")
  , ("three", "3")
  , ("four", "4")
  , ("five", "5")
  , ("six", "6")
  , ("seven", "7")
  , ("eight", "8")
  , ("nine", "9")
  ]

findNum :: String -> (String, Int)
findNum str
  | finds == [] = (str, 1)
  | otherwise = (\(s, n) -> (n, length s)) $ head $ finds
 where
  finds = filter (\(s, n) -> s `isPrefixOf` str) numSpellings

parseLinesnd :: String -> String
parseLinesnd str
  | str == [] = []
  | snd (findNum str) == 1 = (take 1 str) ++ parseLinesnd (drop 1 str)
  | otherwise = fst (findNum str) ++ parseLinesnd (drop 2 str)

solutionFunc :: IO ()
solutionFunc = do
  let filename = "/Users/filip/haskell/advent/advent/src/Ex1/input.txt"
  filecontent <- readFile filename
  let linelist = lines filecontent
  print $ sum $ map parseLine linelist

solutionFuncsnd :: IO ()
solutionFuncsnd = do
  let filename = "./src/Ex1/input.txt"
  filecontent <- readFile filename
  let linelist = lines filecontent
  print $ linelist
  print $ map parseLinesnd linelist
  print $ map parseLine $ map parseLinesnd linelist
  print $ sum $ map parseLine $ map parseLinesnd linelist
