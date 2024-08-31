module Ex9.Solution (solutionFunc) where

type History = [Int]

getHistory :: String -> History
getHistory line = map (read) (words line)

getDerivative :: History -> History
getDerivative [] = []
getDerivative (a : []) = []
getDerivative (a : b : xs) = (b - a) : getDerivative (b : xs)

getNextVal :: History -> Int
getNextVal history
  | all (== 0) (getDerivative history) = last history
  | otherwise = last history + (getNextVal (getDerivative history))

getPrevVal :: History -> Int
getPrevVal history
  | all (== 0) (getDerivative history) = head history
  | otherwise = head history - (getPrevVal (getDerivative history))

solutionFunc :: IO ()
solutionFunc = do
  let filename = "./src/Ex9/input.txt"
  filecontent <- readFile filename
  let lineslist = lines filecontent
  let historylist = map (getHistory) lineslist
  print $ sum $ map (getNextVal) historylist
  print $ sum $ map (getPrevVal) historylist
