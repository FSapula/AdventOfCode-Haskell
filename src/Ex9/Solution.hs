module Ex9.Solution (solutionFunc) where

type History = [Int]

getHistory :: String -> History
getHistory line = map (read) (words line)

getDerivative :: History -> History
getDerivative [] = []
getDerivative (a : []) = []
getDerivative (a : b : xs) = (b - a) : getDerivative (b : xs)

degTillZero :: History -> Int
degTillZero history
  | all (== 0) history = 0
  | otherwise = 1 + degTillZero (getDerivative history)

getAllDerivatives :: History -> [History]
getAllDerivatives history
  | all (== 0) history = [history]
  | otherwise = history : (getAllDerivatives $ getDerivative history)

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
  print $ head historylist
  print $ getDerivative $ head historylist
  print $ getAllDerivatives $ head historylist
  print $ getNextVal $ head historylist
  print $ sum $ map (getNextVal) historylist
  print $ sum $ map (getPrevVal) historylist
