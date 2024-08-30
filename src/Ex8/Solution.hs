module Ex8.Solution (solutionFunc) where

import Data.Char (isAlphaNum, isAsciiUpper)

data Node = Node
  { name :: String
  , leftp :: String
  , rightp :: String
  }
  deriving (Show)

data Step = L | R deriving (Show, Eq)

getSteps :: String -> [Step]
getSteps [] = []
getSteps (x : xs)
  | x == 'L' = L : getSteps (xs)
  | x == 'R' = R : getSteps (xs)

getNode :: String -> Node
getNode line = Node nodeid leftpath rightpath
 where
  nodeid = head $ words line
  leftpath = filter isAlphaNum $ words line !! 2
  rightpath = filter isAlphaNum $ words line !! 3

goToNextNode :: Step -> Node -> [Node] -> Node
goToNextNode step currentnode nodelist = head $ filter (\n -> name n == destname) nodelist
 where
  destname = if step == L then leftp currentnode else rightp currentnode

solutionLoop :: Int -> [Step] -> [Node] -> Node -> IO ()
solutionLoop stepnum steplist nodelist currentnode = do
  let steptotake = steplist !! stepnum
  let newnode = goToNextNode steptotake currentnode nodelist
  if name newnode /= "ZZZ"
    then do
      solutionLoop (stepnum + 1) steplist nodelist (newnode)
    else
      print $ show (stepnum + 1)

stepsTillEndsOnZ :: Int -> [Step] -> Node -> [Node] -> Int
stepsTillEndsOnZ stepnum steplist currentnode nodelist
  | last (name newnode) == 'Z' = (stepnum + 1)
  | otherwise = stepsTillEndsOnZ (stepnum + 1) steplist newnode nodelist
 where
  newnode = goToNextNode (steplist !! stepnum) currentnode nodelist

solutionFunc :: IO ()
solutionFunc = do
  let filename = "./src/Ex8/input.txt"
  filecontent <- readFile filename
  let lineslist = lines filecontent
  let stepline = head lineslist
  let steps = cycle $ getSteps stepline
  let nodelist = map (getNode) (drop 2 lineslist)
  let destinations = filter (\n -> last (name n) == 'Z') nodelist
  let startnode = head $ filter (\n -> name n == "ZZZ") nodelist
  let startnodesA = filter (\n -> (last $ name n) == 'A') nodelist
  print $ take 10 steps
  print $ startnodesA
  print $ destinations
  print $ foldr1 (lcm) $ map (\n -> stepsTillEndsOnZ 0 steps n nodelist) startnodesA
