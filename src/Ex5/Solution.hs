module Ex5.Solution (solutionFunc) where

import Data.List (groupBy)

data Map = Map
  { destrs :: Int
  , soutrs :: Int
  , rl :: Int
  }
  deriving (Show, Eq)

data SeedRange = SeedRange
  { start :: Int
  , len :: Int
  }
  deriving (Show, Eq)

getSeedsToPlant :: String -> [Int]
getSeedsToPlant str = map (read) (drop 1 $ words str)

parseLine :: String -> Map
parseLine str = Map (nums !! 0) (nums !! 1) (nums !! 2)
 where
  nums = map (read) (words str)

splitToMaps :: [String] -> [[String]]
splitToMaps lines = map (filter (/= "")) (groupBy (\x y -> y /= "") lines)

goThroughMap :: Int -> [Map] -> Int
goThroughMap x maplist
  | maps /= [] = destrs (maps !! 0) + (x - soutrs (maps !! 0))
  | otherwise = x
 where
  maps = (filter (\m -> x > soutrs m && (x - soutrs m) < (rl m - 1)) maplist)

goToLocation :: Int -> [[Map]] -> Int
goToLocation x maps = foldl (goThroughMap) x maps

pairSeeds :: [Int] -> [SeedRange]
pairSeeds [] = []
pairSeeds (a : b : xs) = SeedRange a b : pairSeeds xs

seedRangToSeeds :: [(Int, Int)] -> [Int]
seedRangToSeeds srlist = concat $ map (\(s, l) -> [s .. (s + l - 1)]) srlist

solutionFunc :: IO ()
solutionFunc = do
  let filename = "./src/Ex5/input.txt"
  filecontent <- readFile filename
  let lineslist = lines filecontent
  let seedsplant = getSeedsToPlant (lineslist !! 0)
  let mapslines = drop 2 lineslist
  let groups = map (drop 1) $ splitToMaps mapslines
  let mapslist = map (map (parseLine)) groups
  let seedranges = pairSeeds seedsplant
  print seedsplant
  print $ goThroughMap (seedsplant !! 0) (mapslist !! 0)
  print $ foldr1 (min) $ map (\x -> goToLocation x mapslist) seedsplant
