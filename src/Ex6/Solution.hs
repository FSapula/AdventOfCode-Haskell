module Ex6.Solution (solutionFunc) where

data RaceStats = RaceStats
  { time :: Int
  , bestd :: Int
  }
  deriving (Show)

getRacesList :: [String] -> [String] -> [RaceStats]
getRacesList times distances = map (\(t, d) -> RaceStats t d) $ zip (map (read) times) (map (read) distances)

getDistanceTraveled :: Int -> Int -> Int
getDistanceTraveled maxtime timeheld = timeheld * (maxtime - timeheld)

getWaysToWin :: RaceStats -> [Int]
getWaysToWin race = filter (\d -> d > distanceToBeat) $ map (getDistanceTraveled timeAval) [0 .. timeAval]
 where
  timeAval = time race
  distanceToBeat = bestd race

getOneRace :: [String] -> [String] -> RaceStats
getOneRace timeStrs disStrs = RaceStats time dist
 where
  time = read $ concat timeStrs
  dist = read $ concat disStrs

solutionFunc :: IO ()
solutionFunc = do
  let filename = "./src/Ex6/input.txt"
  filecontent <- readFile filename
  let filelines = lines filecontent
  let times = drop 1 $ words (filelines !! 0)
  let distances = drop 1 $ words (filelines !! 1)
  let races = getRacesList times distances
  let race = getOneRace times distances
  print $ product $ map (length . getWaysToWin) races
  print $ length $ getWaysToWin race
