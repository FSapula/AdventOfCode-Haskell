module Ex11.Solution (solutionFunc) where

import Data.Char (GeneralCategory (Space))
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)

data Place = Place
  { row :: Int
  , col :: Int
  , typeP :: SpaceType
  }
  deriving (Show)

data SpaceType = G | E deriving (Show, Eq)

type Space = [Place]

parseLine :: Int -> String -> Space
parseLine rownum line = map (\(col, t) -> Place rownum col (getSpaceType t)) $ zip [0 ..] line
 where
  getSpaceType :: Char -> SpaceType
  getSpaceType x
    | x == '.' = E
    | otherwise = G

parseSpace :: String -> [Space]
parseSpace file = map (\(row, line) -> parseLine row line) $ zip [0 ..] linesFile
 where
  linesFile = lines file

emptyRow :: Int -> Int -> Space
emptyRow rownum len = parseLine rownum (replicate len '.')

isRowEmpty :: Space -> Bool
isRowEmpty rw = all (\p -> typeP p == E) rw

expandRows :: Int -> [Space] -> [Space]
expandRows _ [] = []
expandRows ofset (x : xs)
  | isRowEmpty x = (map (\s -> s{row = (row s + ofset)}) x) : (emptyRow (row (head x) + 999999 + ofset) (length x)) : expandRows (ofset + 999999) xs
  | otherwise = map (\s -> s{row = (row s + ofset)}) x : expandRows ofset xs

isSameColumn :: Place -> Place -> Bool
isSameColumn p1 p2 = col p1 == col p2

getEmptyColumns :: [Space] -> [Int]
getEmptyColumns spacerows = map (\c -> col $ head c) $ filter (isRowEmpty) $ groupBy (isSameColumn) $ sortBy (comparing col) $ concat spacerows

expandRowWithColumn :: Int -> [Int] -> Space -> Space
expandRowWithColumn _ _ [] = []
expandRowWithColumn offset emptyColList (x : xs)
  | col x `elem` emptyColList =
      x{col = (col x) + offset}
        : Place (row x) (col x + offset + 999999) E
        : expandRowWithColumn (offset + 999999) emptyColList xs
  | otherwise = x{col = (col x) + offset} : expandRowWithColumn offset emptyColList xs

expandColums :: [Int] -> [Space] -> [Space]
expandColums colList spacerows = map (expandRowWithColumn 0 colList) spacerows

getDistance :: (Place, Place) -> Int
getDistance (p1, p2) = (abs $ row p1 - row p2) + (abs $ col p1 - col p2)

getPairsList :: Space -> [(Place, Place)]
getPairsList [] = []
getPairsList (x : xs) = [(x, p) | p <- xs] ++ getPairsList xs

solutionFunc :: IO ()
solutionFunc = do
  let filename = "./src/Ex11/input.txt"
  filecontent <- readFile filename
  let space = parseSpace filecontent
  let expandedrowsspace = expandRows 0 space
  let expandedspace = expandColums (getEmptyColumns expandedrowsspace) expandedrowsspace
  let cspace = concat expandedspace
  let galaxylist = filter (\p -> typeP p == G) cspace
  let pairs = getPairsList galaxylist
  print $ getEmptyColumns space
  print $ map (col) $ expandedspace !! 0
  print $ galaxylist
  print $ sum $ map (getDistance) pairs
