module Ex7.Solution (solutionFunc) where

import Data.List (group, sort)

data Hand = Hand
  { cards :: [Char]
  , bid :: Int
  }
  deriving (Eq, Show)

instance Ord Hand where
  (<=) a b = (a == b) || getHandType a < getHandType b || (getHandType a == getHandType b && compareCardSeniority b a)

getHand :: String -> Hand
getHand line = Hand (wrds !! 0) (read (wrds !! 1)) where wrds = words line

getHandType :: Hand -> Int
getHandType hand
  | 5 `elem` pairs = 6
  | 4 `elem` pairs = 5
  | 3 `elem` pairs && 2 `elem` pairs = 4
  | 3 `elem` pairs = 3
  | length (filter (== 2) pairs) == 2 = 2
  | 2 `elem` pairs = 1
  | otherwise = 0
 where
  pairs = map (length) $ group $ sort $ cards hand

getCardSeniority :: Hand -> Int
getCardSeniority hand = fst $ head $ filter (\t -> snd t == card) $ zip [0 .. 12] "23456789TJQKA"
 where
  card = head $ cards hand

-- a >= b
compareCardSeniority :: Hand -> Hand -> Bool
compareCardSeniority a b
  | getCardSeniority a > getCardSeniority b = True
  | getCardSeniority a < getCardSeniority b = False
  | otherwise = False || (compareCardSeniority a{cards = drop 1 (cards a)} (b{cards = drop 1 (cards b)}))

solutionFunc :: IO ()
solutionFunc = do
  let filename = "./src/Ex7/input.txt"
  filecontent <- readFile filename
  let filelines = lines filecontent
  let hands = map (getHand) filelines
  print $ sum $ map (\(i, c) -> i * bid c) $ zip [1 ..] $ sort hands
