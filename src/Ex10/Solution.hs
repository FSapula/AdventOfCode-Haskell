module Ex10.Solution (solutionFunc) where

data Tile = Tile
  { pipet :: Char
  , row :: Int
  , col :: Int
  , consides :: [Side]
  }
  deriving (Show, Eq)

data Side = U | N | E | W | S deriving (Show, Eq)

getTiles :: [String] -> [Tile]
getTiles linelist =
  filter (\t -> pipet t /= '.') $
    concat $
      map (\(r, l) -> (map (\(col, char) -> Tile char r col (getConnectionSides char)) l)) $
        zip [0 ..] $
          map (zip [0 ..]) linelist

getConnectionSides :: Char -> [Side]
getConnectionSides c
  | c == 'F' = [E, S]
  | c == '-' = [E, W]
  | c == '|' = [N, S]
  | c == 'L' = [N, E]
  | c == 'J' = [N, W]
  | c == '7' = [S, W]
  | c == 'S' = [N, S, E, W]

isConnectable :: Tile -> Tile -> Bool
isConnectable t1 t2 = ((t1 `getSide` t2) `elem` consides t1) && ((t2 `getSide` t1) `elem` consides t2)

getSide :: Tile -> Tile -> Side
getSide tilefrom tileto
  | samerow && col tilefrom - col tileto == -1 = E
  | samerow && col tilefrom - col tileto == 1 = W
  | samecol && row tilefrom - row tileto == 1 = N
  | samecol && row tilefrom - row tileto == -1 = S
 where
  samerow = row tilefrom == row tileto
  samecol = col tilefrom == col tileto

getNeighborhood :: Tile -> [Tile] -> [Tile]
getNeighborhood tile tilelist =
  filter
    ( \t ->
        (samerow t tile && (col t - col tile == 1 || col t - col tile == -1))
          || (samecol t tile && (row t - row tile == 1 || row t - row tile == -1))
    )
    tilelist
 where
  samerow :: Tile -> Tile -> Bool
  samerow t1 t2 = row t1 == row t2

  samecol :: Tile -> Tile -> Bool
  samecol t1 t2 = col t1 == col t2

getConnectedPipes :: [Tile] -> Tile -> [Tile]
getConnectedPipes tilelist tile = filter (isConnectable tile) $ getNeighborhood tile tilelist

nextTileOfLoop :: [Tile] -> Tile -> Tile -> Tile
nextTileOfLoop tilelist tilefrom tile = head $ filter (/= tilefrom) $ getConnectedPipes tilelist tile

getLoop :: [Tile] -> [Tile] -> Tile -> Tile -> [Tile]
getLoop tilelist visited start dest
  | nextTile == dest = [dest]
  | otherwise = start : getLoop tilelist (start : visited) (nextTile) dest
 where
  nextTile = nextTileOfLoop tilelist (head visited) start

solutionFunc :: IO ()
solutionFunc = do
  let filename = "./src/Ex10/input.txt"
  filecontent <- readFile filename
  let filelines = lines filecontent
  let tiles = getTiles filelines
  let animaltile = head $ filter (\t -> pipet t == 'S') tiles
  let contoanimal = getConnectedPipes tiles animaltile
  print $ animaltile
  let loop = getLoop tiles [animaltile] (contoanimal !! 0) (contoanimal !! 1)
  let looplength = 1 + length loop
  print $ 1 + (length loop `div` 2)
