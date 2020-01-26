-- TODO: Pretty ugly. Needs refactoring.
module Y2019.Day24 (parts) where

import           Data.Bool
import           Data.Char
import qualified Data.IntMap.Strict   as M
import           Data.List
import           Data.Maybe
import qualified Data.Vector           as V
import           Data.List.Split   (chunksOf)

import Parser hiding (queue)


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "32506911")
        , (part2, Just "2025")
        ]


part1 input = show $ findRepeat [] $ parseGrid input
  where
    findRepeat :: [Biodiversity] -> DeepGrid -> Biodiversity
    findRepeat history dgrid
      | elem bio history = bio
      | otherwise        = findRepeat (bio:history) (step dgrid)
      where
        bio = biodiversity $ getLayer dgrid 0
        biodiversity = sum . V.imap (\i t -> bool 0 (2^i) $ t == Bug)

    step :: DeepGrid -> DeepGrid
    step dgrid = dgrid{ layers = layers' }
      where
        layers' = M.singleton 0 . V.imap step' $ getLayer dgrid 0
        step' i Open = bool Open Bug $ elem (adjacent i) [1, 2]
        step' i Bug  = bool Open Bug $ elem (adjacent i) [1]
        adjacent     = length
                     . filter (== Bug)
                     . fmap (\i -> get' dgrid (0, i))
                     . neighbors dgrid

    neighbors :: DeepGrid -> Int -> [Int]
    neighbors dg i =
        [ toIndex dg (x+x', y+y')
        | x' <- [-1..1]
        , y' <- [-1..1]
        , abs (x' - y') == 1
        , x+x' < size dg && x+x' >= 0
        , y+y' < size dg && y+y' >= 0
        ]
      where (x, y) = toCoord dg i


part2 input = show . countBugs $ iterateLast step 200 $ parseGrid input
  where
    countBugs :: DeepGrid -> Int
    countBugs dgrid = sum . fmap countLayerBugs $ M.elems (layers dgrid)
    countLayerBugs  = V.length . V.filter (== Bug)


data DeepGrid     = DeepGrid { layers :: Grid, size :: Int }
type Grid         = M.IntMap (V.Vector Tile)
data Tile         = Open | Bug | Newline deriving (Eq, Show)
type Biodiversity = Int
type Location     = (Int, Int)


-- | The result of iterating over a function the given number of times.
iterateLast :: (a -> a) -> Int -> a -> a
iterateLast _ 0 init = init
iterateLast f c init = iterateLast f (c-1) (f $! init)


getLayer dgrid i = (layers dgrid) M.! i
get' dgrid (lev, i)
  | M.member lev (layers dgrid) = (getLayer dgrid lev) V.! i
  | otherwise                   = Open


step :: DeepGrid -> DeepGrid
step dgrid = dgrid{ layers = layers' }
  where
    layers' = M.mapWithKey nextGeneration $ layers (padLevels dgrid)
    nextGeneration level grid = V.imap nextGen grid
      where
        center = V.length grid `div` 2
        nextGen i t
          | i == center = Open
          | otherwise   = step' i t
        step' i Open = bool Open Bug $ elem (adjacent i) [1, 2]
        step' i Bug  = bool Open Bug $ adjacent i == 1
        adjacent i   = length . filter (hasBug dgrid)
                     $ neighbors dgrid (level, i)

    padLevels :: DeepGrid -> DeepGrid
    padLevels dg = dg''
      where
        (minLev, minGrid) = M.findMin (layers dg)
        (maxLev, maxGrid) = M.findMax (layers dg)
        size' = size dg
        dg'   = bool (mkBlank dg  $ minLev-1) dg  $ V.all (== Open) minGrid
        dg''  = bool (mkBlank dg' $ maxLev+1) dg' $ V.all (== Open) maxGrid
        mkBlank dg lev = dg{ layers = M.insert lev (blank size') (layers dg) }
        blank n = V.replicate (n*n) Open


hasBug :: DeepGrid -> Location -> Bool
hasBug dgrid (level, i)
  | M.member level (layers dgrid) = getLayer dgrid level V.! i == Bug
  | otherwise                     = False


neighbors :: DeepGrid -> Location -> [Location]
neighbors dg (level, i) = filter ((/= size' * size' `div` 2) . snd)
                        $ concat [sameLevel, levelAbove, levelBelow]
  where
    grid   = getLayer dg level
    (x, y) = toCoord dg i
    range  = [-1..1]
    size'  = size $ dg
    srange = [0 .. size' - 1]
    half   = size' `div` 2
    sameLevel =
      [ (level, i')
      | x' <- range
      , y' <- range
      , let i' = toIndex dg (x+x', y+y')
      , abs (x' - y') == 1
      , x+x' < size dg && x+x' >= 0
      , y+y' < size dg && y+y' >= 0
      ]
    levelAbove = concat [up, down, left, right]
      where
        up    = bool [] (fmap (\n -> (level+1, toIndex dg (n,0))) srange)       $ (x,y) == (half  , half-1)
        down  = bool [] (fmap (\n -> (level+1, toIndex dg (n,size'-1))) srange) $ (x,y) == (half  , half+1)
        left  = bool [] (fmap (\n -> (level+1, toIndex dg (0,n))) srange)       $ (x,y) == (half-1, half)
        right = bool [] (fmap (\n -> (level+1, toIndex dg (size'-1,n))) srange) $ (x,y) == (half+1, half)

    levelBelow = concat [up, down, left, right]
      where
        up    = bool [] [(level-1, toIndex dg (half  , half-1))] $ y == 0
        down  = bool [] [(level-1, toIndex dg (half  , half+1))] $ y == size'-1
        left  = bool [] [(level-1, toIndex dg (half-1, half))]   $ x == 0
        right = bool [] [(level-1, toIndex dg (half+1, half))]   $ x == size'-1


parseGrid :: String -> DeepGrid
parseGrid input = DeepGrid
    { layers = M.singleton 0 (V.filter (/= Newline) tiles')
    , size = size'
    }
  where
    tiles' = V.fromList $ parse (some tile) input
    size' = fromJust $ V.findIndex (\t -> t == Newline) tiles'


tile :: Parser Tile
tile
   =  (char '#'  >> pure Bug)
  <|> (char '.'  >> pure Open)
  <|> (char '\n' >> pure Newline)


toIndex dg (x,y) = y * size dg + x
toCoord dg i     = (i `mod` size dg, i `div` size dg)
idx     dg i     = fromMaybe Open $ (getLayer dg 0) V.!? i
