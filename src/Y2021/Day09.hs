module Y2021.Day09 (parts) where

import           Data.Bool (bool)
import qualified Data.HashSet as S
import           Data.List (findIndex, sort)
import           Data.Maybe (catMaybes, fromJust)
import qualified Data.Vector as V
import           Parser


parts = ( (part1, Just "575")
        , (part2, Just "1019700")
        , parseSmokeMap
        )

part1 :: SmokeMap -> String
part1 smokeMap = show . sum $ risk smokeMap <$> lowPoints
  where
    lowPoints = filter (isLowPoint smokeMap) $ cellIndices smokeMap
    risk sm xy = cell sm xy + 1


part2 :: SmokeMap -> String
part2 smokeMap = show . product . take 3 . reverse . sort $ length <$> basins
  where
    basins = basin smokeMap <$> filter (isLowPoint smokeMap) indicies
    indicies = cellIndices smokeMap

    basin :: SmokeMap -> Index -> [Index]
    basin sm start = start : basin' (neighbors sm start) (S.singleton start)
      where
        basin' [] _ = []
        basin' (xy:xys) seen | cell sm xy == 9  = basin' xys seen
                             | S.member xy seen = basin' xys seen
                             | otherwise        = xy : rest
          where
            rest = basin' (xys ++ neighbors sm xy) (S.insert xy seen)


data SmokeMap = SmokeMap { cells     :: V.Vector Int
                         , mapHeight :: Int
                         , mapWidth  :: Int
                         } deriving Show
type Index = (Int, Int)


parseSmokeMap :: String -> SmokeMap
parseSmokeMap input = SmokeMap { cells = cells', mapHeight = h, mapWidth = w }
  where
    w = fromJust $ findIndex (== '\n') input
    h = length cells' `div` w

    cells' = V.fromList . concat
           $ parse (splitSome (char '\n') (many digit')) input


-- | Every Index in the SmokeMap.
cellIndices :: SmokeMap -> [Index]
cellIndices SmokeMap { mapWidth = w, mapHeight = h } =
    [(x,y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]


-- | The height at the given Index.
cell :: SmokeMap -> Index -> Int
cell sm (x, y) = cells sm V.! (mapWidth sm * y + x)


-- | In-bound Indicies around the given Index.
neighbors :: SmokeMap -> Index -> [Index]
neighbors sm (x, y) = catMaybes [ neighbor (x-1) y
                                , neighbor (x+1) y
                                , neighbor x     (y-1)
                                , neighbor x     (y+1)
                                ]
  where
    neighbor x' y' = bool Nothing (Just (x', y'))
                   $ inBounds mapWidth x' && inBounds mapHeight y'
      where
        inBounds f n = n >= 0 && n < f sm


isLowPoint :: SmokeMap -> Index -> Bool
isLowPoint sm xy = all (cell sm xy <) $ cell sm <$> neighbors sm xy
