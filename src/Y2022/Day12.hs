{-# LANGUAGE ImportQualifiedPost #-}
module Y2022.Day12 (parts) where

import Data.Char
import Data.HashMap.Strict qualified as M
import Data.List
import Data.Maybe
import Data.PQueue.Prio.Min qualified as P
import Util.CharMap2D

parts = ( (part1, Just "481")
        , (part2, Just "480")
        , cellsToTopoMap . map2dParse mapCell
        )
  where
    mapCell 'S' = Start
    mapCell 'E' = End
    mapCell c   = Cell $ ord c - ord 'a'

    cellsToTopoMap :: CharMap2D MapCell -> TopoMap
    cellsToTopoMap m = TopoMap (map2dMap toElev m) s e
      where
        toElev x = case x of Start  -> 0
                             End    -> ord 'z' - ord 'a'
                             Cell n -> n
        s = fromJust $ find ((Start ==) . map2dCell m) $ map2dCoords m
        e = fromJust $ find ((End   ==) . map2dCell m) $ map2dCoords m


part1 :: TopoMap -> String
part1 topoMap = show . length . fromJust $ aStar topoMap


part2 :: TopoMap -> String
part2 tm@TopoMap{ tMap = m } = show $ foldr isBetter initialBest candidates
  where
    initialBest = length . fromJust $ aStar tm
    candidates = filter aWithBNeighbor $ map2dCoords m
      where
        aWithBNeighbor xy = map2dCell m xy == 0 && 1 `elem` neighbors
          where
            neighbors = map2dCell m <$> filter canReach (map2dNeighbors m xy)
            canReach x = map2dCell m x - map2dCell m xy < 2

    isBetter xy best = case maybeLength of
                         Just len -> minimum [len, best]
                         Nothing  -> best
      where
        maybeLength = length <$> aStar tm{ tStart = xy }


data MapCell = Start | End | Cell Elevation deriving (Eq, Show)
data TopoMap = TopoMap { tMap   :: CharMap2D Elevation
                       , tStart :: MapCoord
                       , tGoal  :: MapCoord
                       } deriving Show
type Elevation = Int


-- | Return the shortest path from 'tStart' to 'tEnd', excluding 'tStart', or
-- 'Nothing' if there is no path between them.
aStar :: TopoMap -> Maybe [MapCoord]
aStar (TopoMap m s e) = reverse . backtrack e <$> result
  where
    result = search (P.singleton 0 s)
                    M.empty
                    (M.singleton s 0)
                    (M.singleton s (dist s e))

    dist (x,y) (x',y') = abs (x-x') + abs (y-y')

    search :: P.MinPQueue Int MapCoord
           -> M.HashMap MapCoord MapCoord
           -> M.HashMap MapCoord Int
           -> M.HashMap MapCoord Int
           -> Maybe (M.HashMap MapCoord MapCoord)
    search open cameFrom gScore fScore
        | P.null open  = Nothing
        | current == e = Just cameFrom
        | otherwise    = search open'' cameFrom' gScore' fScore'
      where
        ((g,current),open') = P.deleteFindMin open
        neighbors = filter canReach $ map2dNeighbors m current
        canReach x = map2dCell m x - map2dCell m current < 2

        (open'', cameFrom', gScore', fScore') =
          foldr addNeigh (open', cameFrom, gScore, fScore) neighbors

        addNeigh xy (os, cf, gs, fs) =
          case gs M.!? xy of
            Nothing -> (os', cf', gs', fs')
            Just g' -> if tmpGScore < g'
                       then (os', cf', gs', fs')
                       else (os, cf, gs, fs)
          where
            tmpGScore = g + 1
            f = tmpGScore + dist xy e
            os' = if xy `elem` (snd <$> P.toList os)
                  then os
                  else P.insert f xy os
            cf' = M.insert xy current cf
            gs' = M.insert xy tmpGScore gs
            fs' = M.insert xy f fs


-- | Take a 'cameFrom' map and 'MapCoord' and return the list of MapCoords back
-- to the start.
backtrack :: MapCoord -> M.HashMap MapCoord MapCoord -> [MapCoord]
backtrack xy m = case m M.!? xy of
                   Nothing  -> []
                   Just xy' -> xy : backtrack xy' m
