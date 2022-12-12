module Y2022.Day08 (parts) where

import Data.Bifunctor
import Data.List (nub)
import Util.CharMap2D

parts = ( (part1, Just "1700")
        , (part2, Just "470596")
        , map2dParse charToDigit
        )

part1 :: Forest -> String
part1 fs = show . length . nub . concat
         $ visibleTreesFromOutside <$> [minBound..]
  where
    -- | A list of trees ('MapCoord') that are visible when looking at a
    -- 'Forest' from a given 'Direction'.
    visibleTreesFromOutside :: Direction -> [MapCoord]
    visibleTreesFromOutside dir = nub . concat $ seeTrees (-1) <$> searchRuns
      where
        seeTrees :: Int -> [MapCoord] -> [MapCoord]
        seeTrees _       []     = []
        seeTrees acc (c:cs) | tree > acc = c:seeTrees tree cs
                            | otherwise  = seeTrees acc cs
          where
            tree = map2dCell fs c

        -- | A list of 'MapCoord' for each "column" of trees, when looking at a
        -- 'Forest' from a given 'Direction'.
        searchRuns = case dir of U -> [ dirIndices fs dir x | x <- [0..w-1] ]
                                 R -> [ dirIndices fs dir y | y <- [0..h-1] ]
                                 D -> [ dirIndices fs dir x | x <- [0..w-1] ]
                                 L -> [ dirIndices fs dir y | y <- [0..h-1] ]
        (w, h) = (mapWidth fs, mapHeight fs)


part2 :: Forest -> String
part2 fs = show . maximum $ scenicScore <$> map2dCoords fs
  where
    scenicScore = product . countVisibleTrees
    countVisibleTrees c = visibleTreesFromInside c <$> [minBound..]

    -- | The number of trees visible from a given 'MapCoord' inside the
    -- 'Forest', when looking in a given 'Direction'.
    visibleTreesFromInside :: MapCoord -> Direction -> Int
    visibleTreesFromInside (x,y) dir = countSeen trees
      where
        treehouse = map2dCell fs (x,y)

        countSeen []     = 0
        countSeen (c:cs) | tree < treehouse = 1 + countSeen cs
                         | otherwise        = 1
          where
            tree = map2dCell fs c

        trees = filter inFrontOf runs
        inFrontOf (x',y') = case dir of U -> y' < y
                                        R -> x' > x
                                        D -> y' > y
                                        L -> x' < x
        runs = case dir of U -> dirIndices fs dir x
                           R -> dirIndices fs dir y
                           D -> dirIndices fs dir x
                           L -> dirIndices fs dir y


type Forest = CharMap2D Int
data Direction = U | R | D | L deriving (Bounded, Enum, Eq)


dirIndices :: Forest -> Direction -> Int -> [MapCoord]
dirIndices fs dir axis = take steps $ iterate dirF start
  where
    (w, h) = (mapWidth fs, mapHeight fs)
    (steps, start, dirF) = case dir of U -> (h, (axis, h-1), second pred)
                                       R -> (w, (0,   axis), first  succ)
                                       D -> (h, (axis,   0), second succ)
                                       L -> (w, (w-1, axis), first  pred)
