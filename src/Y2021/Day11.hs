module Y2021.Day11 (parts) where

import           Data.Bool  (bool)
import           Data.List  (findIndex)
import           Data.Maybe (catMaybes, fromJust)
import qualified Data.Vector as V
import           Parser


import Util.Color
import Debug.Trace

parts = ( (part1, Just "1588")
        , (part2, Just "517")
        , parseOctopusMap
        )

part1 :: OctopusMap -> String
part1 map' = show $ iterateFlashes map' 100
  where
    -- | Return the total number of flashes after N iterations.
    iterateFlashes :: OctopusMap -> Int -> Int
    iterateFlashes _    0 = 0
    iterateFlashes m n = countFlashes m' + iterateFlashes (resetMap m') (n-1)
      where
        m' = increaseEnergies m


part2 :: OctopusMap -> String
part2 map' = show $ findStep map' 0
  where
    numOct = mapWidth map' * mapHeight map'

    findStep :: OctopusMap -> Int -> Int
    findStep m acc | count == numOct = acc + 1
                   | otherwise       = findStep (resetMap m') (acc + 1)
      where
        count  = countFlashes m'
        m'     = increaseEnergies m


data OctopusMap = OctopusMap { cells     :: V.Vector Octopus
                             , mapHeight :: Int
                             , mapWidth  :: Int
                             }

type Octopus = (BioState, Int)
data BioState = Dark | Flashing deriving Show
type Index = (Int, Int)


parseOctopusMap :: String -> OctopusMap
parseOctopusMap input = OctopusMap { cells = cells', mapHeight = h, mapWidth = w }
  where
    w = fromJust $ findIndex (== '\n') input
    h = length cells' `div` w

    cells' = V.fromList . map mkOctopus . concat
           $ parse (splitSome (char '\n') (many digit')) input

    mkOctopus n = (Dark, n)


-- | Increase the energy level of every Octopus and flash those that have a
--   high-enough energy level, propoagating energy to their neighbors.
increaseEnergies :: OctopusMap -> OctopusMap
increaseEnergies map' = propagateFlashes
                      $ map'{ cells = V.map (\(b, n) -> (b, n+1)) $ cells map' }


-- | Return the count of Octopuses currently Flashing.
countFlashes :: OctopusMap -> Int
countFlashes map' = V.foldr inc 0 (cells map')
  where
    inc (Flashing, _) acc = acc + 1
    inc _             acc = acc


-- | The height at the given Index.
cell :: OctopusMap -> Index -> Octopus
cell sm (x, y) = cells sm V.! (mapWidth sm * y + x)


-- | In-bound Indicies around the given Index, including diagonals.
neighbors :: OctopusMap -> Index -> [Index]
neighbors sm (x, y) = catMaybes [ neighbor (x-1) y
                                , neighbor (x+1) y
                                , neighbor x     (y-1)
                                , neighbor x     (y+1)
                                , neighbor (x-1) (y-1)
                                , neighbor (x-1) (y+1)
                                , neighbor (x+1) (y-1)
                                , neighbor (x+1) (y+1)
                                ]
  where
    neighbor x' y' = bool Nothing (Just (x', y'))
                   $ inBounds mapWidth x' && inBounds mapHeight y'
      where
        inBounds f n = n >= 0 && n < f sm


-- | Change the BioState of each Octopus with enough energy and increase the
--   energy of their neighbors, recursively.
propagateFlashes :: OctopusMap -> OctopusMap
propagateFlashes map' | null adjacents = map''
                      | otherwise      = propagateFlashes map''
  where
    map'' = map'{ cells = cells' }
    cells' = foldr incEnergy (V.map flashCell $ cells map') adjacents
    incEnergy i acc  = acc V.// [(i, (b, n+1))] where (b, n) = acc V.! i -- TODO: inefficient
    flashCell (bio, n) = (bool bio Flashing $ n > 9, n)

    indicies  = cellIndices map'
    pending   = filter (isAboutToFlash . cell map') indicies
    adjacents = coordsToIdx <$> concatMap (neighbors map') pending
    coordsToIdx (x,y) = mapWidth map' * y + x

    isAboutToFlash (Flashing, _) = False
    isAboutToFlash (Dark,     n) = n > 9


-- | Every Index in the OctopusMap.
cellIndices :: OctopusMap -> [Index]
cellIndices OctopusMap { mapWidth = w, mapHeight = h } =
    [(x,y) | y <- [0 .. h - 1], x <- [0 .. w - 1]]


-- | Stop octopuses from flashing and reset their energy levels.
resetMap :: OctopusMap -> OctopusMap
resetMap octos = octos{ cells = cells' }
  where
    cells' = V.map (\(_, n) -> (Dark, bool n 0 (n > 9))) $ cells octos
