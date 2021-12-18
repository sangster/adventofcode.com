module Y2021.Day11 (parts) where

import           Data.Bool  (bool)
import qualified Data.Vector as V
import           Util.CharMap2D


parts = ( (part1, Just "1588")
        , (part2, Just "517")
        , map2dParse (\c -> (Dark, charToDigit c))
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


type OctopusMap = CharMap2D Octopus
type Octopus = (BioState, Int)
data BioState = Dark | Flashing deriving Show


-- | Increase the energy level of every Octopus and flash those that have a
--   high-enough energy level, propoagating energy to their neighbors.
increaseEnergies :: OctopusMap -> OctopusMap
increaseEnergies = propagateFlashes . map2dMap (\(b, n) -> (b, n+1))


-- | Return the count of Octopuses currently Flashing.
countFlashes :: OctopusMap -> Int
countFlashes map' = V.foldr inc 0 (mapCells map')
  where
    inc (Flashing, _) acc = acc + 1
    inc _             acc = acc


-- | Change the BioState of each Octopus with enough energy and increase the
--   energy of their neighbors, recursively.
propagateFlashes :: OctopusMap -> OctopusMap
propagateFlashes map' | null adjacents = map''
                      | otherwise      = propagateFlashes map''
  where
    map'' = map'{ mapCells = cells' }
    cells' = foldr incEnergy (V.map flashCell $ mapCells map') adjacents
    incEnergy i acc  = acc V.// [(i, (b, n+1))] where (b, n) = acc V.! i -- TODO: inefficient
    flashCell (bio, n) = (bool bio Flashing $ n > 9, n)

    coords    = map2dCoords map'
    pending   = filter (isAboutToFlash . map2dCell map') coords
    adjacents = map2dCoordToIndex map'
            <$> concatMap (map2dNeighborsDiag map') pending

    isAboutToFlash (Flashing, _) = False
    isAboutToFlash (Dark,     n) = n > 9


-- | Stop octopuses from flashing and reset their energy levels.
resetMap :: OctopusMap -> OctopusMap
resetMap = map2dMap (\(_, n) -> (Dark, bool n 0 $ n > 9))
