module Y2021.Day06 (parts) where

import qualified Data.Vector.Unboxed as V
import           Parser

parts = ( (part1, Just "374994")
        , (part2, Just "1686252324092")
        , mkGeneration . parse (splitSome (char ',') natural)
        )


part1 :: Generation -> String
part1 gens = show . countFish $ advanceDays 80 gens


part2 :: Generation -> String
part2 gens = show . countFish $ advanceDays 256 gens


type Generation = V.Vector Int
maxAge   = 8
adultAge = maxAge - 2


mkGeneration :: [Int] -> Generation
mkGeneration xs = V.accum (+) (V.replicate (maxAge + 1) 0) [(x, 1) | x <- xs]


advanceDays :: Int -> Generation -> Generation
advanceDays n = last . take (n+1) . iterate advanceDay


advanceDay :: Generation -> Generation
advanceDay prev = V.imap spawn prev
  where
    spawn n _ | n == maxAge   = prev V.! 0
              | n == adultAge = prev V.! 0 + prev V.! (n+1)
              | otherwise     = prev V.! (n+1)


countFish :: Generation -> Int
countFish = V.foldr1 (+)
