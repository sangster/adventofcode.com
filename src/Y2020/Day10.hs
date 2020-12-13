module Y2020.Day10 (parts) where

import           Data.List (foldr, sort)
import qualified Data.Vector as V


parts = ( (part1, Just "2244")
        , (part2, Just "3947645370368")
        , parseAdapters
        )


part1 adapters = show . uncurry (*) . count1and3 $ diffs adapters


part2 adapters = show $ numArrangements adapters


parseAdapters :: String -> [Int]
parseAdapters input = concat [[0], inputs, [builtin]]
  where
    builtin = last inputs + 3
    inputs  = sort $ read <$> lines input


count1and3 :: [Int] -> (Int, Int)
count1and3 []     = (0, 0)
count1and3 (x:xs) | x == 1    = (n1+1, n3  )
                  | x == 3    = (n1  , n3+1)
                  | otherwise = (n1,   n3  )
  where
    (n1, n3) = count1and3 xs


diffs :: [Int] -> [Int]
diffs adapters = uncurry (-) <$> pairs adapters
  where
    pairs :: [a] -> [(a, a)]
    pairs (x:[]) = []
    pairs (x:x':xs) = (x',x) : pairs (x':xs)


numArrangements :: [Int] -> Int
numArrangements adapters = arrMap V.! 0
  where
    arrMap = foldr count (V.replicate len 1) [0..len - 2]
    len = V.length adapters'
    adapters' = V.fromList adapters
    count n map' = V.unsafeUpd map' [(n, sum furtherOpts)]
      where
        furtherOpts = (\o -> map' V.! (n+o)) <$> options
        options = [1..numOptions adapters' n]


numOptions :: V.Vector Int -> Int -> Int
numOptions v i = length $ takeWhile within [1..]
  where
    len = V.length v
    x = v V.! i
    within n = i+n < len && y-x <= 3 where y = v V.! (i+n)
