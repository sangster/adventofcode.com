module Y2021.Day01 (parts) where

import Parser

parts = ( (part1, Just "1624")
        , (part2, Just "1653")
        , parseDepths
        )

part1 :: [Depth] -> String
part1 = show . length . filter (uncurry (<)) . pairs


part2 :: [Depth] -> String
part2 = show . length . filter (uncurry (<)) . pairs . windowSums
  where
    -- | Return the sum of each set of 3 adjacent depths.
    windowSums :: Num a => [a] -> [a]
    windowSums (x:x':x'':xs) = sum [x, x', x''] : windowSums (x':x'':xs)
    windowSums _ = []


type Depth = Int

parseDepths :: String -> [Depth]
parseDepths = parse (splitSome (char '\n') natural)

-- | Return adjacent pairs from the given list.
pairs :: [a] -> [(a, a)]
pairs (x:x':xs) = (x,x') : pairs (x':xs)
pairs _ = []
