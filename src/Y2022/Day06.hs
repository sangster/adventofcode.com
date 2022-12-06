module Y2022.Day06 (parts) where

import Data.Bool (bool)
import Data.List (nub)

parts = ( (part1, Just "1210")
        , (part2, Just "3476")
        , id
        )

part1 :: String -> String
part1 = show . markerEndIndex 4

part2 :: String -> String
part2 = show . markerEndIndex 14


-- | Return the index after the end of the first marker (of length @len@).
markerEndIndex :: Int -> String -> Int
markerEndIndex len str = findEnd len (reverse seen) queue
  where
    (seen, queue) = splitAt (len-1) str

    findEnd :: Int -> String -> String -> Int
    findEnd i acc (c:cs) = bool rest i $ acc' == nub acc'
      where
        rest = findEnd (i + 1) acc' cs
        acc' = c:take (len - 1) acc
    findEnd _ _ [] = error "no markers found"
