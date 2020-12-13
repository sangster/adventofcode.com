module Y2020.Day01 (parts) where

import           Data.List (sort)
import qualified Data.Vector.Unboxed as U


parts = ( (part1, Just "788739")
        , (part2, Just "178724430")
        , readEntries
        )


part1 :: Entries -> String
part1 entries = show $ x * y
  where
    Just (x, y) = findTwoParts 2020 entries


part2 :: Entries -> String
part2 entries = show $ x * y * z
  where
    Just (x, y, z) = findThreeParts 2020 entries


type Entries = U.Vector Int


readEntries :: String -> Entries
readEntries = U.fromList . sort . fmap read . lines


findTwoParts :: Int -> Entries -> Maybe (Int, Int)
findTwoParts goal entries = search (U.head entries) (U.tail entries)
  where
    search x xs | U.null xs = Nothing
                | binSearch (goal - x) xs = Just (x, goal - x)
                | otherwise = search (U.head xs) (U.tail xs)


findThreeParts :: Int -> Entries -> Maybe (Int, Int, Int)
findThreeParts goal entries = search (U.head entries) (U.tail entries)
  where
    search x xs | U.null xs = Nothing
                | otherwise = case findTwoParts (goal - x) xs of
                                Just (y, z) -> Just (x, y, z)
                                Nothing     -> findThreeParts goal xs


binSearch :: Int -> Entries -> Bool
binSearch e es | U.null es = False
               | e < x = binSearch e left
               | e > x = binSearch e right
               | otherwise = True
  where
    len = div (U.length es) 2
    x = es U.! len
    left = U.take len es
    right = U.drop (len+1) es
