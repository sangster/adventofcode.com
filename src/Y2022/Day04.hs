module Y2022.Day04 (parts) where

import Parser

parts = ( (part1, Just "532")
        , (part2, Just "854")
        , parse assignmentPairs
        )


part1 :: [Pair] -> String
part1 pairs = show . length
            $ filter (\(a, b) -> covers a b || covers b a) pairs
  where
    covers (a1, a2) (b1, b2) = a1 <= b1 && a2 >= b2


part2 :: [Pair] -> String
part2 pairs = show . length $ filter overlaps pairs
  where
    overlaps ((a1, a2), (b1, b2)) | a1 <= b1  = a2 >= b1
                                  | otherwise = b2 >= a1


type Pair = (Assignment, Assignment)
type Assignment = (Section, Section)
type Section = Int


assignmentPairs :: Parser [Pair]
assignmentPairs = splitSome (char '\n') pair
  where
    pair = (,) <$> assignment <*> (char ',' >> assignment)
    assignment = (,) <$> natural <*> (char '-' >> natural)
