module Day05 (parts) where

import Util.Program


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "7259358")
        , (part2, Just "11826654")
        ]


part1 input = do { prog <- aoc19Program' input; show <$> runUntilHalt' prog [1] 0 }
part2 input = do { prog <- aoc19Program' input; show <$> runUntilHalt' prog [5] 0 }
