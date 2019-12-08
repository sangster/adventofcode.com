module Day05 (parts) where

import Util.Computer


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "7259358")
        , (part2, Just "11826654")
        ]


part1 input = do { mem <- parseRAM input; show <$> runUntilHalt' mem [1] 0 }
part2 input = do { mem <- parseRAM input; show <$> runUntilHalt' mem [5] 0 }
