{-# LANGUAGE QuasiQuotes                      #-}

module Day05 (parts, part1, part2) where

parts = [ (part1, Just "9704")
        , (part2, Just "6942")
        ]


part1 :: String
      -> String
part1 content = foldl process [head content] $ tail content
  where
    process a (b:bs)

part2 :: String
      -> String
part2 content = ""
