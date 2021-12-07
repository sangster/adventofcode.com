module Y2021.Day07 (parts) where

import Data.List (sort)
import Parser


parts = ( (part1, Just "340056")
        , (part2, Just "96592275")
        , parse $ splitSome (char ',') natural :: String -> [Position]
        )


part1 :: [Position] -> String
part1 ps = show . sum $ gasCost (median ps) <$> ps
  where
    gasCost x y = abs $ x - y

    median :: [Position] -> Position
    median ps | odd  n = b
              | even n = floor $ fromIntegral (a + b) / 2
      where
        n = length ps
        mid = (n `div` 2) - 1
        [a, b] = take 2 . drop mid $ sort ps


part2 :: [Position] -> String
part2 ps = show . sum $ gasCost (mean ps) <$> ps
  where
    gasCost x y = d * (d+1) `div` 2 where d = abs $ x - y
    mean ps = floor $ fromIntegral (sum ps) / fromIntegral (length ps)


type Position = Int
