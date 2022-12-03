module Y2022.Day03 (parts) where

import Data.Char (ord)
import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

parts = ( (part1, Just "8515")
        , (part2, Just "2434")
        , lines
        )


part1 :: [RuckSack] -> String
part1 sacks = show . sum $ priority . common . splitSack <$> sacks
  where
    common (a,b) = fromJust $ find (`elem` b) a
    splitSack s = splitAt (length s `div` 2) s


part2 :: [RuckSack] -> String
part2 sacks = show . sum $ priority . findBadge <$> chunksOf 3 sacks
  where
    findBadge :: [RuckSack] -> Item
    findBadge [a,b,c] = fromJust $ find (`elem` c) $ filter (`elem` b) a
    findBadge _ = error "wrong size"


type RuckSack = [Item]
type Item = Char
type Priority = Int


priority :: Item -> Priority
priority item | item > 'a' =  1 + ord item - ord 'a'
              | otherwise  = 27 + ord item - ord 'A'
