module Y2022.Day01 (parts) where

import Parser

parts = ( (part1, Just "67633")
        , (part2, Just "199628")
        , parse (splitSome spaces elf)
        )


type Elf = [Int]


part1 :: [Elf] -> String
part1 elves = show . maximum $ sum <$> elves


part2 :: [Elf] -> String
part2 elves = show . sum $ foldr top3 [] totals
  where
    totals = sum <$> elves
    top3 x (a:b:c:_) | x > a     = [x,a,b,c]
                     | x > b     = [a,x,b,c]
                     | x > c     = [a,b,x,c]
                     | otherwise = [a,b,c]
    top3 x (a:ds) | x > a     = x:a:top3 x ds
                  | otherwise = a:top3 x ds
    top3 x [] = [x]


elf :: Parser Elf
elf = splitSome (char '\n') natural
