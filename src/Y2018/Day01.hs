module Y2018.Day01 (parts) where

import qualified Data.IntSet as IntSet


parts = ( (part1, Nothing)
        , (part2, Nothing)
        , map parseSigned . lines
        )


{- converts a textual number like -123, +123 into an Int -}
parseSigned :: [Char] -> Int
parseSigned ('+':str) =     read str
parseSigned ('-':str) = 0 - read str
parseSigned str       = error $ "'"++str++"' is not a signed integer"


part1 :: FreqQueue -> String
part1 numbers = show $ sum numbers


type FreqQueue = [Int]
data FreqCounter = FreqCounter !IntSet.IntSet FreqQueue !Int


findRepeat :: FreqQueue -> Int
findRepeat numbers =
    findRepeat' (FreqCounter (IntSet.singleton 0) (cycle numbers) 0)
  where
    findRepeat' (FreqCounter history (x:xs) freq)
        | IntSet.member freq' history = freq'
        | otherwise = findRepeat' (FreqCounter history' xs freq')
      where freq'    = freq + x
            history' = IntSet.insert freq' history


part2 :: FreqQueue -> String
part2 numbers = show $ findRepeat numbers
