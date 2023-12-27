module Y2023.Day01 (parts) where

import Data.Char (isAlphaNum)
import Data.Maybe (catMaybes)
import Parser

parts = ( (part1, Just "54561")
        , (part2, Just "54076")
        , parse (splitSome (char '\n') parseLine)
        )


part1 :: [[Unit]] -> String
part1 = show . sum . map (extract . digits)
  where
    digits i = [n | (Digit n) <- i]


part2 :: [[Unit]] -> String
part2 = show . sum . map (extract . map toNum)
  where
    toNum (Digit i) = i
    toNum (Word i) = i


data Unit = Digit Int
          | Word  Int
          deriving Show


extract :: [Int] -> Int
extract l = head l * 10 + last l


parseLine :: Parser [Unit]
parseLine = catMaybes <$> some (word' <|> nat <|> noop)
  where
    word' = wordNum 1 "one"
        <|> wordNum 2 "two"
        <|> wordNum 3 "three"
        <|> wordNum 4 "four"
        <|> wordNum 5 "five"
        <|> wordNum 6 "six"
        <|> wordNum 7 "seven"
        <|> wordNum 8 "eight"
        <|> wordNum 9 "nine"
    wordNum n str = string str >> pure (Just (Word n))
    nat = (Just . Digit) <$> digit'
    noop = satisfy isAlphaNum >> pure Nothing
