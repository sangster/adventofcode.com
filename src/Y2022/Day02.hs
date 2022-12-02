module Y2022.Day02 (parts) where

import Data.Bifunctor (second)
import Data.List (find)
import Data.Maybe (fromJust)
import Parser

parts = ( (part1, Just "14069")
        , (part2, Just "12411")
        , parse (splitSome spaces strategyGuide)
        )


part1 :: [Round] -> String
part1 guide = show . sum $ score <$> misunderstoodGuide
  where
    misunderstoodGuide = second confuseColumn <$> guide
    confuseColumn Loss = Rock
    confuseColumn Tie  = Paper
    confuseColumn Win  = Scissors


part2 :: [Round] -> String
part2 guide = show . sum $ score . followGuide <$> guide
  where
    followGuide (t, r) = (t, choice)
      where
        choice = fromJust $ find (\x -> play x t == r) [Rock, Paper, Scissors]


type Round = (Choice, Result)
data Choice = Rock | Paper | Scissors deriving (Eq, Show)
data Result = Loss | Tie | Win deriving (Eq, Show)


score :: (Choice, Choice) -> Int
score (a, b) = v + w
  where
    v = case b of Rock     -> 1
                  Paper    -> 2
                  Scissors -> 3
    w = case play b a of Loss -> 0
                         Tie  -> 3
                         Win  -> 6


play :: Choice -> Choice -> Result
play x y | x == y    = Tie
         | beats x y = Win
         | otherwise = Loss
  where
    beats Paper    Rock     = True
    beats Scissors Paper    = True
    beats Rock     Scissors = True
    beats _        _        = False


strategyGuide :: Parser Round
strategyGuide = (,) <$> theirs <*> (spaces >> result)
  where
    theirs = symbol Rock     (reserved "A")
         <|> symbol Paper    (reserved "B")
         <|> symbol Scissors (reserved "C")
    result = symbol Loss (reserved "X")
         <|> symbol Tie  (reserved "Y")
         <|> symbol Win  (reserved "Z")
