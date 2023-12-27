module Y2023.Day02 (parts) where

import Data.List
import Data.Maybe
import Parser

parts = ( (part1, Just "2685")
        , (part2, Just "83707")
        , parse $ splitSome (char '\n') parseGame
        )

part1 :: [Game] -> String
part1 = show . sum . map gameId . filter (isPossible (12, 13, 14))
 where
  gameId (Game i _) = i
  isPossible (r, g, b) (Game _ xs) = all isEnough xs
    where
      isEnough (r', g', b') = r' <= r && g' <= g && b' <= b


part2 :: [Game] -> String
part2 input = show . sum $ power . minBalls <$> input
  where
    minBalls (Game _ subsets) = foldr max' (0, 0, 0) subsets
    max' (r, g, b) (r', g', b') = (max r r', max g g', max b b')
    power (r, g, b) = r * g * b


data Game = Game Int [Subset] deriving Show

type Subset = (Red, Green, Blue)
type Red   = Int
type Green = Int
type Blue  = Int


parseGame :: Parser Game
parseGame = do id' <- string "Game " >> natural
               sets <- string ": " >> splitSome (string "; ") choices
               pure $ Game id' (normalize <$> sets)
  where
    choices = splitSome (string ", ") balls
    balls = color "red" <|> color "green" <|> color "blue"
    color str = do n <- natural
                   char ' ' >> string str >> pure (str, n)
    normalize i = (get' i "red", get' i "green", get' i "blue")
    get' xs str = fromMaybe 0 $ lookup str xs
