{-# LANGUAGE ImportQualifiedPost, TupleSections #-}
module Y2023.Day04 (parts) where

import Data.IntMap.Strict qualified as M
import Data.List (sort)
import Parser

parts = ( (part1, Just "26914")
        , (part2, Just "13080971")
        , parse $ splitSome (char '\n') parseCard
        )


part1 :: [Card] -> String
part1 = show . sum . map score
  where
    score c = case countWins c of
                0 -> 0
                n -> 2 ^ pred n


part2 :: [Card] -> String
part2 cards = show . runQueue $ (, 1) <$> [1 .. length cards]
  where
    runQueue [] = 0
    runQueue ((id', dupes):qs) = dupes + runQueue qs'
      where
        wins = M.findWithDefault 0 id' cWins
        qs' = addCards dupes qs wins
    cWins = M.fromList $ zip [1 ..] (countWins <$> cards)
    addCards _ qs 0 = qs
    addCards dupes ((id', n):qs) w = (id', n+dupes) : addCards dupes qs (pred w)


countWins :: Card -> Int
countWins c = count (cWinning c) (cDraw c)
  where
    count [] _ = 0
    count _ [] = 0
    count (w:ws) (d:ds) = case compare w d of
                            LT -> count ws (d:ds)
                            GT -> count (w:ws) ds
                            EQ -> succ $ count ws ds


data Card = Card { cId      :: Int
                 , cWinning :: [Int]
                 , cDraw    :: [Int]
                 } deriving Show

type CardMap  = M.IntMap Card
type CardWins = M.IntMap Int

parseCard :: Parser Card
parseCard = do id'   <- string "Card" >> spaces >> natural
               wins  <- sort <$> (string ": "  >> some (spaces >> natural))
               draws <- sort <$> (string " | " >> some (spaces >> natural))
               pure $ Card id' wins draws
