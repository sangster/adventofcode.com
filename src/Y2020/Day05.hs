module Y2020.Day05 (parts) where

import Data.Bool (bool)
import Data.List (maximum, sort)
import Parser


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "874")
        , (part2, Just "594")
        ]


part1 input = show . seatId $ maximum seats
  where
    seats = codeToSeat <$> parse (some seatCode) input


part2 input = show $ seatId emptySeat
  where
    seats = codeToSeat <$> parse (some seatCode) input
    emptySeat = idToSeat $ missingId ids
      where
        ids = seatId <$> sort seats
        missingId (x:x':xs) | x' == x+1 = missingId (x':xs)
                            | otherwise = x+1


planeLength = 8
planeWidth = 4


data SeatCode = SeatCode [Bool] [Bool] deriving Show
data Seat = Seat Int Int deriving Eq

instance Ord Seat where
    a <= b = seatId a <= seatId b


seatCode :: Parser SeatCode
seatCode = do spaces
              row <- replicateM (planeLength - 1) direction
              col <- replicateM (planeWidth - 1) direction
              pure $ SeatCode row col


direction :: Parser Bool
direction = (symbol False $ char 'F')
        <|> (symbol True  $ char 'B')
        <|> (symbol False $ char 'L')
        <|> (symbol True  $ char 'R')


codeToSeat :: SeatCode -> Seat
codeToSeat (SeatCode rs cs) = Seat (toInt rs) (toInt cs)
  where
    toInt xs = foldr toInt' 0 entries
      where
        entries = zip (reverse xs) [0..]
        toInt' (b, i) sum = sum + bool 0 (2^i) b


seatId :: Seat -> Int
seatId (Seat r c) = r * planeLength + c


idToSeat :: Int -> Seat
idToSeat id = uncurry Seat $ divMod id planeLength
