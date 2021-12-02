module Y2021.Day02 (parts) where

import Parser

parts = ( (part1, Just "1815044")
        , (part2, Just "1739283308")
        , parseInstructions
        )

part1 :: [Instruction] -> String
part1 = show . (uncurry (*)) . followInstructions move (0, 0, 0)
  where
    move :: Position -> Instruction -> Position
    move (h, d, a) (Forward, x) = (h + x, d    , a)
    move (h, d, a) (Down,    x) = (h    , d + x, a)
    move (h, d, a) (Up,      x) = (h    , d - x, a)

part2 :: [Instruction] -> String
part2 = show . (uncurry (*)) . followInstructions move (0, 0, 0)
  where
    move :: Position -> Instruction -> Position
    move (h, d, a) (Forward, x) = (h + x, d + a * x, a)
    move (h, d, a) (Down,    x) = (h    , d        , a + x)
    move (h, d, a) (Up,      x) = (h    , d        , a - x)


type Position = (HorizontalPosition, Depth, Aim)
type HorizontalPosition = Int
type Depth = Int
type Aim = Int
type Instruction = (Dir, Int)
data Dir = Forward | Down | Up deriving Show

parseInstructions :: String -> [Instruction]
parseInstructions = parse (splitSome (char '\n') instruction)
  where
    instruction = do d <- dir
                     _ <- char ' '
                     dist <- natural
                     pure (d, dist)
    dir = symbol Forward (string "forward")
      <|> symbol Down    (string "down")
      <|> symbol Up      (string "up")

-- | Return the final HorizontalPosition and Depth, after following a series of
--   movements.
followInstructions :: (Position -> Instruction -> Position)
                   -> Position
                   -> [Instruction]
                   -> (HorizontalPosition, Depth)
followInstructions = ((aimless .) .) . foldl where aimless (h, d, _) = (h, d)
