module Y2022.Day10 (parts) where

import Data.Bool (bool)
import Parser
import Data.List.Split (chunksOf)

parts = ( (part1, Just "13480")
        , (part2, Just $ unlines [ "████  ██    ██ ███   ██   ██  ████ █  █ "
                                 , "█    █  █    █ █  █ █  █ █  █ █    █ █  "
                                 , "███  █       █ ███  █    █    ███  ██   "
                                 , "█    █ ██    █ █  █ █ ██ █    █    █ █  "
                                 , "█    █  █ █  █ █  █ █  █ █  █ █    █ █  "
                                 , "████  ███  ██  ███   ███  ██  █    █  █ "
                                 ])
        , parse $ splitSome (char '\n') instruction
        )
  where
    instruction :: Parser Inst
    instruction = symbol Noop (string "noop")
              <|> Addx <$> (reserved "addx" >> number)


part1 :: [Inst] -> String
part1 = show . sum . scanStrengths
  where
    scanStrengths :: [Inst] -> [Strength]
    scanStrengths insts = scan' 1 (cycles $ head insts) 1 insts
      where
        scan' :: Cycle -> Cycle -> Value -> [Inst] -> [Strength]
        scan' _      _ _   []     = []
        scan' cycle' c acc (i:is) | sample    = strength cycle' acc : rest
                                  | otherwise = rest
          where
            sample = (cycle' - 20) `mod` 40 == 0
            rest = scan' (succ cycle') c' acc' is'
            (c', acc', is') = if c == 1
                              then (cycles i, calc i acc,   is)
                              else (pred c  , acc       , i:is)


part2 :: [Inst] -> String
part2 = unlines . chunksOf w . take (w * h) . draw w
  where
    (w,h) = (40, 6)

    draw :: Int -> [Inst] -> String
    draw width insts = draw' 1 (cycles $ head insts) 1 insts
      where
        draw' :: Cycle -> Cycle -> Value -> [Inst] -> String
        draw' _      _ _   []     = []
        draw' cycle' c acc (i:is) = ch : rest
          where
            ch = bool ' ' '█' shouldDraw
            shouldDraw = abs (hPos - acc) < 2
            hPos = (cycle' - 1) `mod` width

            rest = draw' (succ cycle') c' acc' is'
            (c', acc', is') = if c == 1
                              then (cycles i, calc i acc,   is)
                              else (pred c  , acc       , i:is)


data Inst = Addx Value | Noop deriving Show
type Value = Int
type Cycle = Int
type Strength = Int


cycles :: Inst -> Int
cycles (Addx _) = 2
cycles _        = 1


calc :: Inst -> Int -> Int
calc (Addx n) = (n +)
calc _        = id


strength :: Cycle -> Value -> Strength
strength = (*)
