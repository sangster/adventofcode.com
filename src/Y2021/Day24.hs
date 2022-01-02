module Y2021.Day24 (parts) where

import Data.List       (sort)
import Data.List.Split (chunksOf)
import Parser

parts = ( (part1, Just "93499629698999")
        , (part2, Just "11164118121471")
        , parse $ splitSome (char '\n') instruction
        )


part1 :: [Instr] -> String
part1 = show . findSerial maxSerial
  where
    maxSerial (Equality (Offset i1 o1) (Offset i2 o2)) =
        if off > 0
        then [(i1, 9-off), (i2, 9     )]
        else [(i1, 9    ), (i2, 9+off)]
      where
        off = o1 + o2

part2 :: [Instr] -> String
part2 = show . findSerial minSerial
  where
    minSerial (Equality (Offset i1 o1) (Offset i2 o2)) =
        if off > 0
        then [(i1, 1    ), (i2, 1+off)]
        else [(i1, 1-off), (i2, 1    )]
      where
        off = o1 + o2


-- Types for parsing the program.
data Var = W | X | Y | Z deriving Show
data Rvalue = V (Var) | I (Int) deriving Show
data Instr = Inp Var
           | Add Var Rvalue
           | Mul Var Rvalue
           | Div Var Rvalue
           | Mod Var Rvalue
           | Eql Var Rvalue
           deriving Show

newtype SerialNum = SerialNum [Int]
type Idx = Int
data Offset = Offset Idx Int
data Equality = Equality Offset Offset

instance Show SerialNum where
  show (SerialNum []) = ""
  show (SerialNum (s:sn)) = show s ++ show (SerialNum sn)


-- | Calculate the correct SerialNum for the given program. The passed function
--   is used to solve pairs of digits that are related by an equality, but can
--   have multiple correct values.
findSerial :: (Equality -> [(Idx, Int)]) -> [Instr] -> SerialNum
findSerial fitFunc program = SerialNum . fmap snd . sort
                           $ fitFunc `concatMap` equalities 0 [] c1 c2 c3
  where
    c1 = extractConstants program  4 -- Always 1 or 26.
    c2 = extractConstants program  5 -- Loop-down offset.
    c3 = extractConstants program 15 -- Loop-up offset.

    -- | Each segment of the program "loops" around the number 26. When n1 == 1,
    --   it loops-up, with an offset; when n1 == 26, it loops-down, with an
    --   offset. Finally, because Z both starts and ends at 0, each equation
    --   must be balanced.
    equalities :: Idx -> [Offset] -> [Int] -> [Int] -> [Int] -> [Equality]
    equalities idx offsets (n1:c1') (n2:c2') (n3:c3') =
        if n1 == 1
        then       rest (off:offsets)
        else eql : rest (tail offsets)
      where
        off = Offset idx n3
        eql = Equality (head offsets) (Offset idx n2)
        rest offsets' = equalities (succ idx) offsets' c1' c2' c3'

    equalities _ _ _ _ _ = []


-- | Extract the Rvalue from the Nth instruction. It must be a constant.
extractConstants :: [Instr] -> Int -> [Int]
extractConstants is n = extractC . (!! n) <$> chunksOf chunkSize is
  where
    chunkSize = length is `div` 14

    extractC (Add _ (I x)) = x
    extractC (Mul _ (I x)) = x
    extractC (Div _ (I x)) = x
    extractC (Mod _ (I x)) = x
    extractC (Eql _ (I x)) = x
    extractC x = error $ "extractC: unexpected "++show x


instruction :: Parser Instr
instruction = inp           <|> bin Add "add" <|> bin Mul "mul"
          <|> bin Div "div" <|> bin Mod "mod" <|> bin Eql "eql"
  where
    inp = liftM Inp $ string "inp " >> var
    bin sym name = do a <- string (name++" ") >> var
                      b <- char ' ' >> rvalue
                      pure $ sym a b

    var = symbol W (char 'w') <|> symbol X (char 'x')
      <|> symbol Y (char 'y') <|> symbol Z (char 'z')

    rvalue = liftM V var <|> liftM I number
