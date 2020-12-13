module Y2020.Day08 (parts) where

import qualified Data.HashSet as S
import           Data.List (find)
import           Data.Maybe (fromJust)
import qualified Data.Vector as V
import           Parser


parts = ( (part1, Just "1451")
        , (part2, Just "1160")
        , V.fromList . parse (some instruction)
        )


part1 instructions = show $ accumAtRepeat instructions
  where
    accumAtRepeat = snd . execute


part2 instructions = show $ accumAtEnd $ fromJust fixedInstructions
  where
    fixedInstructions = find reachesEnd $ hackedInstructionSets instructions
    accumAtEnd = snd . execute
    reachesEnd = fst . execute


data Opcode = Acc | Jmp | Nop deriving Show
type Instruction = (Opcode, Int)


instruction :: Parser Instruction
instruction = do spaces
                 op <- opcode
                 n  <- number
                 pure (op, n)


opcode :: Parser Opcode
opcode = (symbol Acc $ reserved "acc")
     <|> (symbol Jmp $ reserved "jmp")
     <|> (symbol Nop $ reserved "nop")


-- | fst :: Completed without repeating? snd :: Accumulator at end
execute :: V.Vector Instruction -> (Bool, Int)
execute instructions = process S.empty 0 0
  where
    endCursor = V.length instructions
    process seen accum cursor
      | cursor == endCursor = (True, accum)
      | S.member cursor seen = (False, accum)
      | otherwise = case instructions V.! cursor of
                      (Acc, n) -> process seen' (accum + n) (cursor + 1)
                      (Jmp, n) -> process seen' accum       (cursor + n)
                      (Nop, _) -> process seen' accum       (cursor + 1)
      where
        seen' = S.insert cursor seen


hackedInstructionSets :: V.Vector Instruction -> [V.Vector Instruction]
hackedInstructionSets orig = sets 0
  where
    sets i
      | i == V.length orig = []
      | otherwise = case orig V.! i of
                      (Acc, _) -> sets (i+1)
                      (Jmp, n) -> V.unsafeUpd orig [(i, (Nop, n))] : sets (i+1)
                      (Nop, n) -> V.unsafeUpd orig [(i, (Jmp, n))] : sets (i+1)
