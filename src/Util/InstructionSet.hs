module Util.InstructionSet
    ( aoc19Set
    , halt
    , math
    , store
    , output
    , jump
    , cmp
    , newBase
    , mkInstruction
    ) where


import Data.Bool  (bool)

import Util.Computer
import Util.OpCode
import Util.RAM


-- The full Advent of Code 2019 instruction set.
aoc19Set :: InstructionSet a
aoc19Set = [ halt    "HALT" 99
           , math    " ADD"  1 (+)
           , math    "MULT"  2 (*)
           , store   "STOR"  3
           , output  " OUT"  4
           , jump    " JEQ"  5 (/= 0)
           , jump    "JNEQ"  6 (== 0)
           , cmp     "  LT"  7 (<)
           , cmp     "  EQ"  8 (==)
           , newBase "BASE"  9
           ]


halt = mkInstruction 0 halt'
  where
    halt' :: [(Mode, Data)] -> Runtime a ()
    halt' _ = modify $ \p -> p{ action = Halt }


math n op f = mkInstruction 3 math' n op
  where
    math' assocs = do
      x   <- modeRead assocs 0
      y   <- modeRead assocs 1
      dst <- modeDest assocs 2
      write dst $ f x y
      relativeJump 4


store n = mkInstruction 1 store' n
  where
    store' assocs = do
      dat <- stdin <$> get
      case dat of
        (i:io) -> do dst <- modeDest assocs 0
                     write dst i
                     modify $ \p -> p{ stdin = io }
                     relativeJump 2
        _      -> error $ n ++ ": stdin empty"


output = mkInstruction 1 output'
  where
    output' assocs = do
      dat <- modeRead assocs 0
      cur <- cursor
      out <- stdout <$> get
      modify $ \p -> p{ action = Signal (cur + 2)
                      , stdout = out ++ [dat]
                      }


jump n op f = mkInstruction 2 jump' n op
  where
    jump' assocs = do
      x   <- modeRead assocs 0
      y   <- modeRead assocs 1
      cur <- cursor
      modify $ \p -> p{ action = Run $ bool (cur + 3) y (f x) }


cmp n op f = mkInstruction 3 cmp' n op
  where
    cmp' assocs = do
      x   <- modeRead assocs 0
      y   <- modeRead assocs 1
      dst <- modeDest assocs 2
      write dst (bool 0 1 $ f x y)
      relativeJump 4


newBase = mkInstruction 1 newBase'
  where
    newBase' assocs = do
      x   <- modeRead assocs 0
      old <- base <$> get
      modify $ \p -> p{ base = old + x }
      relativeJump 2


mkInstruction :: Index
              -> ([(Mode, Data)] -> Runtime a ())
              -> String
              -> OpCode
              -> Instruction a
mkInstruction a c n op = Instruction{ name = n
                                    , opcode = op
                                    , argc = a
                                    , call = c
                                    }
