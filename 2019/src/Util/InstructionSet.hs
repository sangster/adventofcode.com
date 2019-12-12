module Util.InstructionSet
    ( aoc19Set
    , halt
    , math
    , store
    , output
    , jump
    , cmp
    , newBase
    ) where


import Data.Bool  (bool)

import Util.Computer


-- The full Advent of Code 2019 instruction set.
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
  where halt' :: [(a, b)] -> Runtime ()
        halt' _ = modify $ \p -> p{ action = Halt }


math n op f = mkInstruction 3 math' n op
  where math' assocs = do x   <- modeRead assocs 0
                          y   <- modeRead assocs 1
                          dst <- modeDest assocs 2
                          write dst $ f x y
                          relativeJump 4


store = mkInstruction 1 store'
  where store' assocs = do (i:io) <- fifo <$> get
                           dst <- modeDest assocs 0
                           write dst i
                           modify $ \p -> p{ fifo = io }
                           relativeJump 2


output = mkInstruction 1 output'
  where output' assocs = do dat <- modeRead assocs 0
                            cur <- cursor
                            io  <- fifo <$> get
                            modify $ \p -> p{ action = Signal (cur + 2)
                                            , fifo   = io ++ [dat]
                                            }


jump n op f = mkInstruction 2 jump' n op
  where jump' assocs = do x <- modeRead assocs 0
                          y <- modeRead assocs 1
                          cur <- cursor
                          modify $ \p -> p{ action = Run $ bool (cur + 3) y (f x) }


cmp n op f = mkInstruction 3 cmp' n op
  where cmp' assocs = do x   <- modeRead assocs 0
                         y   <- modeRead assocs 1
                         dst <- modeDest assocs 2
                         write dst (bool 0 1 $ f x y)
                         relativeJump 4


newBase = mkInstruction 1 newBase'
  where newBase' assocs = do x   <- modeRead assocs 0
                             old <- base <$> get
                             modify $ \p -> p{ base = old + x }
                             relativeJump 2


mkInstruction a c n op = Instruction{ name = n
                                    , opcode = op
                                    , argc = a
                                    , call = c
                                    }