module Day09 (parts) where

import Util.InstructionSet
import Util.Program


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "3409270027")
        , (part2, Just "82760")
        ]


part1 input = show <$> (program input >>= flip executeUntilHalt' [1])
part2 input = show <$> (program input >>= flip executeUntilHalt' [2])



program :: String -> IO Program
program = (fmap $ Program instructions) . parseRAM
  where instructions :: InstructionSet
        instructions = [ halt    "HALT" 99
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
