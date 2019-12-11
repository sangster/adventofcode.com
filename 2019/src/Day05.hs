module Day05 (parts) where

import Util.InstructionSet
import Util.Program


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "7259358")
        , (part2, Just "11826654")
        ]


part1 input = show <$> (program input >>= flip executeUntilHalt' [1])
part2 input = show <$> (program input >>= flip executeUntilHalt' [5])


program :: String -> IO Program
program = (fmap $ Program instructions) . parseRAM
  where instructions :: InstructionSet
        instructions = [ halt   "HALT" 99
                       , math   " ADD"  1 (+)
                       , math   "MULT"  2 (*)
                       , store  "STOR"  3
                       , output " OUT"  4
                       , jump   " JEQ"  5 (/= 0)
                       , jump   "JNEQ"  6 (== 0)
                       , cmp    "  LT"  7 (<)
                       , cmp    "  EQ"  8 (==)
                       ]
