module Y2019.Day09 (parts) where

import Util.InstructionSet
import Util.Program


parts = ( (part1, Just "3409270027")
        , (part2, Just "82760")
        , id
        )


part1 :: String -> String
part1 input = show $ runST (program input >>= flip executeUntilHalt' [1])


part2 :: String -> String
part2 input = show $ runST (program input >>= flip executeUntilHalt' [2])


program :: PrimMonad m => String -> m (Program' m)
program = (fmap $ Program instructions) . parseRAM
  where instructions =
          [ halt    "HALT" 99
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
