module Util.OpCode
    ( OpCode
    , Mode (..)
    , splitCode
    , getMode
    ) where

import Safe      (atDef)
import Util.RAM  (Data)


-- | A numeric code in memory which names an operation the computer can perform.
type OpCode = Data


-- | Memory can either be references to other memory, or data.
data Mode = Position   -- ^ Return the data at the memory location of the value.
          | Immediate  -- ^ Return the value verbatim.
          | Relative   -- ^ Return the data at the memorry location relative to value.
    deriving Show



-- | Split a number into its Modes and OpCode components.
splitCode :: Int -> ([Mode], Int)
splitCode n = (modes $ n `div` 100, n `mod` 100)
  where modes x | x >= 10   = modes (x `mod` 10) ++ modes (x `div` 10)
                | x == 1    = [Immediate]
                | x == 2    = [Relative]
                | otherwise = [Position]


-- | Get the mode at the given index, defaulting to @Position@.
getMode :: [Mode] -> Int -> Mode
getMode modes i = atDef Position modes i
