module Util.Computer
    ( Mode(..)
    , Action(..)
    , modeRead
    ) where


import Util.RAM


-- | Memory can either be references to other memory, or data.
data Mode = Position   -- ^ Return the data at the memory location of the value.
          | Immediate  -- ^ Return the value verbatim.
    deriving Show


-- | After every instruction, the computer needs to know what to do next.
data Action = Halt          -- ^ Stop execution entirely.
            | Signal Index  -- ^ Stop execution and respond to a signal. Return at the given index.
            | Jump Index    -- ^ Move to the given index and continue execution.
    deriving Show


-- | Read data from memory using position or immediate mode.
modeRead :: RAM
         -> [(Mode, Data)]
         -> Int
         -> IO Data
modeRead mem assocs i = uncurry modeRead' $ assocs !! i
  where modeRead' Immediate a = return a
        modeRead' Position  a = readData mem a
