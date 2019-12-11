module Util.Computer
    ( Action (..)
    , InstructionSet
    , Instruction (..)
    , Process (..)
    , Program (..)
    , Runtime
    , load
    , fetch
    , write
    , modeLoc
    , modeDest
    , modeRead
    , relativeJump
    , cursor
    , module Control.Monad.State
    ) where

import Control.Monad.State

import Util.OpCode
import Util.RAM


type Runtime a = StateT Process IO a


data Process = Process { prog   :: Program
                       , action :: Action
                       , fifo   :: [Data]
                       , base   :: Data
                       }


data Program = Program { is  :: InstructionSet
                       , mem :: RAM
                       }


type InstructionSet = [Instruction]


data Instruction = Instruction
    { name   :: String
    , opcode :: OpCode           -- Its code in memory.
    , argc   :: Index            -- The number of arguments it uses.
    , call   :: [(Mode, Data)] -> Runtime ()
    }


instance Show Instruction where
   show i = name i ++ "-" ++ show (opcode i)



-- | The computer has various types of actions it can perform.
data Action = Halt          -- ^ Stop execution entirely.
            | Signal Index  -- ^ Stop execution and respond to a signal. Return at the given index.
            | Run Index     -- ^ Continue executing the program at the given index.
    deriving Show


load :: Program
     -> Process
load p = Process { prog   = p
                 , action = Run 0
                 , fifo   = []
                 , base   = 0
                 }


fetch :: Index
      -> Runtime Data
fetch src = do growIfSmall src
               ram <- mem . prog <$> get
               lift $ readData ram src


write :: Index
      -> Data
      -> Runtime ()
write dst dat = do growIfSmall dst
                   ram <- mem . prog <$> get
                   lift $ writeData ram dst dat


growIfSmall :: Index
            -> Runtime ()
growIfSmall i = do
    ram <- mem . prog <$> get
    if memlen ram > i
        then return ()
        else do cpy <- lift $ grow ram i
                modify $ \p -> p{ prog = (Program (is . prog $ p) cpy) }



relativeJump :: Data
             -> Runtime ()
relativeJump n = do cur <- cursor
                    modify $ \p -> p{ action = Run (cur + n) }


cursor :: Runtime Index
cursor = do act <- action <$> get
            return $ cursor' act
  where cursor' Halt       = 0
        cursor' (Run    i) = i
        cursor' (Signal i) = i


-- | Read data from memory using position or immediate mode.
modeRead :: [(Mode, Data)]
         -> Data
         -> Runtime Data
modeRead assocs i = do eitherIndexOrData <- modeLoc assocs i
                       case eitherIndexOrData of
                           Left  dat -> return dat
                           Right idx -> fetch  idx


modeDest assocs i = do eitherIndexOrData <- modeLoc assocs i
                       case eitherIndexOrData of
                           Left  a -> return a
                           Right a -> return a


modeLoc :: [(Mode, Data)]
         -> Data
         -> Runtime (Either Data Data)
modeLoc assocs i = do ram <- mem . prog <$> get
                      uncurry modeRead' $ assocs !! i
  where modeRead' :: Mode -> Data -> Runtime (Either Data Data)
        modeRead' Immediate a = return $ Left a
        modeRead' Position  a = return $ Right a
        modeRead' Relative  a = do b <- base <$> get
                                   return . Right $ b + a
