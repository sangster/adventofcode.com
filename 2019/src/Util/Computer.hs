module Util.Computer
    ( Action (..)
    , InstructionSet (..)
    , Instruction (..)
    , UserInstructionSet (..)
    , UserInstruction (..)
    , UserProcess (..)
    , Process (..)
    , Program (..)
    , UserProgram (..)
    , UserRuntime (..)
    , Runtime (..)
    , load'
    , load
    , pop
    , fetch
    , write
    , modeDest
    , modeRead
    , relativeJump
    , cursor
    , userState
    , module Control.Monad.State
    ) where

import Control.Monad.State

import Util.OpCode
import Util.RAM


-- | A state transformer representing a Process currently being run.
type Runtime a = UserRuntime () a


type UserRuntime a b = StateT (UserProcess a) IO b

type Process = UserProcess ()

-- | A @Program@ and the state needed to execute it.
data UserProcess a = UserProcess
    { user   :: a              -- ^ User-defined state.
    , prog   :: UserProgram a  -- ^ Instruction set and RAM.
    , action :: Action         -- ^ What the computer should do next.
    , fifo   :: [Data]         -- ^ Input/Output
    , base   :: Data           -- ^ The base for relative modes.
    }


type Program = UserProgram ()

data UserProgram a = UserProgram { is  :: UserInstructionSet a
                                 , mem :: RAM
                                 }


type InstructionSet       = [Instruction]
type UserInstructionSet a = [UserInstruction a]


type Instruction = UserInstruction ()

data UserInstruction a = UserInstruction
    { name   :: String  -- ^ A useful name for debugging.
    , opcode :: OpCode  -- ^ Its code in memory.
    , argc   :: Index   -- ^ The number of arguments it uses.
    , call   :: [(Mode, Data)] -> UserRuntime a ()
    }


instance Show (UserInstruction a) where
   show i = name i ++ "-" ++ show (opcode i)



-- | The computer has various types of actions it can perform.
data Action = Halt          -- ^ Stop execution entirely.
            | Signal Index  -- ^ Stop execution and respond to a signal. Return at the given index.
            | Run Index     -- ^ Continue executing the program at the given index.
    deriving Show


-- | Create a new @Process@, ready to be executed.
load' :: UserProgram a
      -> a
      -> UserProcess a
load' p a = UserProcess { prog   = p
                        , action = Run 0
                        , fifo   = []
                        , base   = 0
                        , user  = a
                        }

load :: UserProgram ()
     -> Process
load p = load' p ()


-- | Empty the @fifo@ and return its contents.
pop :: UserRuntime a [Data]
pop = do { out <- fifo <$> get; modify (\p -> p{ fifo = [] }); return out }


-- | Fetch a datum from memory, defaulting to @0@ if that value hasn't been
-- written yet.
--
-- The amount of memory will be increased to insure the given index isn't out of
-- bounds.
fetch :: Index
      -> UserRuntime a Data
fetch src = do growIfSmall src
               ram <- mem . prog <$> get
               lift $ readData ram src


-- | TODO: Fetch user state.
userState :: UserRuntime a a
userState = user <$> get

-- | Write a datum to memory.
--
-- The amount of memory will be increased to insure the given index isn't out of
-- bounds.
write :: Index
      -> Data
      -> UserRuntime a ()
write dst dat = do growIfSmall dst
                   ram <- mem . prog <$> get
                   lift $ writeData ram dst dat


-- | Grow the amount of available memory to ensure the given index isn't out of
-- bounds.
growIfSmall :: Index
            -> UserRuntime a ()
growIfSmall i = do
    ram <- mem . prog <$> get
    if memlen ram > i
        then return ()
        else do cpy <- lift $ grow ram i
                modify $ \p -> p{ prog = (UserProgram (is . prog $ p) cpy) }


-- | Move execution of the program relative to its current @cursor@.
relativeJump :: Data
             -> UserRuntime a ()
relativeJump n = do cur <- cursor
                    modify $ \p -> p{ action = Run (cur + n) }


-- | The position in memory currently being executed.
cursor :: UserRuntime a Index
cursor = do act <- action <$> get
            return $ cursor' act
  where cursor' Halt       = 0
        cursor' (Run    i) = i
        cursor' (Signal i) = i


-- | Read data from memory using position or immediate mode.
modeRead :: [(Mode, Data)]
         -> Data
         -> UserRuntime a Data
modeRead assocs i = do eitherIndexOrData <- modeLoc assocs i
                       case eitherIndexOrData of
                           Left  dat -> return dat
                           Right idx -> fetch  idx


-- | Read data from memory, interpreting it as memory location.
--
-- It differs from @modeRead@ by not derefercing @Position@ modes. ie: @Position
-- 10@ will write to @10@, and not reading the current value of @10@ and using
-- that address.
modeDest assocs i = do eitherIndexOrData <- modeLoc assocs i
                       case eitherIndexOrData of
                           Left  a -> return a
                           Right a -> return a


-- | @Left@ for an @Immediate@ location or @Right@ for a @Position@ location.
modeLoc :: [(Mode, Data)]
         -> Data
         -> UserRuntime a (Either Data Data)
modeLoc assocs i = uncurry loc $ assocs !! i
  where loc :: Mode -> Data -> UserRuntime a (Either Data Data)
        loc Immediate a = return $ Left a
        loc Position  a = return $ Right a
        loc Relative  a = do { b <- base <$> get; return . Right $ b + a }
