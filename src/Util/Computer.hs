module Util.Computer
    ( Action (..)
    , InstructionSet (..)
    , InstructionSet'
    , Instruction (..)
    , Instruction'
    , Process (..)
    , Process'
    , Program (..)
    , Program'
    , Runtime (..)
    , Runtime'
    , act
    , cursor
    , fetch
    , load
    , load'
    , modeDest
    , modeRead
    , setStdin
    , pop
    , push
    , relativeJump
    , userState
    , setUserState
    , write
    , cpyProc

    , PrimMonad
    , PrimState
    , module Control.Monad.State
    , module Control.Monad.ST
    ) where

import Control.Monad.State
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Default
import Data.Function

import Util.OpCode
import Util.RAM


-- | A state transformer representing a Process currently being run.
type Runtime  m a b = StateT (Process m a) m b
type Runtime' m   b = Runtime m () b


-- | A @Program@ and the state needed to execute it.
data PrimMonad m => Process m a
  = Process
    { user   :: a           -- ^ User-defined state.
    , prog   :: Program m a -- ^ Instruction set and RAM.
    , action :: Action      -- ^ What the computer should do next.
    , stdin  :: [Data]      -- ^ Input
    , stdout :: [Data]      -- ^ Output
    , base   :: Data        -- ^ The base for relative modes.
    }
type Process' m = Process m ()


data PrimMonad m => Program m a
  = Program
    { is  :: InstructionSet m a
    , mem :: RAM (PrimState m)
    }
type Program' m = Program m ()


type InstructionSet  m a = [Instruction m a]
type InstructionSet' m   = InstructionSet m ()

data PrimMonad m => Instruction m a
  = Instruction
    { name   :: String  -- ^ A useful name for debugging.
    , opcode :: OpCode  -- ^ Its code in memory.
    , argc   :: Index   -- ^ The number of arguments it uses.
    , call   :: [(Mode, Data)] -> Runtime m a ()
    }
type Instruction' m = Instruction m ()


instance PrimMonad m => Eq (Instruction m a) where
  (==) = (==) `on` opcode


-- | The computer has various types of actions it can perform.
data Action
  = Halt          -- ^ Stop execution entirely.
  | Signal Index  -- ^ Stop execution and respond to a signal. Return at the given index.
  | Run Index     -- ^ Continue executing the program at the given index.
  deriving Show


-- | Like @maybe@, but for the action of a @Process@.
-- If the current access is @Halt@ the first argument will be returned,
-- otherwise the @second@ will be called with the action's @Index@.
act :: PrimMonad m
    => a
    -> (Index -> a)
    -> Process m b
    -> a
act false true proc =
    case action proc of
        Halt     -> false
        Signal i -> true i
        Run    i -> true i


-- | Create a new @Process@, ready to be executed.
load :: PrimMonad m
     => Program m a
     -> a
     -> Process m a
load p a = Process
    { prog   = p
    , action = Run 0
    , stdin  = []
    , stdout = []
    , base   = 0
    , user  = a
    }


load' :: PrimMonad m
      => Default a
      => Program m a
      -> Process m a
load' = flip load def


-- | Add the given data to the head of @stdin@.
setStdin :: PrimMonad m => [Data] -> Runtime m a ()
setStdin dd = modify $ \p -> p{ stdin = dd }


-- | Add the given data to the head of @stdin@.
push :: PrimMonad m => Data -> Runtime m a ()
push d = do { dd <- stdin <$> get; setStdin (d:dd) }


-- | Empty the @stdout@ and return its contents.
pop :: PrimMonad m => Runtime m a [Data]
pop = do
    out <- stdout <$> get
    modify $ \p -> p{ stdout = [] }
    pure out


-- | Fetch a datum from memory, defaulting to @0@ if that value hasn't been
-- written yet.
--
-- The amount of memory will be increased to insure the given index isn't out of
-- bounds.
fetch :: PrimMonad m => Index -> Runtime m a Data
fetch src = growIfSmall src >> mem . prog <$> get >>= flip readData src


-- | TODO: Fetch user state.
userState :: (PrimMonad m, Monad m) => Runtime m a a
userState = user <$> get


-- | TODO: Set user state.
setUserState :: PrimMonad m => (a -> a) -> Runtime m a a
setUserState f = do
    state <- f <$> userState
    modify $ \p -> p{ user = state }
    pure state


-- | Write a datum to memory.
--
-- The amount of memory will be increased to insure the given index isn't out of
-- bounds.
write :: PrimMonad m
      => Index
      -> Data
      -> Runtime m a ()
write dst dat = growIfSmall dst
             >> mem . prog <$> get >>= \r -> writeData r dst dat


-- | Grow the amount of available memory to ensure the given index isn't out of
-- bounds.
growIfSmall :: PrimMonad m
            => Index
            -> Runtime m a ()
growIfSmall i = do
    ram <- mem . prog <$> get
    if memlen ram > i
        then pure ()
        else do cpy <- lift $ grow ram i
                modify $ \p -> p{ prog = (Program (is . prog $ p) cpy) }


-- | Move execution of the program relative to its current @cursor@.
relativeJump :: PrimMonad m => Data -> Runtime m a ()
relativeJump n = do
    cur <- cursor
    modify $ \p -> p{ action = Run (cur + n) }


-- | The position in memory currently being executed.
-- A negative number will be returned if the program is halted.
cursor :: PrimMonad m => Runtime m a Index
cursor = get >>= pure . act (-1) id


-- | Read data from memory using position or immediate mode.
modeRead :: PrimMonad m
         => [(Mode, Data)]
         -> Data
         -> Runtime m a Data
modeRead assocs i = do
    eitherIndexOrData <- modeLoc assocs i
    case eitherIndexOrData of
        Left  dat -> pure  dat
        Right idx -> fetch idx


-- | Read data from memory, interpreting it as memory location.
--
-- It differs from @modeRead@ by not derefercing @Position@ modes. ie: @Position
-- 10@ will write to @10@, and not reading the current value of @10@ and using
-- that address.
modeDest assocs i = do
    eitherIndexOrData <- modeLoc assocs i
    case eitherIndexOrData of
        Left  a -> pure a
        Right a -> pure a


-- | @Left@ for an @Immediate@ location or @Right@ for a @Position@ location.
modeLoc :: (PrimMonad m, Functor m)
        => [(Mode, Data)]
        -> Data
        -> Runtime m a (Either Data Data)
modeLoc assocs i = uncurry loc $ assocs !! i
  where
    loc :: (PrimMonad m, Functor m) => Mode -> Data -> Runtime m a (Either Data Data)
    loc Immediate a = pure $ Left a
    loc Position  a = pure $ Right a
    loc Relative  a = do { b <- base <$> get; pure . Right $ b + a }


-- | Make a copy of the given Process.
cpyProc :: PrimMonad m => Process m a -> m (Process m a)
cpyProc p = memcpy (mem $ prog p) >>= \m -> pure p{ prog = (prog p){ mem = m } }
