module Y2019.Day23 (parts) where

import           Control.Monad.Cont
import           Control.Monad.Extra
import           Data.Bool
import qualified Data.IntMap.Strict as M

import Util.InstructionSet
import Util.OpCode
import Util.Program


parts = ( (part1, Just "24954")
        , (part2, Just "17091")
        , id
        )


part1 :: String -> String
part1 input = show $ runST $ do
    net <- parseRAM input >>= boot 50
    runContT (callCC $ communicate net pure) (pure . snd . natMem)


part2 :: String -> String
part2 input = show $ runST $ do
    net <- parseRAM input >>= boot 50
    runContT (callCC $ findRepeat net) (pure . snd . natMem)
  where
    findRepeat net onRepeat = callCC (communicate net onRepeat)
                          >>= flip findRepeat onRepeat


type NetCont s a = ContT a (ST s) (Network s)
type Addr      = Int
type IsIdle    = Bool

data Network s = Network
  { nodes      :: M.IntMap (Process (ST s) IsIdle)
  , natHistory :: [Data]
  , natMem     :: (Data, Data)
  }


insertNode net addr node = net{ nodes = M.insert addr node $ nodes net }
netNode    net addr      = nodes net M.! addr

natAddr = 255


-- | Allow the nodes in the Network to communicate with one another, providing
-- continuations for NAT packets and repeated NAT packets.
communicate :: Network s
            -> (Network s -> NetCont s a)
            -> (Network s -> NetCont s a)
            -> NetCont s a
communicate net onRepeat onNat = runNode onNat net 0
                        >>= restartNetwork onRepeat
                        >>= \n -> communicate n onRepeat onNat


-- | Create n nodes, each with a copy of the given memory.
boot :: Int -> RAM (PrimState (ST s)) -> ST s (Network s)
boot n mem = do
    nodes' <- M.fromList <$> sequence ([0 .. n - 1] >>= pure . mkNic)
    return $ Network{ nodes = nodes', natHistory = [], natMem = (-1, -1) }
  where
    mkNic addr = do
      program <- memcpy mem >>= pure . Program networkSet
      pure (addr, (load program False){ stdin = [addr] })
    networkSet = mergeSets aoc19Set [netStore "STOR" 3]

-- | Run each node in the Network, in order, starting with the given Addr.
-- Each one will run until raising a signal. Packets sent to the NAT (addr 255)
-- will escape the continuation early.
runNode :: ((Network s) -> NetCont s a)
        -> Network s
        -> Addr
        -> NetCont s a
runNode onNat net addr = do
    (comm, node') <- lift $ runStateT execComm (net `netNode` addr)
    let net' = insertNode net addr node'

    case comm of
      []            -> rest net'
      [recip, x, y] -> do
        if recip == natAddr
          then do
            onNat $ net'{ natMem = (x, y) }
          else do
            recip' <- lift $ execStateT (appendStdin [x, y]) (netNode net' recip)
            rest $ insertNode net' recip recip'

  where
    next = succ addr
    rest n = bool (pure n) (runNode onNat n next) $ M.member next (nodes net)
    appendStdin io = stdin <$> get >>= setStdin . (++ io)

    execComm = do
      (comm, rest) <- execute >> (splitAt 3) . stdout <$> get
      if length comm == 3
        then modify (\p -> p{ stdout = rest }) >> pure comm
        else pure []


-- | If every Process in the Network is idle, restart the network by sending
-- node 0 the last NAT payload.
restartNetwork :: (Network s -> NetCont s a)
               -> Network s
               -> NetCont s a
restartNetwork onRepeat net =
    ifM (all' $ liftM2 (&&) isEmpty isIdle) padNatXY (pure net)
  where
    all' f   = allM (evalStateT f) (M.elems $ nodes net)
    isEmpty  = stdin <$> get >>= pure . null
    isIdle   = user <$> get
    padNatXY = bool pushXY (onRepeat net) $ elem (last xy) (natHistory net)
    pushXY   = execStateT (modify active) (net `netNode` 0)
           >>= pure . insertNode net{ natHistory = last xy : natHistory net } 0
    active p = p{ stdin = xy, user = False }
    xy       = [fst $ natMem net, snd $ natMem net]


netStore :: PrimMonad m => String -> OpCode -> Instruction m IsIdle
netStore n = mkInstruction 1 store' n
  where
    store' assocs = do
      dat  <- stdin <$> get
      cur  <- cursor

      if not $ null dat
        then store'' assocs dat  False (Run    $ cur + 2)
        else store'' assocs [-1] True  (Signal $ cur + 2)

    store'' assocs (i:io) user' act = do
      modeDest assocs 0 >>= flip write i
      modify $ \p -> p{ stdin = io, user = user', action = act }
