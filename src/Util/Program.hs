module Util.Program
    ( executeUntilHalt
    , executeUntilHalt'
    , execute
    , module Util.Computer
    , module Util.RAM
    ) where


import Data.List  (find)
import Data.Default

import Util.Computer
import Util.OpCode
import Util.RAM


-- | Like @execute@, but this function will continue to execute until
-- @execute@ returns @Nothing@ for the cursor position.
execUserUntilHalt :: Program a
                  -> a
                  -> [Data]
                  -> IO [Data]
execUserUntilHalt prog' a io' = loop prog' io' 0 0
  where
    loop p i c b = do
      let proc = (load p a){ action = Run c, stdin = i, base = b }
      (dat, proc') <- runStateT (execute >> pop) proc
      act (return dat) (rerun (dat, proc')) proc'

    rerun (dat, proc') c =
      (dat ++) <$> loop (prog proc') (stdin proc') c (base proc')


executeUntilHalt :: (Default a)
                 => Program a
                 -> [Data]
                 -> IO [Data]
executeUntilHalt p = execUserUntilHalt p def


-- | Like @executeUntilHalt@, but only the finaly IO is returned.
executeUntilHalt' :: (Default a)
                  => Program a
                  -> [Data]
                  -> IO Data
executeUntilHalt' = ((last <$>) .) . executeUntilHalt


-- | Execute a program, starting from the given index in its R/W memory.
--
-- This function accepts a @[Data]@ as the program's "stdin." If the given
-- program @Output@s a signal, the program will return before halting, providing
-- the new cursor positon. If the program halts, @Nothing@ will be returned for
-- the cursor position.
execute :: Runtime a ()
execute = get >>= \p -> act (return ()) (execExpr p) p
  where
    execExpr p c = do
        (inst, assocs) <- lift $ parseExpr (prog p) c
        call inst assocs
        p' <- get
        case action p' of
            Run c -> execute
            _     -> return () -- return $ stdout p'


-- | Parse the expression at the given index of the program's memory.
parseExpr :: Program a
          -> Index
          -> IO (Instruction a, [(Mode, Data)])
parseExpr (Program is mem) i = do
    (modes, opcode) <- splitCode <$> readData mem i
    assocs' <- assocs modes $ inst opcode
    return (inst opcode, assocs')
  where
    inst code = case find (\Instruction{ opcode = c } -> c == code) is of
                    Just i  -> i
                    Nothing -> error $ "unknown opcode @ " ++ show i ++ ": " ++
                                   show code

    assocs :: [Mode] -> Instruction a -> IO [(Mode, Data)]
    assocs ms is' = sequence $ mkAssoc <$> [0 .. argc is' - 1]
      where mkAssoc i' = do d <- readData mem $ i+i'+1
                            return (getMode ms i', d)
