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
        (dat, proc') <- runStateT execute $ (load p a){ action = Run c
                                                      , fifo   = i
                                                      , base   = b
                                                      }
        act (return dat) (\c' -> loop (prog proc') dat c' (base proc')) proc'


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
-- This function accepts a @[Data]@, representing a FIFO list of inputs and
-- outputs. If the given program @Output@s a signal, the program will return
-- before halting, providing the new cursor positon and outputted data. If the
-- program halts, @Nothing@ will be returned for the cursor position, along with
-- any remaining output data.
--
-- @Store@ expressions read from the head and @Output@ expressions append to the
-- tail.
execute :: Runtime a [Data]
execute = do { proc <- get; act (return $ fifo proc) (execExpr proc) proc }
  where
    execExpr p c = do
        (inst, assocs) <- lift $ parseExpr (prog p) c
        call inst assocs
        p' <- get
        case action p' of
            Run c -> execute
            _     -> return $ fifo p'


-- | Parse the expression at the given index of the program's memory.
parseExpr :: Program a
          -> Index
          -> IO (Instruction a, [(Mode, Data)])
parseExpr (Program is mem) i = do
    (modes, opcode) <- splitCode <$> readData mem i
    assocs' <- assocs modes $ inst opcode
    return (inst opcode, assocs')
  where
    inst code = case (find (\Instruction{ opcode = c } -> c == code) is) of
                    Just i  -> i
                    Nothing -> error $ "unknown opcode @ " ++ show i ++ ": " ++
                                   show code

    assocs :: [Mode] -> Instruction a -> IO [(Mode, Data)]
    assocs ms is' = sequence $ mkAssoc <$> [0 .. argc is' - 1]
      where mkAssoc i' = do d <- readData mem $ i+i'+1
                            return (getMode ms i', d)
