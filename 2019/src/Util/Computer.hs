module Util.Computer
    ( Expr(..)
    , Mode(..)
    , Op(..)
    , runUntilHalt
    , runUntilHalt'
    , runProgram
    , runProgram'
    , module Util.RAM
    ) where

import Data.Bool
import Safe       (atDef)

import Util.RAM


data Expr  = Expr [Mode] Op
data Mode  = Position | Immediate

data Op = Halt
        | Add       Int Int Index
        | Multiply  Int Int Index
        | Store     Int
        | Output    Int
        | JumpTrue  Int Int
        | JumpFalse Int Int
        | LessThan  Int Int Index
        | Equals    Int Int Index
  deriving Show


-- | Like @runProgram@, but this function will continue to execute until
-- @runProgram@ returns @Nothing@ for the cursor position.
runUntilHalt :: RAM
             -> [Data]
             -> Index
             -> IO [Data]
runUntilHalt mem io cursor = do (maybeCursor, dat) <- runProgram mem io cursor
                                case maybeCursor of
                                    Nothing -> return $ dat
                                    Just c  -> runUntilHalt mem dat c


-- | Like @runUntilHalt@, but only the last output is returned.
-- runUntilHalt' mem io cursor = last <$> runUntilHalt mem io cursor
runUntilHalt' = (((last <$>) .) .) . runUntilHalt


-- | Execute the program found in the given R/W memory, from the given index.
--
-- This function accepts a @[Data]@, representing a FIFO list of inputs and
-- outputs. If the given program @Output@s a signal, the program will return
-- before halting, providing the new cursor positon and outputted data. If the
-- program halts, @Nothing@ will be returned for the cursor position, along with
-- any remaining output data.
--
-- @Store@ expressions read from the head and @Output@ expressions append to the
-- tail.
runProgram :: RAM
           -> [Data]
           -> Index
           -> IO (Maybe Index, [Data])
runProgram mem io cursor = do
    expr <- parseExpr mem cursor

    case expr of
        Expr _     (Halt             ) -> return (Nothing, io)
        Expr modes (Add       a b dst) -> doMath (+)    modes a b dst
        Expr modes (Multiply  a b dst) -> doMath (*)    modes a b dst
        Expr _     (Store         dst) -> store                   dst
        Expr _     (Output        dst) -> output                  dst
        Expr modes (JumpTrue  a   dst) -> doJump (/= 0) modes a   dst
        Expr modes (JumpFalse a   dst) -> doJump (== 0) modes a   dst
        Expr modes (LessThan  a b dst) -> doCmp  (<)    modes a b dst
        Expr modes (Equals    a b dst) -> doCmp  (==)   modes a b dst
  where
    doMath f m a b dst = do a' <- modeRead m 0 a
                            b' <- modeRead m 1 b
                            writeData mem dst $ f a' b'
                            runProgram mem io $ cursor + 4

    doJump f m a dst = do a'   <- modeRead m 0 a
                          dst' <- modeRead m 1 dst
                          runProgram mem io $ bool (cursor + 3) dst' (f a')

    doCmp f m a b dst = do a' <- modeRead m 0 a
                           b' <- modeRead m 1 b
                           writeData mem dst $ bool 0 1 $ f a' b'
                           runProgram mem io $ cursor + 4

    store dst = do writeData mem dst $ head io
                   runProgram mem (tail io) $ cursor + 2

    output dst = do dat <- readData mem dst
                    return (Just (cursor + 2), io ++ [dat])

    modeRead :: [Mode] -> Index -> Int -> IO Data
    modeRead modes i a = case atDef Position modes i of
                             Immediate -> return a
                             Position  -> readData mem a


-- | Parse the expression at the given cursor.
parseExpr :: RAM
          -> Index
          -> IO Expr
parseExpr mem cursor = do
    (modes, opcode) <- splitCode <$> readData mem cursor
    op <- case opcode of
            99 -> return Halt
            1  -> expr3  Add
            2  -> expr3  Multiply
            3  -> expr1  Store
            4  -> expr1  Output
            5  -> expr2  JumpTrue
            6  -> expr2  JumpFalse
            7  -> expr3  LessThan
            8  -> expr3  Equals
            _  -> error $ "unknown code " ++ show opcode
    return $ Expr modes op
  where
    expr1 f = do { a <- read' 1;                             return $ f a     }
    expr2 f = do { a <- read' 1; b <- read' 2;               return $ f a b   }
    expr3 f = do { a <- read' 1; b <- read' 2; c <- read' 3; return $ f a b c }
    read' n = readData mem $ cursor + n :: IO Int


-- | Like runProgram', but on returning the output data, and not the cursor's
-- final position.
runProgram' = ((((last . snd) <$>) .) .) . runProgram


-- | Split a number into its modes and opcode components.
splitCode :: Int -> ([Mode], Int)
splitCode n = (modes $ n `div` 100, n `mod` 100)
  where modes x
          | x >= 10   = modes (x `mod` 10) ++ modes (x `div` 10)
          | x == 1    = [Immediate]
          | otherwise = [Position]
