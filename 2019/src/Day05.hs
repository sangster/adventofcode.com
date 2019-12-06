module Day05 (parts) where

import Data.Array.IO
import Data.Bool     (bool)
import Safe          (atDef)

import Util.Parser hiding (token)


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "7259358")
        , (part2, Just "11826654")
        ]


part1 input = do { mem <- parseInitialMemory input; show <$> runProgram mem 1 0 }
part2 input = do { mem <- parseInitialMemory input; show <$> runProgram mem 5 0 }


type Position        = Int
type Value           = Int
type ReadWriteMemory = IOUArray Position Int

data Expr = Expr [Mode] Op
data Mode = Position | Immediate

data Op = Halt
        | Add       Int Int Position
        | Multiply  Int Int Position
        | Store     Int
        | Output    Int
        | JumpTrue  Int Int
        | JumpFalse Int Int
        | LessThan  Int Int Position
        | Equals    Int Int Position


-- | Convert the input file into a Read/Write buffer of memory.
parseInitialMemory :: String
                   -> IO ReadWriteMemory
parseInitialMemory input = newListArray (0, length codes - 1) codes
  where codes = runParser cells input


-- | Parse a list of memory cells from the input string, containing both opcodes
--   and data.
cells :: Parser [Int]
cells = many $ token cell
  where
    cell    = (some $ satisfy (/= ',')) >>= return . read
    token p = do { t <- p; many (char ','); return t }


-- | Execute the program found in the given R/W memory, using the given value as
--   the origin input, and starting the program's execution at the given
--   position.
runProgram :: ReadWriteMemory
           -> Value
           -> Position
           -> IO Value
runProgram mem input cursor = do
    expr <- parseExpr mem cursor

    case expr of
        Expr _     (Halt             ) -> return input
        Expr modes (Add       a b dst) -> doMath (+)    modes a b dst
        Expr modes (Multiply  a b dst) -> doMath (*)    modes a b dst
        Expr _     (Store         dst) -> doMem  store            dst input
        Expr _     (Output        dst) -> doMem  output           dst input
        Expr modes (JumpTrue  a   dst) -> doJump (/= 0) modes a   dst
        Expr modes (JumpFalse a   dst) -> doJump (== 0) modes a   dst
        Expr modes (LessThan  a b dst) -> doCmp  (<)    modes a b dst
        Expr modes (Equals    a b dst) -> doCmp  (==)   modes a b dst
  where
    doMath f m a b dst = do a' <- modeRead m 0 a
                            b' <- modeRead m 1 b
                            writeArray mem dst $ f a' b'
                            runProgram mem input $ cursor + 4

    doMem f dst input = do out <- f dst input
                           runProgram mem out $ cursor + 2

    doJump f m a dst = do a'   <- modeRead m 0 a
                          dst' <- modeRead m 1 dst
                          runProgram mem input $ bool (cursor + 3) dst' (f a')

    doCmp f m a b dst = do a' <- modeRead m 0 a
                           b' <- modeRead m 1 b
                           writeArray mem dst $ bool 0 1 $ f a' b'
                           runProgram mem input $ cursor + 4

    store :: Position -> Value -> IO Int
    store dst i = do { writeArray mem dst i; return i }

    output :: Position -> Value -> IO Int
    output dst _ = readArray  mem dst

    modeRead :: [Mode] -> Position -> Int -> IO Value
    modeRead modes i a = case atDef Position modes i of
                             Immediate -> return a
                             Position  -> readArray mem a


-- | Parse the expression at the given cursor.
parseExpr :: ReadWriteMemory
          -> Position
          -> IO Expr
parseExpr mem cursor = do
    (modes, opcode) <- splitCode <$> readArray mem cursor
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
    read' n = readArray mem $ cursor + n :: IO Int


-- | Split a number into its modes and opcode components.
splitCode :: Int -> ([Mode], Int)
splitCode n = (modes $ n `div` 100, n `mod` 100)
  where modes x
          | x >= 10   = modes (x `mod` 10) ++ modes (x `div` 10)
          | x == 1    = [Immediate]
          | otherwise = [Position]
