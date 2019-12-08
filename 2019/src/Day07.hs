module Day07 (parts) where

import Control.Monad
import Data.Char
import Data.Bool     (bool)
import Data.List
import Safe          (atDef)

import Util.RAM

parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "338603")
        , (part2, Just "63103596")
        ]


part1 input = do mem <- parseRAM input
                 res <- sequence $ run mem 0 <$> phasePermutations 0 5
                 return . show $ maximum res
  where
    run mem io phases      = makeAmps mem phases >>= foldM run' io
    run' i amp@(Amp p _ _) = last . snd <$> runAmp amp [p, i]


part2 input = do mem <- parseRAM input
                 res <- sequence $ run mem 0 <$> phasePermutations 5 5
                 return . show $ maximum res
  where
    run mem io phases = do makeAmps mem phases >>= loop io

    -- | Continuously re-run all unhalted amps
    loop io amps = do (amps', io') <- foldM run' ([], io) amps
                      case amps' of
                         [] -> return io'
                         _  -> loop io' amps'

    -- | Run the given amp's program, folding the modified amp into the given
    --   list of amps executed during this loop for the next loop, unless it has
    --   halted.
    run' (amps, io) amp = do
        (amp', io') <- case amp of
                           Amp phase _ (Just 0) -> runAmp amp [phase, io]
                           _                    -> runAmp amp [io]
        case amp' of
            Amp _ _ Nothing -> return (amps,           last io') -- halted
            _               -> return (amps ++ [amp'], last io')


data Amp   = Amp Phase RAM (Maybe Index)
type Phase = Int
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


phasePermutations :: Int -> Int -> [[Int]]
phasePermutations i n = permutations [i .. i + n - 1]


makeAmps :: RAM -> [Phase] -> IO [Amp]
makeAmps mem ps = sequence $ (makeAmp mem) <$> ps

makeAmp :: RAM -> Phase -> IO Amp
makeAmp m p = memcpy m >>= return . (flip (Amp p) (Just 0))


-- | Run the program in the given amplifier using the provided input.
-- The result will be the amp, modified with its new cursor, and its output.
runAmp :: Amp
       -> [Data]
       -> IO (Amp, [Data])
runAmp amp@(Amp _ _ Nothing) io = return $ (amp, tail io)
runAmp (Amp p mem (Just cur)) io = do (cur', io') <- runProgram mem io cur
                                      return $ (Amp p mem cur', io')



-- | Execute the program found in the given R/W memory, from the given index.
--
-- This function accepts and returns a @[Data]@, representing a FIFO list of
-- inputs and outputs. @Store@ expressions read from the head and @Output@
-- expressions append to the tail.
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


-- | Split a number into its modes and opcode components.
splitCode :: Int -> ([Mode], Int)
splitCode n = (modes $ n `div` 100, n `mod` 100)
  where modes x
          | x >= 10   = modes (x `mod` 10) ++ modes (x `div` 10)
          | x == 1    = [Immediate]
          | otherwise = [Position]
