module Util.Program
    ( Program(..)
    , aoc19Program
    , aoc19Program'
    , runUntilHalt
    , runUntilHalt'
    , runProgram
    , runProgram'
    , module Util.RAM
    ) where

import Data.Bool
import Data.List
import Data.Maybe
import Safe       (atDef)

import Util.Computer
import Util.InstructionSet
import Util.RAM


data Program = Program { is  :: InstructionSet
                       , mem :: RAM
                       }


-- | Create a @Program@ using the complete Advent Of Code 2019 instruction set.
aoc19Program :: RAM -> Program
aoc19Program = Program aoc19Set


-- | Create a @Program@ using the complete Advent Of Code 2019 instruction set,
-- parsing the @RAM@ from the given string.
aoc19Program' :: String -> IO Program
aoc19Program' = fmap aoc19Program . parseRAM


-- | Like @runProgram@, but this function will continue to execute until
-- @runProgram@ returns @Nothing@ for the cursor position.
runUntilHalt :: Program
             -> [Data]
             -> Index
             -> IO [Data]
runUntilHalt prog io cursor = do (maybeCursor, dat) <- runProgram prog io cursor
                                 case maybeCursor of
                                     Nothing -> return $ dat
                                     Just c  -> runUntilHalt prog dat c


-- | Like @runUntilHalt@, but only the last output is returned.
runUntilHalt' = (((last <$>) .) .) . runUntilHalt


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
runProgram :: Program
           -> [Data]
           -> Index
           -> IO (Maybe Index, [Data])
runProgram prog io i = do
    (inst, assocs) <- parseExpr prog i
    (action, dat)  <- call inst (mem prog) i assocs io

    case action of
        Halt     -> return (Nothing, dat)
        Signal c -> return (Just c, dat)
        Jump   c -> runProgram prog dat c


-- | Parse the expression at the given index of the program's memory.
parseExpr :: Program
          -> Index
          -> IO (Instruction, [(Mode, Data)])
parseExpr (Program is mem) i = do
    (modes, opcode) <- splitCode <$> readData mem i
    assocs' <- assocs modes $ inst opcode
    return (inst opcode, assocs')
  where
    inst code = case (find (\Instruction{ opcode = c } -> c == code) is) of
                    Nothing -> error $ "unknown opcode @ " ++ show i ++ ": " ++
                                   show code
                    Just i  -> i

    assocs :: [Mode] -> Instruction -> IO [(Mode, Data)]
    assocs ms is' = sequence $ mkAssoc <$> [0 .. argc is' - 1]
      where mkAssoc i' = do d <- readData mem $ i+i'+1
                            return (atDef Position ms i', d)


-- | Like runProgram', but only returning the output data, and not the cursor's
-- final position.
runProgram' = ((((last . snd) <$>) .) .) . runProgram


-- | Split a number into its modes and opcode components.
splitCode :: Int -> ([Mode], Int)
splitCode n = (modes $ n `div` 100, n `mod` 100)
  where modes x | x >= 10   = modes (x `mod` 10) ++ modes (x `div` 10)
                | x == 1    = [Immediate]
                | otherwise = [Position]
