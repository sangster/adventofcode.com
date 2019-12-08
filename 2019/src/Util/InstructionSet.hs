module Util.InstructionSet
    ( InstructionSet
    , Instruction (..)
    , aoc19Set
    , halt
    , math
    , store
    , output
    , jump
    , cmp
    ) where


import Data.Bool

import Util.Computer
import Util.RAM


type InstructionSet = [Instruction]

data Instruction = Instruction
    { opcode :: OpCode           -- Its code in memory.
    , argc   :: Int              -- The number of arguments it uses.
    , call   :: InstructionCall  -- What it does.
    }

type OpCode           = Data
type InstructionCall  = RAM
                     -> Index
                     -> [(Mode, Data)]
                     -> [Data]
                     -> IO (Action, [Data])


-- The full Advent of Code 2019 instruction set.
aoc19Set = [ halt  99
           , math   1 (+)
           , math   2 (*)
           , store  3
           , output 4
           , jump   5 (/= 0)
           , jump   6 (== 0)
           , cmp    7 (<)
           , cmp    8 (==)
           ]


halt c = Instruction { opcode = c, argc = 0, call = halt' }
  where halt' _ _ _ io = return (Halt, io)


math c f = Instruction { opcode = c, argc = 3, call = math' }
  where math' m i assocs io = do x <- modeRead m assocs 0
                                 y <- modeRead m assocs 1
                                 let dst = snd $ assocs !! 2
                                 writeData m dst $ f x y
                                 return (Jump $ i+4, io)


jump c f = Instruction { opcode = c, argc = 2, call = jump' }
  where jump' m i assocs io = do x   <- modeRead m assocs 0
                                 dst <- modeRead m assocs 1
                                 return (Jump $ bool (i+3) dst (f x), io)


store c = Instruction { opcode = c, argc = 1, call = store' }
  where store' m i [(_, dst)] io = do writeData m dst $ head io
                                      return (Jump $ i+2, tail io)


output c = Instruction { opcode = c, argc = 1, call = output' }
  where output' m i [(_, src)] io = do dat <- readData m src
                                       return (Signal $ i+2, io ++ [dat])


cmp c f = Instruction { opcode = c, argc = 3, call = cmp' }
  where cmp' m i assocs io = do x <- modeRead m assocs 0
                                y <- modeRead m assocs 1
                                let dst = snd $ assocs !! 2
                                writeData m dst $ bool 0 1 $ f x y
                                return (Jump $ i+4, io)
