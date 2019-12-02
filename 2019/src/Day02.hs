module Day02 (parts, part1, part2) where

import Data.Array.IO


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "2782414")
        , (part2, Just "9820")
        ]


part1 input = do mem  <- parseInitialMemory input
                 zero <- runAdjustedProject mem 12 2
                 return $ show zero


part2 input = do mem    <- parseInitialMemory input
                 result <- findNounAndVerb mem expectedResult 0 0
                 return $ render result
  where
    expectedResult = 19690720
    render = maybe "fail" (\(noun, verb) -> show $ 100 * noun + verb)


type Cursor = Int
type Value  = Int
type ReadWriteMemory = IOUArray Cursor Value

data Op = Add | Multiply | Halt


parseInitialMemory :: String -> IO ReadWriteMemory
parseInitialMemory str = newListArray (0, length ints - 1) ints
  where
    ints = read <$> split str
    split [] = [""]
    split (c:cs) | c == ','  = "" : rest
                 | otherwise = (c : head rest) : tail rest
      where rest = split cs


runAdjustedProject mem noun verb = do writeArray mem 1 noun
                                      writeArray mem 2 verb
                                      runProgram mem 0
                                      readArray mem 0


runProgram mem cursor = do
    opCode <- readArray mem cursor
    case parseOp opCode of
        Halt -> return ()
        op'  -> execute mem op' cursor >>= maybe (return ()) (runProgram mem)


parseOp 1  = Add
parseOp 2  = Multiply
parseOp 99 = Halt
parseOp c  = error $ "unknown opcode: " ++ (show c)


execute :: ReadWriteMemory
        -> Op
        -> Cursor
        -> IO (Maybe Cursor)
execute mem Halt _ = return Nothing
execute mem op' cursor = do
    arg1   <- read' (cursor + 1) >>= read'
    arg2   <- read' (cursor + 2) >>= read'
    resLoc <- read' (cursor + 3)

    writeArray mem resLoc $ execute op' arg1 arg2
    return $ Just (cursor + 4)
  where
    read' = readArray mem
    execute Add x y = x + y
    execute _   x y = x * y


findNounAndVerb :: ReadWriteMemory
                -> Value            -- expected
                -> Value            -- noun
                -> Value            -- verb
                -> IO (Maybe (Value, Value))
findNounAndVerb _ _ 100  _   = return Nothing
findNounAndVerb m e noun 100 = findNounAndVerb m e (noun + 1) 0

findNounAndVerb mem expected noun verb = do
    cpy <- memCopy mem
    result <- runAdjustedProject cpy noun verb

    if (expected == result)
        then return $ Just (noun, verb)
        else findNounAndVerb mem expected noun (verb + 1)

  where memCopy m = do bounds <- getBounds m
                       items  <- getElems m
                       newListArray bounds items
