module Day02 (parts) where

import Util.Program

parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "2782414")
        , (part2, Just "9820")
        ]


part1 input = do prog <- aoc19Program' input
                 zero <- runAdjustedProject prog 12 2
                 return $ show zero


part2 input = do prog   <- aoc19Program' input
                 result <- findNounAndVerb prog expectedResult 0 0
                 return $ render result
  where
    expectedResult = 19690720
    render = maybe "fail" $ \(noun, verb) -> show $ 100 * noun + verb


runAdjustedProject prog noun verb = do writeData (mem prog) 1 noun
                                       writeData (mem prog) 2 verb
                                       runUntilHalt prog [] 0
                                       readData (mem prog) 0


findNounAndVerb :: Program
                -> Data     -- expected
                -> Data     -- noun
                -> Data     -- verb
                -> IO (Maybe (Data, Data))
findNounAndVerb _ _ 100  _   = return Nothing
findNounAndVerb p e noun 100 = findNounAndVerb p e (noun + 1) 0

findNounAndVerb prog expected noun verb = do
    mem'   <- memcpy $ mem prog
    result <- runAdjustedProject (Program (is prog) mem') noun verb

    if (expected == result)
        then return $ Just (noun, verb)
        else findNounAndVerb prog expected noun $ verb + 1
