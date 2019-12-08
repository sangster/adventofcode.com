module Day02 (parts) where

import Util.Computer

parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "2782414")
        , (part2, Just "9820")
        ]


part1 input = do mem  <- parseRAM input
                 zero <- runAdjustedProject mem 12 2
                 return $ show zero


part2 input = do mem    <- parseRAM input
                 result <- findNounAndVerb mem expectedResult 0 0
                 return $ render result
  where
    expectedResult = 19690720
    render = maybe "fail" (\(noun, verb) -> show $ 100 * noun + verb)


runAdjustedProject mem noun verb = do writeData mem 1 noun
                                      writeData mem 2 verb
                                      runUntilHalt mem [0] 0
                                      readData mem 0


findNounAndVerb :: RAM
                -> Data  -- expected
                -> Data  -- noun
                -> Data  -- verb
                -> IO (Maybe (Data, Data))
findNounAndVerb _ _ 100  _   = return Nothing
findNounAndVerb m e noun 100 = findNounAndVerb m e (noun + 1) 0

findNounAndVerb mem expected noun verb = do
    mem'   <- memcpy mem
    result <- runAdjustedProject mem' noun verb

    if (expected == result)
        then return $ Just (noun, verb)
        else findNounAndVerb mem expected noun (verb + 1)
