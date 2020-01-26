module Y2019.Day02 (parts) where

import Util.InstructionSet
import Util.Program


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "2782414")
        , (part2, Just "9820")
        ]


part1 input = runST $ do
    prog <- program input
    show <$> runAdjustedProject prog 12 2


part2 input = runST $ do
    prog <- program input
    render <$> findNounAndVerb prog expectedResult 0 0
  where
    expectedResult = 19690720
    render = maybe "fail" $ \(noun, verb) -> show $ 100 * noun + verb


program :: PrimMonad m => String -> m (Program' m)
program = (fmap $ Program instructions) . parseRAM
  where
    instructions =
      [ halt "HALT" 99
      , math " ADD"  1 (+)
      , math "MULT"  2 (*)
      ]


runAdjustedProject prog noun verb = do
    writeData (mem prog) 1 noun
    writeData (mem prog) 2 verb
    executeUntilHalt prog []
    readData (mem prog) 0


findNounAndVerb :: PrimMonad m
                => Program' m
                -> Data     -- expected
                -> Data     -- noun
                -> Data     -- verb
                -> m (Maybe (Data, Data))
findNounAndVerb _ _ 100  _   = pure Nothing
findNounAndVerb p e noun 100 = findNounAndVerb p e (noun + 1) 0

findNounAndVerb prog expected noun verb = do
    mem'   <- memcpy $ mem prog
    result <- runAdjustedProject (Program (is prog) mem') noun verb

    if expected == result
      then pure $ Just (noun, verb)
      else findNounAndVerb prog expected noun $ verb + 1
