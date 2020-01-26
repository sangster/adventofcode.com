module Y2019.Day07 (parts) where

import Data.List  (permutations)

import Util.InstructionSet
import Util.Program


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "338603")
        , (part2, Just "63103596")
        ]


part1 input = runST $ do
    prog <- program input
    res  <- sequence $ run prog 0 <$> phasePermutations 0 5
    pure . show $ maximum res
  where
    run prog io phases   = makeAmps prog phases >>= foldM run' io
    run' i amp@(Amp p _) = last . snd <$> runAmp amp [p, i]


part2 input = runST $ do
    prog <- program input
    res  <- sequence $ run prog 0 <$> phasePermutations 5 5
    pure . show $ maximum res
  where
    run prog io phases = makeAmps prog phases >>= loop io

    -- | Continuously re-run all unhalted amps
    loop io amps = do
      (amps', io') <- foldM run' ([], io) amps
      case amps' of
          [] -> pure io'
          _  -> loop io' amps'

    -- | Run the given amp's program, folding the modified amp into the given
    --   list of amps executed during this loop for the next loop, unless it has
    --   halted.
    run' (amps, io) amp = do
        (amp', io') <- case action . ampProc $ amp of
                           Run 0 -> runAmp amp [phase amp, io]
                           _     -> runAmp amp [io]

        pure $ act (amps, last io')
                   (\_-> (amps ++ [amp'], last io'))
                   (ampProc amp')


data Amp m
  = Amp
    { phase   :: Phase
    , ampProc :: Process m ()
    }
type Phase = Int


program :: PrimMonad m => String -> m (Program' m)
program = (fmap $ Program instructions) . parseRAM
  where
    instructions = [ halt   "HALT" 99
                   , math   " ADD"  1 (+)
                   , math   "MULT"  2 (*)
                   , store  "STOR"  3
                   , output " OUT"  4
                   , jump   " JEQ"  5 (/= 0)
                   , jump   "JNEQ"  6 (== 0)
                   , cmp    "  LT"  7 (<)
                   , cmp    "  EQ"  8 (==)
                   ]


phasePermutations :: Int -> Int -> [[Int]]
phasePermutations i n = permutations [i .. i + n - 1]


makeAmps :: PrimMonad m => Program m () -> [Phase] -> m [Amp m]
makeAmps prog ps = sequence $ makeAmp prog <$> ps


makeAmp :: PrimMonad m => Program m () -> Phase -> m (Amp m)
makeAmp (Program i m) ph = memcpy m >>= pure . (Amp ph) . load' . (Program i)


-- | Run the program in the given amplifier using the provided input.
-- The result will be the amp, modified with its new cursor, and its output.
runAmp :: PrimMonad m
       => Amp m
       -> [Data]
       -> m (Amp m, [Data])
runAmp amp io = act pureAmp runAmp' $ ampProc amp
  where
    pureAmp = pure (amp, tail (stdout . ampProc $ amp))
    runAmp' _ = do (i', p') <- runStateT execAndPoll (ampProc amp){ stdin = io }
                   pure $ (Amp (phase amp) p', i')

    execAndPoll = execute >> stdout <$> get
