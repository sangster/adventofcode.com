module Y2019.Day07 (parts) where

import Data.List  (permutations)

import Util.InstructionSet
import Util.Program


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "338603")
        , (part2, Just "63103596")
        ]


part1 input = do prog <- program input
                 res  <- sequence $ run prog 0 <$> phasePermutations 0 5
                 return . show $ maximum res
  where
    run prog io phases   = makeAmps prog phases >>= foldM run' io
    run' i amp@(Amp p _) = last . snd <$> runAmp amp [p, i]


part2 input = do prog <- program input
                 res  <- sequence $ run prog 0 <$> phasePermutations 5 5
                 return . show $ maximum res
  where
    run prog io phases = makeAmps prog phases >>= loop io

    -- | Continuously re-run all unhalted amps
    loop io amps = do (amps', io') <- foldM run' ([], io) amps
                      case amps' of
                         [] -> return io'
                         _  -> loop io' amps'

    -- | Run the given amp's program, folding the modified amp into the given
    --   list of amps executed during this loop for the next loop, unless it has
    --   halted.
    run' (amps, io) amp = do
        (amp', io') <- case action . ampProc $ amp of
                           Run 0 -> runAmp amp [phase amp, io]
                           _     -> runAmp amp [io]

        return $ act (amps, last io')
                     (\_-> (amps ++ [amp'], last io'))
                     (ampProc amp')


data Amp   = Amp { phase :: Phase, ampProc :: Process () }
type Phase = Int


program :: String -> IO Program'
program = (fmap $ Program instructions) . parseRAM
  where instructions :: InstructionSet'
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


makeAmps :: Program () -> [Phase] -> IO [Amp]
makeAmps prog ps = sequence $ makeAmp prog <$> ps


makeAmp :: Program () -> Phase -> IO Amp
makeAmp (Program i m) ph = memcpy m >>= return . (Amp ph) . load' . (Program i)


-- | Run the program in the given amplifier using the provided input.
-- The result will be the amp, modified with its new cursor, and its output.
runAmp :: Amp
       -> [Data]
       -> IO (Amp, [Data])

runAmp amp io = act returnAmp runAmp' $ ampProc amp
  where
    returnAmp = return $ (amp, tail (fifo . ampProc $ amp))
    runAmp' _ = do (i', p') <- runStateT execute (ampProc amp){ fifo = io }
                   return $ (Amp (phase amp) p', i')
