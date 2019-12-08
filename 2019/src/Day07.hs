module Day07 (parts) where

import Control.Monad
import Data.List

import Util.Program


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "338603")
        , (part2, Just "63103596")
        ]


part1 input = do prog <- aoc19Program' input
                 res  <- sequence $ run prog 0 <$> phasePermutations 0 5
                 return . show $ maximum res
  where
    run prog io phases     = makeAmps prog phases >>= foldM run' io
    run' i amp@(Amp p _ _) = last . snd <$> runAmp amp [p, i]


part2 input = do prog <- aoc19Program' input
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
        (amp', io') <- case amp of
                           Amp phase _ (Just 0) -> runAmp amp [phase, io]
                           _                    -> runAmp amp [io]
        case amp' of
            Amp _ _ Nothing -> return (amps,           last io') -- halted
            _               -> return (amps ++ [amp'], last io')


data Amp   = Amp Phase Program (Maybe Index)
type Phase = Int


phasePermutations :: Int -> Int -> [[Int]]
phasePermutations i n = permutations [i .. i + n - 1]


makeAmps :: Program -> [Phase] -> IO [Amp]
makeAmps prog ps = sequence $ makeAmp prog <$> ps


makeAmp :: Program -> Phase -> IO Amp
makeAmp (Program i m) p = memcpy m >>= return . (flip (Amp p) $ Just 0) . Program i


-- | Run the program in the given amplifier using the provided input.
-- The result will be the amp, modified with its new cursor, and its output.
runAmp :: Amp
       -> [Data]
       -> IO (Amp, [Data])
runAmp amp@(Amp _ _ Nothing)  io = return $ (amp, tail io)
runAmp (Amp p mem (Just cur)) io = do (cur', io') <- runProgram mem io cur
                                      return $ (Amp p mem cur', io')
