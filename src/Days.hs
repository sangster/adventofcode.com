module Days (parts, callDayTimed) where

import           Control.Applicative (liftA2)
import           Control.Exception (evaluate)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Input (lookupInput)
import           Solution
import           System.TimeIt (timeItT)
import qualified Year2018
import qualified Year2019
import qualified Year2020


type Year    = String
type Day     = String
type Runtime = Double


parts :: [( (Year, Day), Solveable DaySolution)]
parts = daysFor "2018" Year2018.days
     ++ daysFor "2019" Year2019.days
     ++ daysFor "2020" Year2020.days


daysFor :: Year
        -> [Solveable DaySolution]
        -> [((Year, Day), Solveable DaySolution)]
daysFor y parts = [ ((y, fmt d), p) | (d, p) <- zip [1..] parts]
  where
    fmt d | d < 10    = ('0':show d)
          | otherwise = show d


-- | Execute the solution for a single December day in a single year.
-- The 3 "Runtimes" are how long parsing, Part A, and Part B took, respectively.
callDayTimed :: MonadIO m
             => Year
             -> Day
             -> MaybeT m (Runtime, Runtime, Runtime, [(String, Maybe String)])
callDayTimed year "last" = callDayTimed year $ (snd . fst) (last dd)
  where
    dd = filter ((== year) . fst . fst) parts
callDayTimed year day = do
    part  <- MaybeT . pure $ lookup (year, day) parts
    input <- MaybeT . pure . lookupInput $ year ++ "/" ++ day
    liftIO $ call part input


call :: MonadIO m
     => Solveable DaySolution
     -> String
     -> m (Runtime, Runtime, Runtime, [(String, Maybe String)])
call (MkSolveable s) input = do
    (timeP, input') <- timeItT . evaluate' $ parseInput s input
    (timeA, resA)   <- timeItT . evaluate' $ fa input'
    (timeB, resB)   <- timeItT . evaluate' $ fb input'

    pure (timeP, timeA, timeB, [(resA, mayA), (resB, mayB)])
  where
    (fa, mayA) = partA s
    (fb, mayB) = partB s


-- Lazily evaluate
evaluate' :: MonadIO m
          => a
          -> m a
evaluate' = liftIO . evaluate
