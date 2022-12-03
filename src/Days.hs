module Days
  ( solutions
  , callDayTimed
  , mostRecentDay
  , dateStr
  ) where

import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import GeneratedSolutions (lookupInput, solutions, Year, Day)
import Solution
import System.TimeIt (timeItT)
import Util.Color

type Runtime = Double


-- | Execute the solution for a single December day in a single year.
-- The 3 "Runtimes" are how long parsing, Part A, and Part B took, respectively.
callDayTimed :: MonadIO m
             => Year
             -> Day
             -> MaybeT m (Runtime, Runtime, Runtime, [(String, Maybe String)])
callDayTimed year "last" = callDayTimed year (mostRecentDay year)
callDayTimed year day    = do
    part  <- MaybeT . pure $ lookup (year, day) solutions
    input <- MaybeT . pure . lookupInput $ year ++ "/" ++ day
    liftIO $ call part input


-- | Return the most recent Day in the given year that has a solution so far.
mostRecentDay :: Year -> Day
mostRecentDay year = (snd . fst) (last dd)
  where
    dd = filter ((== year) . fst . fst) solutions


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


-- | Lazily evaluate
evaluate' :: MonadIO m
          => a
          -> m a
evaluate' = liftIO . evaluate


-- | Render the date in a colorful way.
dateStr :: Year -> Day -> String
dateStr y "last" = dateStr y (mostRecentDay y)
dateStr y d      = ansiForeground Dull Cyan y
                ++ "-12-"
                ++ ansiForeground Dull Magenta d
