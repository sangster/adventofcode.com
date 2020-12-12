module Days (parts, callDay) where

import Control.Applicative (liftA2)
import Input (lookupInput)
import qualified Year2018
import qualified Year2019
import qualified Year2020
import Debug.Trace
import Data.Maybe
import Solution


type Year = String
type Day  = String


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
callDay :: Year -> Day -> Maybe [(String, Maybe String)]

callDay year "last" = callDay year $ (snd . fst) (last dd)
  where
    dd = filter ((== year) . fst . fst) parts

callDay year day = liftA2 call part input
  where
    part  = lookup (year, day) parts
    input = lookupInput $ year ++ "/" ++ day


call (MkSolveable s) x =
    [(fa x', mayA), (fb x', mayB)]
  where
    (fa, mayA) = partA s
    (fb, mayB) = partB s
    x' = parseInput s x
