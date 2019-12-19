module Days (parts, callDay) where

import Control.Applicative  (liftA2)

import           Input     (lookupInput)
import qualified Year2018
import qualified Year2019


type Year = String
type Day  = String


parts :: [( (Year, Day), [((String -> IO String), Maybe String)] )]
parts =
  daysFor "2018" Year2018.days
  ++
  daysFor "2019" Year2019.days


daysFor :: Year
        -> [[((String -> IO String), Maybe String)]]
        -> [( (Year, Day), [((String -> IO String), Maybe String)] )]
daysFor y pp = [ ((y, fmt d), p) | (d, p) <- zip [1..] pp]
  where
    fmt d | d < 10    = ('0':show d)
          | otherwise = show d


-- | Execute the solution for a single December day in a single year.
callDay :: Year -> Day -> Maybe [(IO String, Maybe String)]

callDay year "last" = callDay year $ (snd . fst) (last dd)
  where
    dd = filter ((== year) . fst . fst) parts

callDay year day = liftA2 call part input
  where
    part      = lookup (year, day) parts
    input     = lookupInput $ year ++ "/" ++ day
    call fs x = [(f x, expected) | (f, expected) <- fs]
