module Days (days, callDay) where

import Control.Applicative  (liftA2)
import Data.List            (intercalate)
import Text.Printf          (printf)

import Input (lookupInput)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12


parts = [ Day01.parts
        , Day02.parts
        , Day03.parts
        , Day04.parts
        , Day05.parts
        , Day06.parts
        , Day07.parts
        , Day08.parts
        , Day09.parts
        , Day10.parts
        , Day11.parts
        , Day12.parts
        ]


days :: [(FilePath, [((String -> IO String), Maybe String)])]
days = [ (fmt d, part) | (d, part) <- zip [1..] parts]
  where fmt d | d < 10    = ('0':show d)
              | otherwise = show d


callDay :: String -> Maybe [(IO String, Maybe String)]
callDay "last" = callDay (fst . last $ days)
callDay day = liftA2 callParts (lookup day days) (lookupInput day)
  where callParts fs x = [(f x, expected) | (f, expected) <- fs]
