module Days (days, callDay) where

import Control.Applicative (liftA2)
import Data.List (intercalate)

import Input (lookupInput)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06


days :: [(FilePath, [((String -> IO String), Maybe String)])]
days = [ ("01", Day01.parts)
       , ("02", Day02.parts)
       , ("03", Day03.parts)
       , ("04", Day04.parts)
       , ("05", Day05.parts)
       , ("06", Day06.parts)
       ]


callDay :: String -> Maybe [(IO String, Maybe String)]
callDay "last" = callDay (fst . last $ days)
callDay day = liftA2 callParts (lookup day days) (lookupInput day)
  where callParts fs x = [(f x, expected) | (f, expected) <- fs]
