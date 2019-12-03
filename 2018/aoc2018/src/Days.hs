module Days (days, callDay) where

import Control.Applicative (liftA2)
import Data.List (intercalate)

import Input (lookupInput)
import qualified Day01
import qualified Day02
import qualified Day03


days :: [(FilePath, [(String -> String)])]
days = [ ("01", Day01.parts)
       , ("02", Day02.parts)
       , ("03", Day03.parts)
       ]


callDay :: String -> Maybe [String]
callDay day = liftA2 callParts (lookup day days) (lookupInput day)
  where callParts fs x = ($ x) <$> fs
