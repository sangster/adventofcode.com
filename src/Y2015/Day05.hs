module Y2015.Day05 (parts) where

import Data.Bool (bool)
import Data.List (isInfixOf)


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "238")
        , (part2, Just "69")
        ]


part1 input = show . length $ filter (valid 3 1) (lines input)
  where
    isVowel = flip elem "aeiou"
    valid v d "" = v <= 0 && d <= 0
    valid _ _ ('a':'b':_) = False
    valid _ _ ('c':'d':_) = False
    valid _ _ ('p':'q':_) = False
    valid _ _ ('x':'y':_) = False
    valid v d (c:"") = valid v' d []
      where
        v' = v - bool 0 1 (isVowel c)
    valid v d (c:c':cs) = valid v' d' (c':cs)
      where
        v' = v - bool 0 1 (isVowel c)
        d' = bool d (d-1) (c == c')


part2 input = show . length $ filter valid (lines input)
  where
    valid s = pairs s && skip s
    pairs (_:_:_:[]) = False
    pairs (c:c':cs) | isInfixOf [c,c'] cs = True
                    | otherwise = pairs (c':cs)
    skip (_:_:[]) = False
    skip (c:c':c'':cs) | c == c''  = True
                       | otherwise = skip (c':c'':cs)
