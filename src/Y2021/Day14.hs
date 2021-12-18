module Y2021.Day14 (parts) where

import Parser
import Data.Char
import qualified Data.HashMap.Strict as M

parts = ( (part1, Just "2375")
        , (part2, Just "1976896901756")
        , parse instructions
        )


part1 :: (Template, Rules) -> String
part1 (template, rules) = show $ max' - min'
  where
    (min', max') = minMaxIterate template rules 10


part2 :: (Template, Rules) -> String
part2 (template, rules) = show $ max' - min'
  where
    (min', max') = minMaxIterate template rules 40


-- | Iterate over the given template a number of times, then return the number
--   of elements in the Polymer for the Elements appearing the least, and the
--   most.
minMaxIterate :: Template -> Rules -> Iterations -> (Int, Int)
minMaxIterate tmpl rules = M.foldl minmax (maxBound, minBound)
                         . countIterate rules tmpl
  where
    minmax :: (Int, Int) -> Int -> (Int, Int)
    minmax (a, z) n = (minimum [a,n], maximum [n, z])


type Template = [Element]
type Element  = Char
type Pair     = (Element, Element)
type Rules    = M.HashMap Pair Element
type CountMap = M.HashMap Element Int

-- | Maps a pair of Elements, and a number of iterations, with the number of
--   Elements created at that level. This functions as a cache, to avoid
--   repeating when encountering the same pair at the same level multiple times.
type IterationMap = M.HashMap (Iterations, Pair) CountMap
type Iterations   = Int


-- | Iterate over the given template a number of times, then return a map of
--   each Element to the number of time it occurs in the resulting polymer.
countIterate :: Rules -> Template -> Iterations -> CountMap
countIterate rules tmpl iterations = foldr addElem createdElemsCount tmpl
  where
    createdElemsCount = foldr1 (M.unionWith (+))
                      $ (snd . depthFirst iterations M.empty) <$> (pairs tmpl)

    addElem e = M.insertWith (+) e 1

    pairs :: [a] -> [(a,a)]
    pairs (x:x':xs) = (x,x') : pairs (x':xs)
    pairs _         = []

    -- | Count the number of Elements that are created Elements after a number
    --   of iterations. This recursive function operates depth first, processing
    --   the left-most pairs until the number of iterations are exhausted,
    --   before moving to the right. IterationMap is pass throughout, to cache
    --   the number of elements created by each Pair at a given iteration.
    depthFirst :: Iterations -> IterationMap -> Pair -> (IterationMap, CountMap)
    depthFirst 0 acc _     = (acc, M.empty)
    depthFirst i acc (l,r) = case acc M.!? key of
                               Just ec -> (acc, ec)
                               Nothing -> (M.insert key rest accRight, rest)
      where
        m    = rules M.! (l,r)
        key  = (i, (l,r))
        rest = addElem m $ M.unionWith (+) left right

        (accLeft,  left)  = depthFirst (i-1) acc     (l,m)
        (accRight, right) = depthFirst (i-1) accLeft (m,r)


instructions :: Parser (Template, Rules)
instructions = do tmpl  <- some element
                  rules <- string "\n\n" >> splitSome (char '\n') rule
                  pure (tmpl, M.fromList rules)
  where
    rule = do a <- element
              b <- element
              c <- string " -> " >> element
              pure ((a,b), c)
    element = satisfy isAsciiUpper
