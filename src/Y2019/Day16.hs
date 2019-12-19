{-# LANGUAGE BangPatterns #-}
module Y2019.Day16 (parts) where

import Parser
import Debug.Trace

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

type Nums = U.Vector Int


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "61149209")
        , (part2, Just "16178430")
        ]


part1 input = return -- . show
            $ concat . map show
            $ U.toList
            $ U.take 8
            $ extractPhrase 100 nums
 where
   nums = U.fromList $ parse (some digit') input


part _ = return ""
part2 input = return
            $ concat . fmap show
            $ U.toList
            $ U.take 8
            $ extractPhrase 100
            $ U.unsafeDrop (offset 7)
            $ longNums 10000
 where
   longNums n = U.fromList $ take (n * length nums) $ cycle nums
   nums       = parse (some digit') input
   offset   n = read . concat . fmap show . take n $ nums :: Int


extractPhrase :: Int -> Nums -> Nums
extractPhrase' n nums = last
                     $ take (n+1)
                     $ iterate fftPhase nums
extractPhrase n nums = iterateLast fftPhase (n+1) nums


iterateLast :: (a -> a) -> Int -> a -> a
iterateLast' f c i = trace ("iterateLast f "++show c) (iterateLast' f c i)

iterateLast _ 1      init = init
iterateLast f count init = iterateLast f (count - 1) (f $! init)



fftPhase' n = trace ("\n\nfftPhase "++show (U.length n)) $ fftPhase' n

fftPhase :: Nums -> Nums
fftPhase nums = G.iscanr' scanner 0 nums
  where scanner idx num acc = (num + acc) `mod` 10


pattern 0 = 0
pattern 1 = 1
pattern 2 = 0
pattern 3 = (-1)

-- | Repeat every element in @pattern@ @n@ times, and drop the resulting @head@.
multiple spread n = pattern $ (n+1) `div` spread `mod` 4
