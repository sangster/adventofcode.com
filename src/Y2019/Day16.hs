module Y2019.Day16 (parts) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import Parser


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "61149209")
        , (part2, Just "16178430")
        ]


type Nums = U.Vector Int


part1 input = return $ extractMessage fftPhase 8 100 nums
 where
   nums = U.fromList $ parse (some digit') input


fftPhase :: Nums -> Nums
fftPhase nums = U.imap nextDigit nums
  where
    nextDigit col _ =
      (abs $ U.ifoldl' (sumProd col) 0 (U.drop col nums)) `mod` 10

    sumProd col acc i n = acc + n * multiple (col+1) (col+i)


part2 input = return
            $ extractMessage (G.scanr' ((flip mod 10 .) . (+)) 0) 8 100
            $ U.unsafeDrop (offset 7)
            $ longNums 10000
 where
   longNums n = U.fromList $ take (n * length nums) $ cycle nums
   nums       = parse (some digit') input
   offset   n = read . concat . map show . take n $ nums


extractMessage :: (Nums -> Nums) -> Int -> Int -> Nums -> String
extractMessage f length iterations nums
    = concat . map show
    $ U.toList . U.take length
    $ iterateLast f (iterations + 1)
    $ nums


iterateLast :: (a -> a) -> Int -> a -> a
iterateLast _ 1     init = init
iterateLast f count init = iterateLast f (count - 1) (f $! init)


pattern 0 = 0
pattern 1 = 1
pattern 2 = 0
pattern 3 = (-1)


-- | Repeat every element in @pattern@ @n@ times, and drop the resulting @head@.
multiple spread n = pattern $ (n+1) `div` spread `mod` 4
