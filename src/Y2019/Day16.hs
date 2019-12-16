{-# LANGUAGE BangPatterns #-}
module Y2019.Day16 (parts) where

import Parser
import Debug.Trace

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B

type Nums = U.Vector Int


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "61149209")
        , (part2, Nothing)
        ]


part1 input = return -- . show
            $ concat . map show
            $ U.toList
            $ U.take 8
            $ extractPhrase 1 nums
 where
   nums = U.fromList $ parse (some digit') input


part' _ = return ""
part input = return
            $ concat . fmap show
            $ U.toList
            $ U.take 8
            $ U.drop (offset 7)
            $ extractPhrase 100
            $ longNums 10000
 where
   longNums n = U.fromList $ take (n * length nums) $ cycle nums
   nums       = parse (some digit') input
   offset   n = read . concat . fmap show . take n $ nums :: Int


-- part2 input = return . show
--             $ multiple 5 <$> [0 .. 40]

part2 input = return -- . show
            $ concat . map show
            $ U.toList
            $ U.take 8
            $ extractPhraseRecursrive 1 nums
 where
   nums = U.fromList $ parse (some digit') input


extractPhrase :: Int -> Nums -> Nums
extractPhrase1 n nums = last
                     $ take (n+1)
                     $ iterate fftPhase nums
extractPhrase n !nums = iterateLast fftPhase (n+1) nums


iterateLast :: (a -> a) -> Int -> a -> a
iterateLast' f c i = trace ("iterateLast f "++show c) (iterateLast' f c i)

iterateLast _ 1      init = init
iterateLast f count !init = iterateLast f (count - 1) (f $! init)



fftPhase' n = trace ("\n\nfftPhase "++show n) $ fftPhase' n

fftPhase :: Nums -> Nums
fftPhase nums = U.imap newColumn nums
  where
    newColumn :: Int -> Int -> Int
    newColumn' i n = trace ("newColumn "++show i++" "++show n++" = "++show r) r where r = (newColumn' i n)
    newColumn col num = (abs $ U.ifoldl' (sumProd col) 0 (U.drop col nums)) `mod` 10

    sumProd :: Int -> Int -> Int -> Int -> Int
    -- sumProd' col acc idx num = trace ("sumProd "++show col++" "++show acc++" "++show idx++" "++show num++" = "++show r) r
    --   where r = sumProd' col acc idx num

    sumProd c a i n = trace ("sumProd:  col "++show c++", "++show i++"  "++show n++" x "++show (multiple (c+1) (c+i)))
                            (sumProd' c a i n)
    sumProd' col acc idx num = acc + (num * multiple (col+1) (col+idx))


-- fftPhase :: Int -> Nums -> Nums
-- fftPhase len nums = U.convert $ B.map ones sets
--   where
--     sets = B.map (\n -> U.imap (\i val -> (val, multiple n i)) nums) (B.fromList [1 .. len])
--     ones = flip mod 10 . sumProd
--     sumProd pairs = abs $ U.foldr' (\(a,b) sum -> sum + (a*b)) 0 pairs
extractPhraseRecursrive n !nums = iterateLast fftPhaseRecursive (n+1) nums

fftPhaseRecursive :: Nums -> Nums
fftPhaseRecursive nums = U.imap newColumn nums
  where
    newColumn :: Int -> Int -> Int
    newColumn col num = abs $ (descend col) `mod` 10
      where
        descend !index
            | index >= count = 0
            | otherwise      = moo amount (descend $! (index + spread))

          where
            moo !a b = a + b
            amount = sum [digitProduct (index + i) | i <- [0 .. spread - 1]]
        digitProduct i = trace ("digitProduct:  col "++show col++", "++show i++"  "++show num++" x "++show (multiple (col+1) (col+i)))
                            (digitProduct' i)
        digitProduct' idx = num * multiple spread (idx)
        spread           = col + 1
    count = U.length nums

--   where

-- TODO: recurse through with accumulator. pass index to facilitate skipping patterns 0/2
-- TODO: prepend 0 to input list to avoid having to skip head of pattern
-- TODO: index past paterns 0 / 2
-- TODO: can patterns 1 / 3 sometimes cancel out?
pattern 0 = 0
pattern 1 = 1
pattern 2 = 0
pattern 3 = (-1)


-- | Repeat every element in @pattern@ @n@ times, and drop the resulting @head@.
multiple' s n = trace ("                           multiple "++show s++" "++show n++" = "++show r) r where r = multiple' s n

multiple spread n = pattern  idx
  where
    idx = (n+1) `div` spread `mod` 4 -- U.length pattern
