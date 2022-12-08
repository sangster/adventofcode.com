module Y2020.Day25 (parts) where

import Data.List
import Data.Maybe
import GHC.Natural (powModNatural)


parts = ( (part1, Just "18862163")
        , (part2, Just "Merry Christmas!")
        , map read . lines :: String -> [Int]
        )


part1 :: [Int] -> String
part1 publicKeys = show $ powModInt (last publicKeys) loopSize ringSize
 where
   loopSize = findLoopSize $ head publicKeys


part2 :: a -> String
part2 _ = "Merry Christmas!"


ringSize = 20201227


findLoopSize key = fst . fromJust $ find ((key ==) . snd) candidates
  where
    candidates = [1..] `zip` iterate (transform 7) val
    val = powModInt 7 1 ringSize


transform :: Int -> Int -> Int
transform subject val = (val * subject) `mod` ringSize


powModInt :: Int -> Int -> Int -> Int
powModInt b e m = fromIntegral
                $ powModNatural (fromIntegral b) (fromIntegral e) (fromIntegral m)
