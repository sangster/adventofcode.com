module Y2020.Day15 (parts) where

import           Control.Monad (forM_)
import           Control.Monad.ST (ST(..), runST)
import           Data.Bool (bool)
import           Data.Foldable (foldlM)
import           Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed.Mutable as V


parts = ( (part1, Just "1696")
        , (part2, Just "37385")
        , map (read :: String -> Int) . splitOn ","
        )


part1 :: [Int] -> String
part1 starting = show $ runST $ playGame starting 2020


part2 :: [Int] -> String
part2 starting = show $ runST $ playGame starting 30000000


playGame :: [Int] -> Int -> ST s Int
playGame starting n = do
    map' <- V.new n
    forM_ (init starting `zip` [1..]) (uncurry $ V.write map')
    foldlM (turn map') (last starting) [length starting+1 .. n]
  where
    turn map' prev i = do
        prevSpoke <- V.read map' prev
        V.write map' prev (i-1)
        pure $ bool 0 (i - prevSpoke - 1) $ prevSpoke /= 0
