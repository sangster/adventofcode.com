module Y2020.Day03 (parts) where

import           Data.Bool
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U


parts = ( (part1, Just "232")
        , (part2, Just "3952291680")
        , parseMap
        )


part1 :: Map -> String
part1 map' = show $ countTrees map' 3 1


part2 :: Map -> String
part2 map' = show $ foldr (*) 1
                  $ uncurry (countTrees map') <$> runs
  where
    runs = [ (1, 1)
           , (3, 1)
           , (5, 1)
           , (7, 1)
           , (1, 2)
           ]


type Map = V.Vector (U.Vector Bool)


parseMap :: String -> Map
parseMap = V.unfoldr colGen
  where
    colGen [] = Nothing
    colGen xs = Just (row, drop (U.length row + 1) xs)
      where
        row = U.unfoldr rowGen xs
    rowGen [] = Nothing
    rowGen ('\n':_) = Nothing
    rowGen ('#':xs) = Just (True, xs)
    rowGen (_:xs) = Just (False, xs)


countTrees :: Map -> Int -> Int -> Int
countTrees map dx dy = count 0 0 0
  where
    count seen x y
      | y >= (V.length map) = seen
      | otherwise           = count seen' (x+dx) (y+dy)
      where
        seen' = bool seen (seen+1) $ isTreeAt map x y


isTreeAt :: Map -> Int -> Int -> Bool
isTreeAt map x y = row U.! (x `mod` U.length row)
  where row = map V.! y
