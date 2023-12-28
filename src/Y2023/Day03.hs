module Y2023.Day03 (parts) where

import Data.Char (isDigit)
import Util.CharMap2D

parts = ( (part1, Just "528799")
        , (part2, Just "84907174")
        , buildMap
        )


part1 :: (CharMap2D Cell, [Number]) -> String
part1 (map', nums) = show . sum $ numValue <$> filter isPartNumber nums
  where
    neighbors num = filter (map2dCoordInBounds map')
                  $ [ (x',y')
                    | x' <- [pred x .. x + numLength num]
                    , y' <- [pred y .. succ y]
                    ]
      where
        (x, y) = numCoord num
    isPartNumber = any (isSymbol . map2dCell map') . neighbors


part2 :: (CharMap2D Cell, [Number]) -> String
part2 (map', nums) = show . sum . map snd
                   $ foldr gearPowers [] (map2dCoords map')
  where
    gearPowers xy acc | notGear xy        = acc
                      | length nums' == 2 = (xy, product nums') : acc
                      | otherwise         = acc
      where nums' = adjacentNumbers xy
    notGear xy = map2dCell map' xy /= Symbol '*'
    adjacentNumbers (x,y) = find' nums
      where
        find' [] = []
        find' (n:ns) | y' > succ y          = []   -- too low
                     | y' < pred y          = rest -- too high
                     | x' > succ x          = rest -- too right
                     | x' + numLength n < x = rest -- too left
                     | otherwise            = numValue n : rest
          where
            (x',y') = numCoord n
            rest = find' ns


data Cell = Space | Digit Int | Symbol Char deriving Eq

data Number = Number { numCoord  :: MapCoord
                     , numLength :: Int
                     , numValue  :: Int
                     } deriving Show


buildMap :: String -> (CharMap2D Cell, [Number])
buildMap input = (map', nums)
  where
    map' = map2dParse charToCell input
    nums = find' (0, 0)
    find' (x, y) | x >= mapWidth  map' = find' (0, succ y)
                 | y >= mapHeight map' = []
                 | otherwise = case cell of
                                 Digit _ -> num : find' (x + numLength num, y)
                                 _       -> find' (succ x, y)
      where
        cell = map2dCell map' (x, y)
        num = Number { numCoord  = (x, y)
                     , numLength = length digits
                     , numValue  = foldl (\acc n -> acc * 10 + n) 0 digits
                     }
          where
            digits = findDigits x
            findDigits x' | x' >= mapWidth map' = []
                          | otherwise = case map2dCell map' (x', y) of
                                          Digit n -> n : findDigits (succ x')
                                          _       -> []


charToCell :: Char -> Cell
charToCell '.' = Space
charToCell n | isDigit n = Digit $ charToDigit n
             | otherwise = Symbol n


isSymbol :: Cell -> Bool
isSymbol (Symbol _) = True
isSymbol _          = False
