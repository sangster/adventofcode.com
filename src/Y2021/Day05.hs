module Y2021.Day05 (parts) where

import           Data.Bool (bool)
import qualified Data.HashMap.Strict as M
import           Parser

parts = ( (part1, Just "7473")
        , (part2, Just "24164")
        , parse $ splitSome (char '\n') ventLine
        )


part1 :: [VentLine] -> String
part1 lines = show . M.size . M.filter (>=2) $ intersectionMap nondiag
  where
    nondiag = filter (not . isDiagonal) lines


part2 :: [VentLine] -> String
part2 lines = show . M.size . M.filter (>=2) $ intersectionMap lines


type X = Int
type Y = Int
type Point = (X, Y)
type VentLine = (Point, Point)


-- | Parse a single vent, in the format of: x1,y1 -> x2,y2
ventLine :: Parser VentLine
ventLine = do { p1 <- point; string " -> "; p2 <- point; pure (p1, p2) }
  where
    point = do { x <- natural; char ','; y <- natural; pure (x, y) }


isDiagonal :: VentLine -> Bool
isDiagonal ((x1, y1), (x2, y2)) | x1 == x2  = False
                                | y1 == y2  = False
                                | otherwise = True


linePoints :: VentLine -> [Point]
linePoints ((x1, y1), (x2, y2)) = points x1 y1
  where
    dx  = signum $ x2 - x1
    dy  = signum $ y2 - y1
    points x y | x == x2 && y == y2 = [(x,y)]
               | otherwise          = (x,y) : points (x+dx) (y+dy)


-- | Return a Map of every Point to the number of lines which intersect it.
intersectionMap :: [VentLine] -> M.HashMap Point Int
intersectionMap []     = M.empty
intersectionMap (v:vs) = foldr (\p m -> M.insertWith (+) p 1 m) rest points
  where
    points = linePoints v
    rest   = intersectionMap vs
