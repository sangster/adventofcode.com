module Y2021.Day13 (parts) where

import Data.Bool (bool)
import Data.List (intercalate, nub, unlines)
import Parser


parts = ( (part1, Just "788")
        , (part2, Just $ intercalate "\n" part2Expected)
        , parse manualPage
        )

part2Expected = [ "█  █   ██ ███  █  █ ████ █  █ ███   ██ "
                , "█ █     █ █  █ █ █  █    █  █ █  █ █  █"
                , "██      █ ███  ██   ███  █  █ ███  █   "
                , "█ █     █ █  █ █ █  █    █  █ █  █ █ ██"
                , "█ █  █  █ █  █ █ █  █    █  █ █  █ █  █"
                , "█  █  ██  ███  █  █ ████  ██  ███   ███"
                ]

part1 :: ([Point], [Fold]) -> String
part1 (points, folds) = show . length . nub $ foldAt points $ head folds


part2 :: ([Point], [Fold]) -> String
part2 (points, folds) = draw $ foldAll points folds
  where
    foldAll ps []     = ps
    foldAll ps (f:fs) = foldAll (foldAt ps f) fs


data Fold = Horizontal Int
          | Vertical   Int
          deriving Show

type Point = (X, Y)
type X = Int
type Y = Int


manualPage :: Parser ([Point], [Fold])
manualPage = do { ps <- points; fs <- (char '\n' >> folds); pure (ps,fs) }
  where
    points = splitSome (char '\n') point
    point  = do { x <- natural; y <- (char ',' >> natural); pure (x,y) }
    folds  = splitSome (char '\n') fold
    fold   = (string "fold along y=" >> natural >>= pure . Horizontal)
         <|> (string "fold along x=" >> natural >>= pure . Vertical)


-- | Translate every point at the given Fold.
foldAt :: [Point] -> Fold -> [Point]
foldAt ps (Vertical   v) = [(bool x (v - (x - v)) $ x > v, y) | (x,y) <- ps]
foldAt ps (Horizontal h) = [(x, bool y (h - (y - h)) $ y > h) | (x,y) <- ps]


-- | Return a string showing a "dot" at each point.
draw :: [Point] -> String
draw ps = intercalate "\n" [ row y | y <- [0..h]]
  where
    row y = [ bool ' ' '█' (elem (x,y) ps) | x <- [0..w]]
    (w, h) = foldr1 (\(x,y) (w,h) -> (maximum [x,w], maximum [y,h])) ps
