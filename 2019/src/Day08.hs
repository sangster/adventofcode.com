module Day08 (parts) where

import Data.Bool      (bool)
import Data.Function  (on)
import Data.List      (minimumBy)

import Util.Parser


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "2684")
        , (part2, Just part2Expected)
        ]

part2Expected = unlines [ "█   █ ██  ███  █   █████ "
                        , "█   ██  █ █  █ █   █   █ "
                        , " █ █ █    █  █  █ █   █  "
                        , "  █  █ ██ ███    █   █   "
                        , "  █  █  █ █ █    █  █    "
                        , "  █   ███ █  █   █  ████ "
                        ]


part1 input = return . show $ count White smallest * count Trans smallest
  where
    layers'     = layers (parse colors input) 25 6
    smallest    = minimumBy (compare `on` count Black) layers'
    count c lay = sum [bool 0 1 (c == c') | row <- lay, c' <- row]


part2 input = return $ show' compiled
  where
    layers'  = layers (parse colors input) 25 6
    compiled = compile layers'
    show' l  = unlines $ [concatMap show row | row <- l]


type Layer = [[Color]]
data Color = Black
           | White
           | Trans
  deriving Eq


instance Show Color where
    show Black = " "
    show White = "█"
    show _     = "?"


colors :: Parser [Color]
colors = do { ds <- many digit; spaces; return $ color <$> ds }
  where
    color '0' = Black
    color '1' = White
    color _   = Trans


compile :: [Layer] -> Layer
compile [layer]  = layer
compile (lay:ls) = compile' lay $ compile ls
  where
    compile'   a b = uncurry compileRow <$> zip a b
    compileRow a b = uncurry render     <$> zip a b
    render Trans b = b
    render a     _ = a


layers :: [Color] -> Int -> Int -> [Layer]
layers colors w h = group h $ group w colors
  where
    group _ [] = []
    group n xs = take n xs : group n (drop n xs)
