module Y2019.Day17 (parts) where

import Data.Char
import qualified Draw
import Data.Maybe
import Data.List
import Data.Bool

import qualified Data.Vector as V

import Util.InstructionSet
import Util.Program


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "5740")
        , (part2, Nothing)
        ]


data Map = Map { tiles :: V.Vector Tile
               , width :: Int
               }

instance Show Map where
    show m = tail . concat $ uncurry show' <$> (coords m)
      where
        pairs = [(x,y) | y <- [0 .. (height m) - 1], x <- [0.. (width m) - 1]]
        show' :: Int -> Int -> String
        show' 0 y = '\n':(tile 0 y)
        show' x y = tile x y
        tile x y = bool (show t) ("x") (isIntersection m (x, y)) where t = m ! (x,y)


coords m = [(x,y) | y <- [0 .. (height m) - 1], x <- [0.. (width m) - 1]]


isIntersection m (x,y)
    | x == 0              = False
    | y == 0              = False
    | x == (width m)  - 1 = False
    | y == (height m) - 1 = False
    | otherwise           = all isInt [(0,0), (-1,0), (0,-1), (1,0), (0,1)]
  where
    isInt (x',y') = t == Scaffold where t = m ! (x+x', y+y')


alignment = uncurry (*)


m ! (x,y) = (tiles m) V.! ((y * width m) + x)

height :: Map -> Int
height m = V.length (tiles m) `div` width m


data Tile = Open | Scaffold | Bot  deriving Eq

instance Show Tile where
    show Open     = Draw.dot
    show Scaffold = Draw.lightShade
    show Bot      = Draw.avatar


readTile '.' = Open
readTile '#' = Scaffold
readTile '^' = Bot
readTile 'v' = Bot
readTile '<' = Bot
readTile '>' = Bot
readTile c   = error $ "unknown char " ++ [c]


part1 input = do m <- parseMap input

                 return . show . sum . map alignment
                   $ filter (isIntersection m) (coords m)



parseMap :: String -> IO Map
parseMap input = do dat <- (program input >>= (flip executeUntilHalt) [])
                    let cc = chr <$> dat
                    return $ Map { tiles = V.fromList (readTile <$> (filter (/= '\n') cc))
                                 , width = fromJust $ findIndex (== '\n') cc
                                 }




part2 input = return $ ""



program :: String -> IO Program'
program = fmap (Program aoc19Set) . parseRAM
