module Day10 (parts) where

import Data.Bool
import Data.List
import Data.Function
import Util.Parser
import Debug.Trace
import Safe

parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "340")
        , (part2, Just "2628")
        ]


part1 input = return . show . length $ visible rocks best
  where best  = findBest rocks
        rocks = parse (some asteroid) input


part2 input = return . show $ code (destroyPos 200)
  where destroyPos i = sorted !! (i - 1)
        code (x,y) = x * 100 + y
        sorted = sortBy (compare `on` laserBearing best) $ visible rocks best
        best   = findBest rocks
        rocks  = parse (some asteroid) input


type X = Int
type Y = Int
type Asteroid = (X, Y)


asteroid :: Parser Asteroid
asteroid = do { void; st <- get; char '#'; void; return (col st, line st) }
  where void = many $ oneOf ".\n"


findBest :: [Asteroid] -> Asteroid
findBest rocks = maximumBy (compare `on` (length . visible rocks)) rocks


-- | Return the list of asteroids that are visible from the given asteroid.
visible :: [Asteroid] -> Asteroid -> [Asteroid]
visible as a = as \\ nub obs
  where obs = a:(as >>= obscured w h a)
        w   = maximum $ fst <$> as
        h   = maximum $ snd <$> as


-- | Return the list of coordinates that are obscured by @b@ when viewed from
-- @a@.
obscured :: Int -> Int -> Asteroid -> Asteroid -> [(X, Y)]
obscured w h a b@(x, y) | a == b             = []
                        | dx == 0 && dy == 0 = []
                        | dx == 0            = zip (repeat x) yrange
                        | dy == 0            = zip xrange     (repeat y)
                        | otherwise          = zip xrange     yrange
  where
    xrange    = tailSafe [x, x + dx .. (bool 0 w $ dx > 0)]
    yrange    = tailSafe [y, y + dy .. (bool 0 h $ dy > 0)]
    (dx, dy)  = angle a b


angle :: Asteroid -> Asteroid -> (X, Y)
angle (x, y) (x', y') = shrink (x' - x) (y' - y)
  where shrink x y = (x `div` gcd x y, y `div` gcd x y)


-- | Return the "bearing" (the clockwise angle, in radians, relative to "north')
-- of one asteroid relative to the other.
laserBearing :: Asteroid -> Asteroid -> Double
laserBearing (x,y) (x',y') = (theta `on` fromIntegral) (y - y') (x' - x)
  where theta     y x = bool a (a + (2 * pi)) (a < 0) where a = flipAngle $ atan2 y x
        flipAngle a   = pi / 2 - a
