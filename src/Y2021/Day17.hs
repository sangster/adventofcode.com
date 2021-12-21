module Y2021.Day17 (parts) where

import Data.Maybe (catMaybes)
import Parser


parts = ( (part1, Just "25200")
        , (part2, Just "3012")
        , parse targetArea
        )


part1 :: Target -> String
part1 target = show . maximum $ findHeightsOfHits target


part2 :: Target -> String
part2 target = show . length $ findHeightsOfHits target


type X        = Int
type Y        = Int
type Coord    = (X, Y)
type Velocity = (X, Y)
type Target   = (Coord, Coord) -- ^ Top-left and bottom-right Corners


findHeightsOfHits :: Target -> [Y]
findHeightsOfHits target@(_, (tx', ty')) = catMaybes $ canHit <$> velocities
  where
    velocities = [ (dx,dy)
                 | dy <- [ty' .. negate ty']
                 , dx <- [1 .. tx']
                 ]
    canHit v = case fire target v of
                 Just  y -> Just y
                 Nothing -> Nothing


fire :: Target -> Velocity -> Maybe Y
fire = move (0,0) 0
  where
    move :: Coord -> Y -> Target -> Velocity -> Maybe Y
    move xy maxY t@((tx, _), (tx', ty')) v
        | isInside t (x,y)  = Just maxY'
        | x > tx'           = Nothing
        | y < ty'           = Nothing
        | dx == 0 && x < tx = Nothing
        | otherwise         = move (x, y) maxY' t (dx, dy)
      where
        ((dx, dy), (x,y)) = step v xy
        maxY' = maximum [y, maxY]


isInside :: Target -> Coord -> Bool
isInside ((tx, ty), (tx', ty')) (x, y) | x < tx || x > tx' = False
                                       | y > ty || y < ty' = False
                                       | otherwise         = True


-- | The new Velocity and Coord after a single step.
step :: Velocity -> Coord -> (Velocity, Coord)
step (dx, dy) (x, y) = (v, c)
  where
    v = (dx + negate (signum dx), dy - 1)
    c = (x + dx, y + dy)


targetArea :: Parser Target
targetArea = do (x, x') <- string "target area: x=" >> coords
                (y, y') <- string ", y="            >> coords
                pure ( (minimum [x, x'], maximum [y, y'])
                     , (maximum [x, x'], minimum [y, y'])
                     )
  where
    coords = do a <- number
                b <- string ".." >> number
                pure (a,b)
