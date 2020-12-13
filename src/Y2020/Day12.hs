module Y2020.Day12 (parts) where

import Data.Bool (bool)
import Parser


parts = ( (part1, Just "362")
        , (part2, Just "29895")
        , parse (splitSome spaces nav)
        )


part1 :: [Nav] -> String
part1 navs = show . manhattanDistance . fst
           $ foldl doNav ((0,0), East) navs
  where
    doNav (xy,    b) (Turn d  ) = (xy, rotateDeg d b)
    doNav ((x,y), b) (Move d n) = (doMove d, b)
      where
        doMove East    = (x+n, y  )
        doMove South   = (x  , y-n)
        doMove West    = (x-n, y  )
        doMove North   = (x  , y+n)
        doMove Forward = doMove b


part2 :: [Nav] -> String
part2 navs = show . manhattanDistance . fst
           $ foldl doNav ((0,0), (10,1)) navs
  where
    doNav (xy,     wxy     ) (Turn d        ) = (xy, rotateWaypointDeg d wxy)
    doNav ((x, y), (wx, wy)) (Move Forward n) = ((x+wx*n, y+wy*n), (wx, wy))
    doNav (xy,     (wx, wy)) (Move d       n) = (xy, moveWaypoint d)
      where
        (wx', wy') = moveWaypoint d

        moveWaypoint East    = (wx+n, wy  )
        moveWaypoint South   = (wx  , wy-n)
        moveWaypoint West    = (wx-n, wy  )
        moveWaypoint North   = (wx  , wy+n)
        moveWaypoint _       = (wx  , wy  )


data Nav = Move Dir Int
         | Turn Int
         deriving (Eq, Show)


data Dir = East
         | South
         | West
         | North
         | Forward
         deriving (Bounded, Enum, Eq, Show)


nav :: Parser Nav
nav = move <|> turn


move :: Parser Nav
move = do d <- dir
          n <- natural
          pure $ Move d n
  where
    dir = (symbol East    $ char 'E')
      <|> (symbol South   $ char 'S')
      <|> (symbol West    $ char 'W')
      <|> (symbol North   $ char 'N')
      <|> (symbol Forward $ char 'F')


turn :: Parser Nav
turn = do r <- isRight
          n <- natural
          pure . Turn $ bool (360-n) n r
  where
    isRight = (symbol False $ char 'L')
          <|> (symbol True  $ char 'R')


rotateDeg :: Int -> Dir -> Dir
rotateDeg 0   dir = dir
rotateDeg deg dir = rotateDeg (deg-90) (rotate dir)
  where
    rotate North = East
    rotate d     = succ d


rotateWaypointDeg :: Int -> (Int, Int) -> (Int, Int)
rotateWaypointDeg 0 xy = xy
rotateWaypointDeg d xy = rotateWaypointDeg (d-90) (rotate xy)
  where
    rotate (x,y) = (y, (x * (-1)))


manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y
