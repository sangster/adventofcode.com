module Y2021.Day25 (parts) where

import Data.Bool (bool)
import Parser
import Util.CharMap2D

parts = ( (part1, Just "474")
        , (part2, Just "Merry Christmas!")
        , parse $ map2dParser cell
        )


part1 :: SeaFloor -> String
part1 seaFloor = show $ countMoves seaFloor


part2 :: SeaFloor -> String
part2 _ = "Merry Christmas!"


type SeaFloor = CharMap2D Cell
data Cell = East
          | South
          | Empty
          deriving (Eq, Show)


countMoves :: SeaFloor -> Int
countMoves sf = go 1 sf
  where
    go :: Int -> SeaFloor -> Int
    go n = maybe n (go $ succ n) . move


-- | Move the east-facing herd, then the south.
move :: SeaFloor -> Maybe SeaFloor
move sf = case move' East sf of
            Nothing  -> move' South sf
            Just sf' -> move' South sf' <|> Just sf'


-- | Move a single heard.
move' :: Cell -> SeaFloor -> Maybe SeaFloor
move' c sf = bool Nothing (Just sf') $ sf /= sf'
  where
    sf' = map2dMapWithCoords f sf
    f xy c' | c' == c && canMoveInto sf c (facing sf xy) = Empty
            | c' == Empty && canMoveInto sf c xy         = c
            | otherwise                                  = c'


-- | Return the MapCoord of the adjacent cucumber, from the given herd, if there
--   is one and this space is empty.
canMoveInto :: SeaFloor -> Cell -> MapCoord -> Bool
canMoveInto sf c (x,y)
    | not $ isEmpty (x,y)   = False
    | incoming East  (x',y) = True
    | incoming South (x,y') = True
    | otherwise             = False
  where
    x' = bool (x-1) (mapWidth  sf - 1) $ x == 0
    y' = bool (y-1) (mapHeight sf - 1) $ y == 0

    isEmpty xy     = map2dCell sf xy == Empty
    incoming c' xy = c == c' && map2dCell sf xy == c'


facing :: SeaFloor -> MapCoord -> MapCoord
facing sf (x,y) = case map2dCell sf (x,y) of
                    East  -> (x', y)
                    South -> (x, y')
                    _     -> error "facing: unexpected (x,y)"
  where
    x' = bool 0 (x+1) $ x < (mapWidth  sf - 1)
    y' = bool 0 (y+1) $ y < (mapHeight sf - 1)


cell :: Parser Cell
cell = symbol East  (char '>')
   <|> symbol South (char 'v')
   <|> symbol Empty (char '.')
