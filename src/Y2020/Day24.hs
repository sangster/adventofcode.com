-- Always a handy ref: https://www.redblobgames.com/grids/hexagons
module Y2020.Day24 (parts) where

import           Data.Bool (bool)
import           Data.Foldable (foldr')
import qualified Data.HashSet as S
import           Parser


parts = ( (part1, Just "400")
        , (part2, Just "3768")
        , mapFloorTiles . parse (splitSome whitespace path)
        )


part1 :: Floor -> String
part1 = show . countBlack


part2 :: Floor -> String
part2 floor = show . countBlack
            $ iterate flipAll floor !! 100


data Dir    = E | SE | SW | W | NW | NE deriving (Bounded, Enum, Show)
type Path   = [Dir]
type Coords = (Int, Int, Int)
data Floor  = Floor{ floorTiles :: S.HashSet Coords
                   , minCoords  :: Coords
                   , maxCoords  :: Coords
                   }


path :: Parser Path
path = some dir
  where
    dir = symbol SE (string "se")
      <|> symbol SW (string "sw")
      <|> symbol NW (string "nw")
      <|> symbol NE (string "ne")
      <|> symbol E  (string "e")
      <|> symbol W  (string "w")


mapFloorTiles :: [Path] -> Floor
mapFloorTiles = foldr' flipTile $ Floor S.empty (0,0,0) (0,0,0)
  where
    flipTile t f = toggle f (foldl step (0,0,0) t)


step :: Coords -> Dir -> Coords
step (x,y,z) E  = (x+1, y-1, z  )
step (x,y,z) SE = (x  , y-1, z+1)
step (x,y,z) SW = (x-1, y  , z+1)
step (x,y,z) W  = (x-1, y+1, z  )
step (x,y,z) NW = (x  , y+1, z-1)
step (x,y,z) NE = (x+1, y  , z-1)


countBlack :: Floor -> Int
countBlack = S.size . floorTiles


flipAll :: Floor -> Floor
flipAll floor = foldr' flip floor{ floorTiles = S.empty } (allCoords floor)
  where
    flip xyz floor'
      | blackTile && (nBlack == 0 || nBlack > 2) = set False
      | not blackTile && nBlack == 2             = set True
      | otherwise                                = set blackTile
      where
        blackTile = isBlack floor xyz
        nBlack    = length . filter id $ isBlack floor <$> neighbors
        neighbors = step xyz <$> [minBound..maxBound]

        set False = floor'
        set True  = toggle floor' xyz


isBlack :: Floor -> Coords -> Bool
isBlack = flip S.member . floorTiles


allCoords :: Floor -> [Coords]
allCoords f = [ (x,y,z)
              | x <- [minX-1 .. maxX+1]
              , y <- [minY-1 .. maxY+1]
              , z <- [minZ-1 .. maxZ+1]
              , x+y+z == 0
              ]
  where
    (minX,minY,minZ) = minCoords f
    (maxX,maxY,maxZ) = maxCoords f

toggle :: Floor -> Coords -> Floor
toggle floor xyz =
    Floor{ floorTiles = (bool S.insert S.delete blackTile) xyz tiles
         , minCoords  = select minimum xyz (minCoords floor)
         , maxCoords  = select maximum xyz (maxCoords floor)
         }
  where
    blackTile   = isBlack floor xyz
    tiles       = floorTiles floor
    select f (x,y,z) (x',y',z') = (f [x,x'], f [y,y'], f [z,z'])
