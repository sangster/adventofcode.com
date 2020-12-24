-- Always a handy ref: https://www.redblobgames.com/grids/hexagons
module Y2020.Day24 (parts) where

import           Data.Bool (bool)
import           Data.Foldable (foldr')
import qualified Data.HashMap.Strict as M
import           Parser


parts = ( (part1, Just "400")
        , (part2, Just "3768")
        , mapFloorTiles . parse (splitSome spaces tileLoc)
        )


part1 :: Floor -> String
part1 = show . countBlack


part2 :: Floor -> String
part2 floor = show . countBlack
            $ iterate flipAll floor !! 100


data Dir = E | SE | SW | W | NW | NE
           deriving (Bounded, Enum, Show)
type TileLoc = [Dir]
type Coords  = (Int, Int, Int)

data Floor = Floor { floorTiles :: M.HashMap Coords Bool
                   , minCoords  :: Coords
                   , maxCoords  :: Coords
                   }


fitCoords :: ([Int] -> Int) -> Coords -> Coords -> Coords
fitCoords f (x,y,z) (x',y',z') = (f [x,x'], f [y,y'], f [z,z'])


countBlack :: Floor -> Int
countBlack = M.foldr ((+) . bool 0 1) 0 . floorTiles


isBlack :: Floor -> Coords -> Bool
isBlack floor xyz = M.lookupDefault False xyz (floorTiles floor)


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


tileLoc :: Parser TileLoc
tileLoc = some dir
  where
    dir = symbol SE (string "se")
      <|> symbol SW (string "sw")
      <|> symbol NW (string "nw")
      <|> symbol NE (string "ne")
      <|> symbol E  (string "e")
      <|> symbol W  (string "w")


mapFloorTiles :: [TileLoc] -> Floor
mapFloorTiles = foldr' flipTile $ Floor M.empty (0,0,0) (0,0,0)
  where
    flipTile t f = insertWith f (const not) (move (0,0,0) t) True


move :: Coords -> TileLoc -> Coords
move from []     = from
move from (l:ls) = move (step from l) ls


step :: Coords -> Dir -> Coords
step (x,y,z) E  = (x+1, y-1, z  )
step (x,y,z) SE = (x  , y-1, z+1)
step (x,y,z) SW = (x-1, y  , z+1)
step (x,y,z) W  = (x-1, y+1, z  )
step (x,y,z) NW = (x  , y+1, z-1)
step (x,y,z) NE = (x+1, y  , z-1)


insertWith :: Floor -> (Bool -> Bool -> Bool) -> Coords -> Bool -> Floor
insertWith floor f xyz val =
    Floor{ floorTiles = M.insertWith f xyz val (floorTiles floor)
         , minCoords  = fitCoords minimum xyz (minCoords floor)
         , maxCoords  = fitCoords maximum xyz (maxCoords floor)
         }


flipAll :: Floor -> Floor
flipAll floor = foldr' flipTile floor{ floorTiles = M.empty } (allCoords floor)
  where
    flipTile xyz floor'
      | blackTile && (nBlack == 0 || nBlack > 2) = floor'
      | not blackTile && nBlack == 2             = set True
      | otherwise                                = set blackTile
      where
        blackTile = isBlack floor xyz
        nBlack    = length . filter id $ isBlack floor <$> neighbors
        neighbors = step xyz <$> [minBound..maxBound]
        set False = floor'
        set True  = insertWith floor' const xyz True
