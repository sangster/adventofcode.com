--
-- TODO: This sucks and needs refactoring.
--
module Y2020.Day20 (parts) where

import           Data.Bool (bool)
import           Data.Hashable (Hashable, hashWithSalt)
import qualified Data.HashMap.Strict as M
import           Data.List (delete, find)
import qualified Data.Vector.Unboxed as U
import           Parser


parts = ( (part1, Just "66020135789767")
        , (part2, Just "1537")
        , parse $ splitSome whitespace tile
        )


part1 :: [Tile] -> String
part1 tiles = show $ foldr (\t acc -> acc * tileId t) 1 cornerTiles
  where
    cornerTiles = (map' M.!) <$> corners
    (w, h, map') = tileGrid tiles
    corners = [ (0  , 0  )
              , (w-1, 0  )
              , (0  , h-1)
              , (w-1, h-1)
              ]


part2 tiles = show $ numTrue - (countMonsters bigMap * monsterSize)
  where
    (w, h, map') = tileGrid tiles
    bigMap = compileTiles 1 w h map'
    numTrue = U.foldr (\b acc -> bool acc (succ acc) b) 0 (mapElems bigMap)
    monsterSize = length $ findCoords 0 0 monster



countMonsters :: UnboxedMap Bool
              -> Int
countMonsters map' = foldr count 0 translations -- countInstances isMonster <$> translations
  where
    count m acc = bool acc (countInstances isMonster m) (acc == 0)
    isMonster = mkMonsterPredicate monster
    translations = take 4 (iterate mapRotate map')


countInstances :: U.Unbox a
               => (UnboxedMap a -> (Int,Int) -> Bool)
               -> UnboxedMap a
               -> Int
countInstances func map' = foldr addMonster 0 (mapCoords map')
  where
    addMonster xy acc | func map' xy = succ acc
                      | otherwise    = acc


mkMonsterPredicate :: String
                   -> (UnboxedMap Bool -> (Int,Int) -> Bool)
mkMonsterPredicate str = (\m xy -> all isSet $ lookupAll m xy)
  where
    coords = findCoords 0 0 str
    offsetCoords (x,y) = (\(x',y') -> (x+x', y+y')) <$> coords
    lookupAll m xy = mapLookup m <$> offsetCoords xy
    isSet (Just True) = True
    isSet _           = False

findCoords _ _ [] = []
findCoords x y ('\n':cs) = findCoords 0 (y+1) cs
findCoords x y ('#':cs)  = (x,y) : findCoords (x+1) y cs
findCoords x y (_:cs)    = findCoords (x+1) y cs


-- | Combine all the tiles in the given map into a single @UnboxedMap@, first
--   removing a border around the edge of each tile.
compileTiles :: Int
             -> Int
             -> Int
             -> M.HashMap (Int,Int) Tile
             -> UnboxedMap Bool
compileTiles border w h map' = UnboxedMap{ mapElems  = elems'
                                         , mapWidth  = totalW
                                         , mapHeight = totalH
                                         }
  where
    totalW = w * (tileW - border*2)
    totalH = h * (tileH - border*2)
    tileW = (mapWidth  . tileMap $ map' M.! (0,0))
    tileH = (mapHeight . tileMap $ map' M.! (0,0))

    elems' = foldr addPixels (U.replicate (totalW*totalH) False) coords
    coords = [ (x,y) | x <- [0..w-1], y <- [0..h-1] ]

    addPixels (x,y) acc = foldr translatePixel acc tCoords
      where
        tMap = tileMap $ map' M.! (x,y)
        tCoords = [ (x,y)
                  | x <- [border..tileW-border-1]
                  , y <- [border..tileH-border-1]
                  ]
        translatePixel (tX,tY) acc' = acc' U.// [(idx, pixel)]
          where
            pY    = y * tileH + tY - (border*y*2 + border)
            pX    = x * tileW + tX - (border*x*2 + border)
            idx   = totalW * pY + pX
            pixel = mapGet tMap (tX,tY)


tileGrid :: [Tile] -> (Int, Int, M.HashMap (Int,Int) Tile)
tileGrid ts = (w+1, h+1, map')
  where
    (w,h) = foldr (\(x,y) (maxX, maxY) -> (maximum [x, maxX], maximum [y, maxY])) (0,0) $ M.keys map'
    map' = M.fromList $ toXYElems 0 arranged
    arranged = tileRow ts East <$> westCol
    start = findCorner (West, North) ts
    westCol = tileRow ts South start

    toXYElems y [] = []
    toXYElems y (row:rows) = (mkCoords <$> [0..] `zip` row) ++ rest
      where
        mkCoords (x,t) = ((x,y), t)
        rest = toXYElems (y+1) rows


tileRow :: [Tile] -> Side -> Tile -> [Tile]
tileRow ts dir = build
  where
    build t = case findAdjacent ts t dir of
                Nothing -> [t]
                Just t' -> t : build t'


-- Find any corner.
findCorner :: (Side, Side) -> [Tile] -> Tile
findCorner (x,y) (t:ts) = findEdge ts t' y
  where t' = findEdge ts t x


findEdge :: [Tile] -> Tile -> Side -> Tile
findEdge ts t dir = case findAdjacent ts t dir of
                      Nothing -> t
                      Just t' -> findEdge ts t' dir


findAdjacent :: [Tile] -> Tile -> Side -> Maybe Tile
findAdjacent ts t dir = foldr (findSideTile dir) Nothing ts
  where
    findSideTile _ _ (Just x) = Just x
    findSideTile s t' Nothing = find (\t'' -> t /= t'' && sameEdge t s t'' (opposite s)) translations
      where
        translations = concat [ take 4 (iterate tileRotate t')
                              , take 4 (iterate tileRotate $ tileFlipVer t')
                              ]


buildMap :: [Tile] -> M.HashMap (Int, Int) Tile
buildMap tiles = M.empty


findConnections :: [Tile] -> M.HashMap Tile (M.HashMap Side (Maybe Tile))
findConnections [] = M.empty
findConnections (t:ts) = M.union (M.singleton t sides) rest
  where
    (sides, ts') = foldr insertSide (M.empty, ts) allSides
    rest = findConnections ts
    insertSide s (m, ts') = (M.insert s match m, maybe ts' (flip delete ts') match)
      where
        match = findSide s ts'

    findSide s ts' = foldr (findSideTile s) Nothing ts'
    findSideTile _ _ (Just x) = Just x
    findSideTile s t' Nothing = find (\t' -> sameEdge t s t' (opposite s)) translations
      where
        translations = concat [ take 4 (iterate tileRotate t')
                              , take 4 (iterate tileRotate $ tileFlipVer t')
                              ]


backFillConnections :: M.HashMap Tile (M.HashMap Side (Maybe Tile))
                    -> M.HashMap Tile (M.HashMap Side (Maybe Tile))
backFillConnections map' = M.foldrWithKey addReverse map' map'
  where
    addReverse t sideMap m = M.foldrWithKey mkReversePath m sideMap
      where
        mkReversePath _ Nothing acc = acc
        mkReversePath s (Just t') acc = M.insert t' otherSideMap acc
          where
            otherSideMap = M.insert (opposite s) (Just t) (M.lookupDefault M.empty t' acc)


monster = unlines [ "                  # "
                  , "#    ##    ##    ###"
                  , " #  #  #  #  #  #   "
                  ]


data Tile = Tile { tileId :: Int
                 , tileMap :: UnboxedMap Bool
                 , tileSides :: M.HashMap Side [Bool]
                 }

data UnboxedMap a = UnboxedMap { mapElems  :: U.Vector a
                               , mapWidth  :: Int
                               , mapHeight :: Int
                               } deriving Show


data Side = North | East | South | West deriving (Eq, Enum, Show)


instance Eq Tile where
  Tile{ tileId = a } == Tile{ tileId = b } = a == b

instance Hashable Tile where
  hashWithSalt salt = hashWithSalt salt . tileId

instance Hashable Side where
  hashWithSalt salt = hashWithSalt salt . fromEnum


instance Show Tile where
  show t = "Tile "++show (tileId t)++"\n"++ map'
  -- show t = "#"++show (tileId t)
    where
      map' = showMap (tileMap t)


showMap :: UnboxedMap Bool
        -> String
showMap m = unlines $ mkRow <$> [0..h-1]
  where
    h = mapHeight m
    w = mapWidth  m
    mkRow y = concat $ (\x -> draw $ mapGet m (x,y)) <$> [0..w-1]
    draw True  = "#"
    draw False = "."


tile :: Parser Tile
tile = do string "Tile "
          name <- natural
          string ":\n"

          map' <- unboxedMap isBlack
          pure $ Tile { tileId = name
                      , tileMap = map'
                      , tileSides = allMapSides map'
                      }
  where
    isBlack = symbol True (char '#') <|> symbol False (char '.')


unboxedMap :: (Show a, U.Unbox a)
           => Parser a
           -> Parser (UnboxedMap a)
unboxedMap p = do line' <- line <$> get
                  elems' <- splitSome (char '\n') (many p)
                  line'' <- line <$> get

                  pure $ UnboxedMap { mapElems = U.fromList (concat elems')
                                    , mapWidth = length $ head elems'
                                    , mapHeight = line'' - line' - 1
                                    }

mapTranslate :: U.Unbox a
             => ((Int,Int) -> (Int,Int))
             -> UnboxedMap a
             -> UnboxedMap a
mapTranslate f m = m{ mapElems = (mapElems m) U.// foldr translate [] xys }
  where
    xys = mapCoords m
    translate xy acc = (mapIndex m $ f xy, mapGet m xy) : acc


mapRotate  m = mapTranslate (\(x,y) -> (mapHeight m - y - 1, x)) m
mapFlipHor m = mapTranslate (\(x,y) -> (mapWidth m - x - 1, y)) m
mapFlipVer m = mapTranslate (\(x,y) -> (x, mapHeight m - y - 1)) m


tileRotate t  = t{ tileMap = mapRotate  (tileMap t) }
tileFlipHor t = t{ tileMap = mapFlipHor (tileMap t) }
tileFlipVer t = t{ tileMap = mapFlipVer (tileMap t) }


mapGet :: U.Unbox a
       => UnboxedMap a
       -> (Int, Int)
       -> a
mapGet m xy = mapElems m U.! mapIndex m xy


mapLookup :: U.Unbox a
          => UnboxedMap a
          -> (Int, Int)
          -> Maybe a
mapLookup m xy = mapElems m U.!? mapIndex m xy


mapCoords :: U.Unbox a
          => UnboxedMap a
          -> [(Int, Int)]
mapCoords m = [ (x,y)
              | x <- [0 .. mapWidth  m - 1]
              , y <- [0 .. mapHeight m - 1]
              ]


mapIndex :: UnboxedMap a
         -> (Int, Int)
         -> Int
mapIndex m (x,y) = mapWidth m * y + x


allMapSides :: U.Unbox a
            => UnboxedMap a
            -> M.HashMap Side [a]
allMapSides m = M.fromList $ (\s -> (s, mapSide m s)) <$> allSides


allSides = [North, East, South, West]


mapSide :: U.Unbox a
        => UnboxedMap a
        -> Side
        -> [a]
mapSide m s | s == North = [mapGet m (x , 0 ) | x <- [0 .. x']]
            | s == East  = [mapGet m (x', y ) | y <- [0 .. y']]
            | s == South = [mapGet m (x , y') | x <- [0 .. x']]
            | s == West  = [mapGet m (0 , y ) | y <- [0 .. y']]
  where
    x' = mapWidth  m - 1
    y' = mapHeight m - 1


opposite :: Side -> Side
opposite North = South
opposite East  = West
opposite South = North
opposite West  = East


sameEdge aTile aSide bTile bSide = a == b
  where
    a = mapSide (tileMap aTile) aSide
    b = mapSide (tileMap bTile) bSide
