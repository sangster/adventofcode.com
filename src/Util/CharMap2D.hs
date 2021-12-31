{-# LANGUAGE GADTs, NamedFieldPuns #-}

module Util.CharMap2D
    ( CharMap2D(..)
    , MapCoord
    , map2dSize
    , map2dCoords
    , map2dCoordToIndex
    , map2dIndexToCoord
    , map2dCoordInBounds
    , map2dCell
    , map2dCell'
    , map2dNeighbors
    , map2dNeighbors'
    , map2dNeighborsDiag
    , map2dNeighborsDiag'
    , map2dParse
    , map2dParseDigits
    , charToDigit
    , map2dMap
    , map2dMapWithCoords
    , map2dParser
    ) where

import           Data.Char  (ord)
import           Data.Bool  (bool)
import           Data.List  (findIndex, intercalate)
import           Data.Maybe (catMaybes, fromJust)
import qualified Data.Vector as V
import           Parser


data CharMap2D a where
  CharMap2D :: { mapCells :: V.Vector a
               , mapHeight :: Int
               , mapWidth  :: Int
               }
            -> CharMap2D a


type MapCoord = (Int, Int)


instance Show a => Show (CharMap2D a) where
  show m = intercalate "\n" [row y | y <- [0 .. mapHeight m - 1]]
    where
      row y = concat [show $ map2dCell m (x,y) | x <- [0 .. mapWidth m - 1]]


map2dSize :: CharMap2D a -> Int
map2dSize = V.length . mapCells


-- | Every Index in the OctopusMap.
map2dCoords :: CharMap2D a -> [MapCoord]
map2dCoords m = [ (x,y)
              | y <- [0 .. mapHeight m - 1]
              , x <- [0 .. mapWidth  m - 1]
              ]


map2dCoordToIndex :: CharMap2D a -> MapCoord -> Int
map2dCoordToIndex m (x,y) = mapWidth m * y + x


map2dIndexToCoord :: CharMap2D a -> Int -> MapCoord
map2dIndexToCoord m i = (i `mod` mapWidth m, i `div` mapWidth m)


map2dCoordInBounds :: CharMap2D a -> MapCoord -> Bool
map2dCoordInBounds CharMap2D { mapHeight, mapWidth } (x,y)
    | x < 0 || x >= mapWidth  = False
    | y < 0 || y >= mapHeight = False
    | otherwise               = True


-- | The height at the given Index.
map2dCell :: CharMap2D a -> MapCoord -> a
map2dCell sm (x, y) = mapCells sm V.! (mapWidth sm * y + x)


map2dCell' :: CharMap2D a -> MapCoord -> Maybe a
map2dCell' sm (x, y) = mapCells sm V.!? (mapWidth sm * y + x)


-- | In-bound Indicies around the given Index, including diagonals.
map2dNeighborsDiag :: CharMap2D a -> MapCoord -> [MapCoord]
map2dNeighborsDiag sm xy = map2dNeighbors sm xy
                        ++ neighborsF neighborDiagIndices sm xy


-- | Indicies around the given Index, including diagonals.
map2dNeighborsDiag' :: MapCoord -> [MapCoord]
map2dNeighborsDiag' xy = map2dNeighbors' xy
                      ++ neighborsF' neighborDiagIndices xy


neighborDiagIndices :: [(Int, Int)]
neighborDiagIndices = [ (-1, -1)
                      , (-1,  1)
                      , ( 1, -1)
                      , ( 1,  1)
                      ]


-- | In-bound Indicies around the given Index, including diagonals.
map2dNeighbors :: CharMap2D a -> MapCoord -> [MapCoord]
map2dNeighbors = neighborsF neighborIndices


-- | Indicies around the given Index, including diagonals.
map2dNeighbors' :: MapCoord -> [MapCoord]
map2dNeighbors' = neighborsF' neighborIndices


neighborIndices :: [(Int, Int)]
neighborIndices = [ (-1,  0)
                  , ( 1,  0)
                  , ( 0, -1)
                  , ( 0,  1)
                  ]


neighborsF :: [(Int, Int)]
           -> CharMap2D a
           -> MapCoord
           -> [MapCoord]
neighborsF ds sm (x, y) = catMaybes
                        $ (\(dx,dy) -> neighbor (x+dx) (y+dy)) <$> ds
  where
    neighbor x' y' = bool Nothing (Just (x', y'))
                   $ inBounds mapWidth x' && inBounds mapHeight y'
      where
        inBounds f n = n >= 0 && n < f sm


neighborsF' :: [(Int, Int)] -> MapCoord -> [MapCoord]
neighborsF' ds (x, y) = (\(dx,dy) -> ((x+dx), (y+dy))) <$> ds


map2dParse :: (Char -> a)
         -> String
         -> CharMap2D a
map2dParse f input = CharMap2D { mapCells = cells', mapHeight = h, mapWidth = w }
  where
    w = fromJust $ findIndex (== '\n') input
    h = length cells' `div` w

    cells' = V.fromList . map f . concat
           $ parse (splitSome (char '\n') (many (noneOf " \n\r"))) input


map2dParser :: Show a => Parser a -> Parser (CharMap2D a)
map2dParser cell = do rows <- splitSome (char '\n') (some cell)
                      pure CharMap2D{ mapCells  = V.fromList $ concat rows
                                    , mapHeight = length rows
                                    , mapWidth  = length $ head rows
                                    }


map2dParseDigits :: String -> CharMap2D Int
map2dParseDigits = map2dParse charToDigit


charToDigit :: Char -> Int
charToDigit c = ord c - ord '0'


map2dMap :: (a -> b) -> CharMap2D a -> CharMap2D b
map2dMap f m = m{ mapCells = V.map f $ mapCells m }


map2dMapWithCoords :: (MapCoord -> a -> b) -> CharMap2D a -> CharMap2D b
map2dMapWithCoords f m = m{ mapCells = V.imap f' $ mapCells m }
  where
    f' i v = f (map2dIndexToCoord m i) v
