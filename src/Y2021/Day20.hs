{-# LANGUAGE ImportQualifiedPost, NamedFieldPuns #-}
module Y2021.Day20 (parts) where

import Data.Bool
import Data.Vector qualified as V
import Parser
import Util.CharMap2D


parts = ( (part1, Just "5425")
        , (part2, Just "14052")
        , parse scanner
        )


part1 :: Image -> String
part1 img = show . countLight $ (iterate process img) !! 2


part2 :: Image -> String
part2 img = show . countLight $ (iterate process img) !! 50


data Pixel = Light | Dark deriving (Eq, Show)
type Enhancement = V.Vector Pixel
data Image = Image { imgMap :: CharMap2D Pixel
                   , bgColor :: Pixel
                   , enhancement :: Enhancement
                   } deriving Show


countLight :: Image -> Int
countLight = V.length . V.filter (== Light) . mapCells . imgMap


process :: Image -> Image
process img@Image{ imgMap } = img{ imgMap = imgMap'
                                 , bgColor = nextBgColor img
                                 }
  where
    imgMap' = CharMap2D { mapCells  = V.fromList $ outputPixel img <$> coords
                        , mapHeight = mapHeight imgMap + 2
                        , mapWidth  = mapWidth  imgMap + 2
                        }
    coords = [ (x,y)
             | y <- [-1 .. mapHeight imgMap]
             , x <- [-1 .. mapWidth  imgMap]
             ]


outputPixel :: Image -> MapCoord -> Pixel
outputPixel img@Image { enhancement } (x,y) = newPix $ toInt pixels
  where
    newPix n = enhancement V.! n
    pixels = imgGet img <$> coords
    coords = [(x+dx, y+dy) | dy <- [-1 .. 1], dx <- [-1 .. 1]]


nextBgColor :: Image -> Pixel
nextBgColor Image { enhancement, bgColor } = enhancement V.! toInt pixels
  where
    pixels = take 9 $ repeat bgColor


-- | Convert a list of Pixels into an Int (big-endian).
toInt :: [Pixel] -> Int
toInt = fst . foldr (\p (n, e) -> (n + bool 0 e (p == Light), e*2)) (0, 1)


imgGet :: Image -> MapCoord -> Pixel
imgGet img c | map2dCoordInBounds (imgMap img) c = map2dCell (imgMap img) c
             | otherwise                         = bgColor img


scanner :: Parser Image
scanner = do enh <- V.fromList <$> some pixel
             img <- string "\n\n" >> map2dParser pixel
             pure $ mkImg enh img
  where
    pixel = symbol Light (char '#') <|> symbol Dark (char '.')
    mkImg enh img = Image { imgMap = img
                          , bgColor = Dark
                          , enhancement = enh
                          }
