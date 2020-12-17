module Y2020.Day17 (parts) where

import           Data.Bool (bool)
import           Data.Maybe (fromJust)
import           Parser
import qualified Data.HashSet as S


parts = ( (part1, Just "380")
        , (part2, Just "2332")
        , parse grid2D
        )


part1 :: Grid -> String
part1 grid = show . S.size . active $ gridSeries !! 6
 where
   gridSeries = iterate cycleGrid grid3D
   grid3D     = extrude 3 grid


part2 :: Grid -> String
part2 grid = show . S.size . active $ gridSeries !! 6
 where
   gridSeries = iterate cycleGrid grid4D
   grid4D     = extrude 4 grid


type Coords = [Int]
data Grid = Grid { active :: S.HashSet Coords
                 , dimensions :: Int
                 , mins :: Coords
                 , maxs :: Coords
                 } deriving Show


grid2D :: Parser Grid
grid2D = do rows <- splitSome (char '\n') row
            pure $ Grid { active = S.fromList . filterActive $ mkCubes rows
                        , dimensions = 2
                        , mins  = [0,0]
                        , maxs  = [length (head rows) - 1, length rows - 1]
                        }
  where
    row = many cube
    cube = symbol False (char '.') <|> symbol True (char '#')
    mkCubes rows = concat $ mkEntries <$> ([0..] `zip` rows)
    mkEntries (y,row) = (\(x,cube) -> ([x,y], cube)) <$> ([0..] `zip` row)
    filterActive [] = []
    filterActive ((coords, True):xs) = coords : filterActive xs
    filterActive (_:xs) = filterActive xs


extrude :: Int -> Grid -> Grid
extrude d g = Grid { active = S.map (++ newDims) (active g)
                   , dimensions = d
                   , mins = (mins g) ++ newDims
                   , maxs = (maxs g) ++ newDims
                   }
  where
    newDims = take (d - dimensions g) (repeat 0)


neighbors :: Coords -> [Coords]
neighbors coords = filter (/= coords) $ neighbors' coords
  where
    neighbors' (x:[]) = [[x-1], [x], [x+1]]
    neighbors' (x:xs) = concat $ (\i -> (i:) <$> rest) <$> [x-1, x, x+1]
      where
        rest = neighbors' xs


cycleGrid :: Grid -> Grid
cycleGrid g@Grid{ active = act } = g{ active = act'
                                    , mins = fromJust <$> min''
                                    , maxs = fromJust <$> max''
                                    }
  where
    (act', min'', max'') = foldr foldNeighbors initialState (allCoords g)
    initialState = (S.empty, bounds, bounds)
    bounds = take (dimensions g) (repeat Nothing)

    foldNeighbors coords prev@(act', min', max')
      | S.member coords act = bool prev added $ nAct == 2 || nAct == 3
      | otherwise           = bool prev added $ nAct == 3
      where
        added = (S.insert coords act', min'', max'')
        nAct  = length . filter (flip S.member act) $ neighbors coords
        min'' = uncurry (testMaybe (<)) <$> min' `zip` coords
        max'' = uncurry (testMaybe (>)) <$> max' `zip` coords


allCoords :: Grid -> [Coords]
allCoords g = coords $ (mins g) `zip` (maxs g)
  where
    coords ((min',max'):[]) = (:[]) <$> [min'-1 .. max'+1]
    coords ((min',max'):xs) = foldr prepend [] [min'-1 .. max'+1]
      where
        prepend i xs' = foldr (\r x -> (i:r):x) xs' (coords xs)


-- | If the first param is Nothing, return the second; otherwise, compare them
-- using the given function, returning the second if true, the first if false.
testMaybe :: (a -> a -> Bool)
          -> Maybe a
          -> a
          -> Maybe a
testMaybe func (Just x) y = Just $ bool x y (func y x)
testMaybe _    Nothing  y = Just y
