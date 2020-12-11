module Y2020.Day11 (parts) where

import           Data.Bool     (bool)
import           Data.List     (findIndex)
import           Data.Default  (Default, def)
import           Data.Maybe    (catMaybes, fromJust)
import qualified Data.Vector   as V


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "2494")
        , (part2, Just "2306")
        ]


part1 input = show . countOccupied $ findStableSeating ferry
  where
    ferry = (parseFerry input){ neighborTolerance = 4
                              , neighborCoords    = adjacentCoords
                              }


part2 input = show . countOccupied $ findStableSeating ferry
  where
    ferry = (parseFerry input) { neighborTolerance = 5
                               , neighborCoords    = visibleCoords
                               }


data Seat = Missing | Empty | Occupied deriving Eq

data Ferry = Ferry { width :: Int
                   , height :: Int
                   , seating :: V.Vector Seat
                   , neighborTolerance :: Int
                   , neighborCoords :: Ferry -> (Int, Int) -> [(Int, Int)]
                   }


instance Default Ferry where
  def = Ferry { width   = 0
              , height  = 0
              , seating = V.empty
              , neighborTolerance = 0
              , neighborCoords    = const . const []
              }

instance Eq Ferry where
  a == b = seating a == seating b


parseFerry :: String -> Ferry
parseFerry input = def{ width = w, height = h, seating = seats }
  where
    w = fromJust $ findIndex (== '\n') input
    h = V.length seats `div` w
    seats = V.fromList $ seatList input

    seatList []       = []
    seatList ('L':xs) = Empty   : seatList xs
    seatList ('.':xs) = Missing : seatList xs
    seatList (_:xs)   = seatList xs


countOccupied Ferry{ seating = s } = V.length $ V.filter (== Occupied) s


findStableSeating :: Ferry -> Ferry
findStableSeating f = case nextGeneration f of
                        Nothing -> f
                        Just f' -> findStableSeating f'


nextGeneration :: Ferry -> Maybe Ferry
nextGeneration f@Ferry{ seating = seats, neighborTolerance = nt } =
    bool Nothing (Just f') $ f /= f'
  where
    f' = f{ seating = V.imap nextGen seats }
    nextGen i Missing  = Missing
    nextGen i Empty    = bool Occupied Empty $ occupiedNeighbors f i > 0
    nextGen i Occupied = bool Occupied Empty $ occupiedNeighbors f i >= nt


occupiedNeighbors :: Ferry -> Int -> Int
occupiedNeighbors f@Ferry{ neighborCoords = coords } idx =
    foldr addSeat 0 $ coords f (x,y)
  where
    (y,x) = idx `divMod` width f
    addSeat xy sum = bool sum (sum+1) (seatGet f xy == Occupied)


seatGet :: Ferry -> (Int, Int) -> Seat
seatGet f (x,y) = seating f V.! (y * width f + x)


adjacentCoords :: Ferry -> (Int, Int) -> [(Int, Int)]
adjacentCoords f (x,y) = filter (inBounds f) options
  where
    options = (\(fx, fy) -> (fx x, fy y)) <$> unitCoords


visibleCoords :: Ferry -> (Int, Int) -> [(Int, Int)]
visibleCoords f (x,y) = filter (inBounds f) options
  where
    options = catMaybes $ findSeat (x,y) <$> unitCoords
    findSeat (x',y') (fx, fy)
      | not $ inBounds f (x'', y'')     = Nothing
      | seatGet f (x'', y'') == Missing = findSeat (x'', y'') (fx, fy)
      | otherwise                       = Just (x'', y'')
      where
        (x'', y'') = (fx x', fy y')


unitCoords :: [(Int -> Int, Int -> Int)]
unitCoords = [ (pred, pred), (id, pred), (succ, pred)
             , (pred, id  ),             (succ, id  )
             , (pred, succ), (id, succ), (succ, succ)
             ]


inBounds :: Ferry -> (Int, Int) -> Bool
inBounds Ferry{ width = w, height = h } (x,y)
  | x <  0 || y <  0 = False
  | x >= w || y >= h = False
  | otherwise        = True
