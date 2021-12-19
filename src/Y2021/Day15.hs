module Y2021.Day15 (parts) where

import           Control.Monad.State.Strict
import           Data.Bool (bool)
import qualified Data.HashSet          as S
import qualified Data.PQueue.Prio.Min  as Q
import qualified Data.Vector as V
import           Util.CharMap2D

parts = ( (part1, Just "441")
        , (part2, Just "2849")
        , map2dParseDigits
        )


part1 :: CharMap2D Int -> String
part1 m = show $ evalState shortestPath (newSearch m)


-- TODO: slow
part2 :: CharMap2D Int -> String
part2 m = show $ evalState shortestPath (newSearch $ growMap 5 m)


growMap :: Int -> CharMap2D Int -> CharMap2D Int
growMap n m = CharMap2D { mapCells  = V.fromList cells
                        , mapHeight = h'
                        , mapWidth  = w'
                        }
  where
    cells = [cellVal x y | y <- [0 .. h' - 1] , x <- [0 .. w' - 1]]
    w  = mapWidth  m
    h  = mapHeight m
    w' = w * 5
    h' = h * 5

    cellVal x y = mod1 $ (orig + (x `div` w) + (y `div` h))
      where
        orig = map2dCell m (x `mod` w, y `mod` h)
        mod1 :: Int -> Int
        mod1 n = bool n (n `mod` 10 + 1) $ n > 9


type Risk = Int
type Search a = State SearchState a  -- ^ The state of a search in progress.
type PQueue a = Q.MinPQueue a


data SearchState = SearchState
  { map2d :: CharMap2D Int
  , seen  :: S.HashSet MapCoord
  , queue :: PQueue Risk (MapCoord, Risk)
  , risk  :: Risk
  , goal  :: MapCoord
  }


newSearch :: CharMap2D Int -> SearchState
newSearch m = SearchState
    { map2d = m
    , seen  = S.empty
    , queue = Q.singleton 0 ((0,0), 0)
    , risk  = maxBound
    , goal  = (mapWidth m - 1, mapHeight m - 1)
    }


shortestPath :: Search Risk
shortestPath = do
    st  <- get
    top <- pop
    case top of
      Nothing   -> pure $ risk st
      Just next -> evalNext (goal st) next

  where
    evalNext goal' (xy, r) =
      bool (evaluateCell xy r) (pure r) $ xy == goal'


-- | Extract the next location from the stack (if there is one), including its
--   risk.
pop :: Search (Maybe (MapCoord, Risk))
pop = do
  st <- get
  case Q.minView (queue st) of
    Nothing        -> pure Nothing
    Just (res, q') -> (put $ st{ queue = q' }) >> (pure $ Just res)


-- | Check if the given location is the goal and if has less risk than previous
--   solutions. If this isn't our solution, continue the search in neighboring
--   tiles.
evaluateCell :: MapCoord -> Risk -> Search Risk
evaluateCell current r = do
    st <- get
    bool (checkTile st) shortestPath $ S.member current (seen st)
  where
    checkTile st = if current == goal st
                   then put st{ risk = minimum [r, risk st] } >> pure r
                   else searchNeighbors st

    searchNeighbors st = do
        modify $ \s -> s{ seen = S.insert current (seen s) }
        enqueueNeighbors current (risk st) r
        shortestPath


-- | Add the neighbors around the given location to the search queue.
enqueueNeighbors :: MapCoord -> Risk -> Risk -> Search ()
enqueueNeighbors current best' r =
    modify $ \s -> s{ queue = foldr queueFold (queue s) (nexts s) }
  where
    queueFold (next, r') = Q.insert (r + r') (next, r + r')

    nexts s = filter (\(_, r') -> r' < best')
            $ (\xy -> (xy, map2dCell (map2d s) xy))
          <$> map2dNeighbors (map2d s) current
