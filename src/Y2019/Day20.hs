--
--
-- TODO: This day was really boring. I'll refactor this code... later.
--
--
module Y2019.Day20 (parts) where

import           Data.Bool
import           Data.Char
import qualified Data.Dequeue          as DQ
import qualified Data.HashMap.Strict   as M
import qualified Data.HashSet          as S
import           Data.List
import           Data.Maybe
import qualified Data.PQueue.Prio.Min  as Q
import qualified Data.Vector           as V

import Parser hiding (queue)
import qualified Draw


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "594")
        , (part2, Just "6812")
        ]


part1 input = do
    show $ evalState shortestPath search
  where
    maze' = parseMaze input
    search = newSearch maze' (\_ (_,lev) to -> Just (to,lev)) ("AA", 0) ("ZZ", 0)


part2 input = do
    show $ evalState shortestPath search
  where
    maze' = parseMaze input
    search = newSearch maze' shifter ("AA", 0) ("ZZ", 0)
    shifter maze' (from, level) to
      | isOutside maze' from = bool Nothing (Just (to, level-1)) $ level /= 0
      | otherwise            = Just (to, level+1)


isOutside :: Maze -> Index -> Bool
isOutside m i = isOutside' $toCoord m i
  where
    isOutside' (2,_) = True
    isOutside' (_,2) = True
    isOutside' (x,y)
      | x == (width  m - 3) = True
      | y == (height m - 3) = True
      | otherwise           = False


shortestPath :: Search Distance
shortestPath = do
    st <- get
    top <- pop
    maybe (error . show $ len st) (evalNext $ goal st) $ top
  where
    evalNext goal' (i, dist) =
      bool (evaluateTile i dist) (pure dist) $ i == goal'


-- | Check if the given location is the solution to our problem.
-- If this isn't our solution, continue the search in neighboring tiles.
evaluateTile :: (Index, Level) -> Distance -> Search Distance
evaluateTile (current, lev) dist = do
    st <- get
    bool (checkTile st) shortestPath $ S.member (current, lev) (seen st)
  where
    checkTile st = bool (searchNeighbors st) (pure dist)
                 $ (current, lev) == goal st

    searchNeighbors st = do
        modify $ \s -> s{ seen = S.insert (current, lev) (seen s) }
        enqueueNeighbors (current,lev) dist
        shortestPath



-- | Add the neighbors around the given location to the search queue.
enqueueNeighbors :: (Index, Level) -> Distance -> Search ()
enqueueNeighbors (current, lev) dist = do
    modify $ \s -> s{ queue = foldr queueFold (queue s) (nexts s)
                    , len = dist
                    }
  where
    queueFold (next, d) = Q.insert (dist + d) (next, dist + d)
    nexts s = catMaybes $ (\(i,d) -> getNext s i d) <$> (nghbr s M.! current)
      where
        getNext s i d = bool (Just ((i, lev), d)) (getNext' s i d) $ d == 1
        getNext' s i d =
          case (func s) (maze s) (current, lev) i of
            Just (i', lev') -> Just ((i', lev'), d)
            Nothing         -> Nothing


-- | Extract the next location from the stack (if there is one), including the
--   distance to it.
pop :: Search (Maybe ((Index, Level), Distance))
pop = do
  st <- get
  case queueView (queue st) of
    Nothing        -> pure Nothing
    Just (res, q') -> (put $ st { queue = q' }) >> (pure $ Just res)


-- | The maze is just a random-access array of @Tile@.
-- We store the @width@ to quickly translate between indicies and X/Y
-- coordinates.
data Maze = Maze
  { tiles :: V.Vector Tile
  , width :: Int
  }


data DoorGraph = DoorGraph
  { names :: M.HashMap String [Index]
  , coord :: M.HashMap Index String
  }

allDoors dg = M.keysSet (coord dg)

adjacents dg i = delete i <$> (M.lookup i (coord dg) >>= flip M.lookup (names dg))


-- | A single space in the @Maze@.
data Tile
  = Open                -- ^ Can be visited.
  | Wall                -- ^ Can't be visited.
  | Door { dID :: ID }  -- ^ Can be visited after the key with the matching ID.
  | Newline             -- ^ The end of a row of Tiles.
  deriving (Eq, Show)


data SearchState = SearchState
  { maze  :: Maze
  , seen  :: S.HashSet (Index, Level)
  , queue :: PQueue Distance ((Index, Level), Distance)
  , len   :: Distance
  , level :: Level
  , goal  :: (Index, Level)
  , graph :: DoorGraph
  , nghbr :: NeighborGraph
  , func  :: LevelShift
  }


type Search a      = State SearchState a  -- ^ The state of a search in progress.
type ID            = Char                 -- ^ Identifies a @Door@ or its @Key@.
type Index         = Int                  -- ^ A location in the @Maze@.
type Distance      = Int
type Level         = Int
type NeighborGraph = M.HashMap Index  [(Index, Distance)]
type PQueue a      = Q.MinPQueue a
type Queue a       = DQ.BankersDequeue a
type LevelShift    = (Maze -> (Index,Level) -> Index -> Maybe (Index,Level))

queueView = Q.minView


-- | Create a new search state, starting the search at the given location.
newSearch :: Maze -> LevelShift -> (String, Level) -> (String, Level) -> SearchState
newSearch maze' func' (origin', oLev) (goal', gLev) = SearchState
    { maze  = maze'
    , seen  = S.empty
    , queue = Q.singleton 0 ((originIdx, oLev), 0)
    , len   = maxBound
    , level = 0
    , goal  = (goalIdx, gLev)
    , graph = graph'
    , nghbr = neighborGraph maze' graph'
    , func  = func'
    }
  where
    graph' = doorGraph maze'
    originIdx = head (names graph' M.! origin')
    goalIdx   = head (names graph' M.! goal')


-- | Build a map of each door, to the list of other doors it can move
--   to.
neighborGraph :: Maze -> DoorGraph -> NeighborGraph
neighborGraph maze' graph' = fst $ execState findNeighbors (M.empty, queue)
  where
    doors      = allDoors graph'
    queue      = foldl' DQ.pushBack DQ.empty (S.toList doors)
    isLocation = flip S.member doors

    findNeighbors :: State (NeighborGraph, Queue Index) ()
    findNeighbors = do
        (ngraph, q) <- get
        maybe (pure ()) (uncurry $ addNeighbor ngraph) $ DQ.popFront q

    addNeighbor ngraph current q' = put (ngraph', newQueue) >> findNeighbors
      where
        ngraph'     = M.insert current (concat [neighbors', neighbors'']) ngraph
        neighbors'  = hunt (DQ.pushBack DQ.empty (current,0)) S.empty []
        neighbors'' = map (\i -> (i,1)) adjs
        adjs        = fromJust $ adjacents graph' current

        newQueue       = foldl' DQ.pushBack q' queueNeighbors
        queueNeighbors = filter (not . flip M.member ngraph) (fst <$> neighbors')

        hunt :: Queue (Index, Distance) -> S.HashSet Index -> [(Index, Distance)] -> [(Index, Distance)]
        hunt q seen ns = step seen ns (DQ.popFront q)
        step _    ns Nothing = ns
        step seen ns (Just ((j, dist), q')) =
            bool skip continue $ not (S.member j seen)
          where
            skip = hunt q' seen ns
            continue = bool branch storeLocation
                     $ current /= j && isLocation j
            storeLocation = hunt q' (S.insert j seen) ((j, dist):ns)
            branch = hunt (foldl' (enqueue dist) q' (neighbors maze' j))
                          (S.insert j seen) ns
            enqueue dist q k = DQ.pushBack q (k, dist + 1)


parseMaze :: String -> Maze
parseMaze input = Maze
    { tiles = V.filter (/= Newline) tiles'
    , width = width'
    }
  where
    tiles' = V.fromList $ parse (some tile) input
    width' = fromJust $ V.findIndex (\t -> t == Newline) tiles'


doorGraph :: Maze -> DoorGraph
doorGraph m = DoorGraph{ names = names', coord = coord' }
  where
    names' = V.ifoldl' addDoor M.empty (tiles m)
    coord' = foldl' (\c (n, xs) -> foldl' (\c' x -> M.insert x n c') c xs) M.empty (M.toList names')

    addDoor map' i (Door d) =
        case other m x y of
          Just (i',d') -> M.insertWith (++) [d, d'] [findOpen [i, i']] map'
          Nothing      -> map'
      where
        (x,y) = toCoord m i
        findOpen xs = head $ concatMap (neighbors m) xs
    addDoor map' _ _ = map'


other m x y = otherId (x,y+1) <|> otherId (x+1,y)
  where
    otherId (x',y') = maybe Nothing (Just . ((,) i) . dID) $ maybeDoor (m `idx` i)
      where i = toIndex m (x',y')
    maybeDoor t = bool Nothing (Just t) $ isDoor t


neighbors :: Maze -> Index -> [Index]
neighbors m i =
    [ i'
    | p' <- range
    , q' <- range
    , let i' = toIndex m (p+p', q+q')
    , abs (p' - q') == 1
    , m `idx` i' == Open
    ]
  where
    (p,q) = toCoord m i
    range = [-1..1]


tile :: Parser Tile
tile
   =  (oneOf "# "  >> pure Wall)
  <|> (char  '.'   >> pure Open)
  <|> (char  '\n'  >> pure Newline)
  <|> (satisfy isAsciiUpper >>= pure . Door)


toIndex m (x,y) = y * width m + x
toCoord m i     = (i `mod` width m, i `div` width m)
idx     m i     = fromMaybe Wall $ tiles m V.!? i


isDoor (Door _) = True
isDoor _        = False


height m = (V.length $ tiles m) `div` width m
