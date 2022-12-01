module Y2019.Day18 (parts) where

import           Data.Bool
import           Data.Char
import qualified Deque.Lazy            as DQ
import qualified Data.HashMap.Strict   as M
import qualified Data.HashSet          as S
import           Data.List
import           Data.Maybe
import qualified Data.PQueue.Prio.Min  as Q
import qualified Data.Vector           as V

import Parser hiding (queue)


parts = ( (part1, Just "5392")
        , (part2, Just "1684")
        , parseMaze
        )


part1 :: Maze -> String
part1 maze' = show $ evalState findShortestLengthToAllKeys search
  where
    search = newSearch maze' $ head (origins maze')


part2 :: Maze -> String
part2 maze' = show . sum $ singleBotMinLen <$> origins maze''
  where
    singleBotMinLen = evalState findShortestLengthToAllKeys . newSearch maze''
    maze'' = mogrifyMaze maze'


data SearchState = SearchState
  { maze  :: Maze
  , seen  :: S.HashSet (Index, KeySet) -- ^ Index/Keys pairs already visited.
  , queue :: Q.MinPQueue Distance (Index, KeySet, Distance)
  , len   :: Distance -- ^ The shortest distance that found all keys (so far).
  , count :: Int -- ^ How many keys we need to find in total (stored for speed).
  , graph :: NeighborGraph
  }


-- | The maze is just a random-access array of @Tile@.
-- We store the @width@ to quickly translate between indicies and X/Y
-- coordinates.
data Maze = Maze
  { tiles :: V.Vector Tile
  , width :: Int
  }


-- | A single space in the @Maze@.
data Tile
  = Origin              -- ^ Where the bot starts in the Maze.
  | Open                -- ^ Can be visited.
  | Wall                -- ^ Can't be visited.
  | Key  { kId :: ID }  -- ^ Unlocks the Door with the matching ID.
  | Door { dID :: ID }  -- ^ Can be visited after the key with the matching ID.
  | Newline             -- ^ The end of a row of Tiles.
  deriving (Eq, Show)


type Search a      = State SearchState a  -- ^ The state of a search in progress.
type Index         = Int                  -- ^ A location in the @Maze@.
type ID            = Char                 -- ^ Identifies a @Door@ or its @Key@.
type Queue a       = DQ.Deque a
type NeighborGraph = M.HashMap Index [(Index, Distance)]
type KeySet        = [Index] -- ^ A list of key locations in the @Maze@.
type Distance      = Int


-- | The list of keys which cannot be reached from the given location.
--
-- These keys must be in a part of the maze occupied by a different bot.
unreachableKeys :: Maze -> NeighborGraph -> Index -> KeySet
unreachableKeys maze' graph' start =
    filter (\i -> not $ elem i reachableKeys) targetIndicies
  where
    targetIndicies = keyIndicies maze'
    reachableKeys  = search S.empty (DQ.snoc start mempty) []

    search :: S.HashSet Index -> Queue Index -> KeySet -> KeySet
    search seen q acc =
      case DQ.uncons q of
        Nothing     -> acc
        Just (i,q') ->
          let
            neighbors    = fst <$> graph' M.! i
            newNeighbors = filter (not . flip S.member seen) neighbors
            q''          = foldl' (flip DQ.snoc) q' newNeighbors
          in
            if i `elem` targetIndicies && not (i `S.member` seen)
              then search (S.insert i seen) q'' (i:acc)
              else search (S.insert i seen) q'' (acc)


-- | Modify the given maze by replacing the origin and surrounding tiles.
--
-- @#@
-- ###
-- @#@
mogrifyMaze :: Maze -> Maze
mogrifyMaze maze' =
    maze' { tiles = (tiles maze') V.// (uncurry coords <$> changes) }
  where
    originalOrigin = head $ origins maze'
    changes = [ ((-1, -1), Origin), ((0, -1), Wall), ((1, -1), Origin)
              , ((-1,  0), Wall  ), ((0,  0), Wall), ((1,  0), Wall  )
              , ((-1,  1), Origin), ((0,  1), Wall), ((1,  1), Origin)
              ]
    coords c t = (toIndex maze' (addCoord c), t)
    addCoord (x',y') = (x+x', y+y')
      where
        (x,y) = toCoord maze' originalOrigin


-- | Create a new search state, starting the search at the given location.
newSearch :: Maze -> Index -> SearchState
newSearch maze' origin' = SearchState
    { maze  = maze'
    , seen  = S.empty
    , queue = Q.singleton 0 (origin', keys', 0)
    , len   = maxBound
    , count = length $ keyIndicies maze'
    , graph = graph'
    }
  where
    graph' = neighborGraph maze'
    keys'  = unreachableKeys maze' graph' origin'


-- | Build a map of each "important location", to the list of other important
--   locations it can move to without passing through an intermediate "important
--   place."
--
-- Important places are things like keys, doors, and the starting locations.
-- Each key an index in @Maze@ and each element is an Index/Distance pair.
neighborGraph :: Maze -> NeighborGraph
neighborGraph maze' = fst $ execState findNeighbors (M.empty, queue)
  where
    queue      = foldl' (flip DQ.snoc) mempty (origins maze')
    isLocation = flip notElem [Wall, Open]

    findNeighbors :: State (NeighborGraph, Queue Index) ()
    findNeighbors = do
        (ngraph, q) <- get
        maybe (pure ()) (uncurry $ addNeighbor ngraph) $ DQ.uncons q

    addNeighbor ngraph current q' = put (newGraph, newQueue) >> findNeighbors
      where
        newGraph     = M.insert current neighbors ngraph
        neighbors    = hunt (DQ.snoc (current,0) mempty) S.empty []

        newQueue     = foldl' (flip DQ.snoc) q' newNeighbors
        newNeighbors = filter (not . flip M.member ngraph) (fst <$> neighbors)

        hunt :: Queue (Index, Distance) -> S.HashSet Index -> [(Index, Distance)] -> [(Index, Distance)]
        hunt q seen neighbors = step seen neighbors (DQ.uncons q)
        step _    neighbors Nothing = neighbors
        step seen neighbors (Just ((j, dist), q')) =
            bool skip continue $ not (S.member j seen)
          where
            skip = hunt q' seen neighbors
            continue = bool branch storeLocation
                     $ current /= j && isLocation (maze' `idx` j)
            storeLocation = hunt q' (S.insert j seen) ((j, dist):neighbors)
            branch = hunt (foldl' (enqueue dist) q' nonWalls)
                          (S.insert j seen) neighbors
            enqueue dist q k = DQ.snoc (k, dist + 1) q
            nonWalls = filter ((Wall /=) . (tiles maze' V.!))
                     $ neighborIndicies maze' j


-- | Find all remaining keys in the search state from its current location.
findShortestLengthToAllKeys :: Search Distance
findShortestLengthToAllKeys = do
    st <- get
    top <- pop
    maybe (pure $ len st) (evalNext $ count st) $ top
  where
    evalNext goal (i, keys, dist) =
      bool (evaluateTile i keys dist) (pure $ dist - 1) $ length keys == goal


-- | Check if the given location and keys are the solution to our problem.
-- If this isn't our solution, continue the search in neighboring tiles.
evaluateTile :: Index -> KeySet -> Distance -> Search Distance
evaluateTile current keys dist = do
    st <- get
    bool (checkTile st) skipToNext $ S.member (current, sort keys) (seen st)
  where
    skipToNext = findShortestLengthToAllKeys
    moreKeys   = nub . sort $ current:keys

    checkTile st = bool (searchNeighbors st keys') (pure dist)
                 $ length keys' == count st
      where
        keys' = bool keys moreKeys $ isKey (maze st `idx` current)

    searchNeighbors st keys' = do
        modify $ \s -> s{ seen = S.insert (current, keys') (seen s) }
        bool (enqueueNeighbors current keys' dist) (pure ()) $ isLocked
        skipToNext
      where
        isLocked  = isDoor (maze st `idx` current) && not (elem keyIdx keys)
        keyNeeded = keyFor $ (maze st) `idx` current
        keyIdx    = fromJust $ V.findIndex (== keyNeeded) (tiles (maze st))


-- | Extract the next location from the stack (if there is one), including the
--   keys available at that time, and the distance to it.
pop :: Search (Maybe (Index, KeySet, Distance))
pop = do
  st <- get
  case Q.minView (queue st) of
    Nothing        -> pure Nothing
    Just (res, q') -> (put $ st { queue = q' }) >> (pure $ Just res)


-- | Add the neighbors around the given location to the search queue.
enqueueNeighbors :: Index -> KeySet -> Distance -> Search ()
enqueueNeighbors current keys dist = do
    modify $ \s -> s{ queue = foldr queueFold (queue s) $ graph s M.! current
                    , len = dist
                    }
  where
    queueFold (next, d) = Q.insert (dist + d) (next, keys, dist + d)


-- | All in-bounds locations around the given location.
neighborIndicies :: Maze -> Index -> [Index]
neighborIndicies m i =
    filter inBounds $ toIndex m <$> (addCoord <$> [(0,1), (0,-1), (1,0), (-1,0)])
  where
    addCoord (x',y') = (x+x', y+y') where (x,y) = toCoord m i
    inBounds j | j <  0                  = False
               | j >= V.length (tiles m) = False
               | otherwise               = True


parseMaze :: String -> Maze
parseMaze input = Maze
    { tiles = V.filter (/= Newline) tiles
    , width = width
    }
  where
    tiles = V.fromList $ parse (some tile) input
    width = fromJust $ V.findIndex (\t -> t == Newline) tiles


tile :: Parser Tile
tile
   =  (char '#'  >> pure Wall)
  <|> (char '.'  >> pure Open)
  <|> (char '@'  >> pure Origin)
  <|> (char '\n' >> pure Newline)
  <|> (satisfy isAsciiUpper >>= pure . Door)
  <|> (satisfy isAsciiLower >>= pure . Key)


toIndex m (x,y) = y * width m + x
toCoord m i     = (i `mod` width m, i `div` width m)
idx     m i     = tiles m V.! i
origins m       = V.toList $ V.findIndices (== Origin) (tiles m)
keyIndicies m   = V.toList $ V.findIndices isKey (tiles m)


-- | The tile that must first be visited before this door may be passed.
keyFor :: Tile -> Tile
keyFor (Door d) = Key $ chr (ord d + 0x20)
keyFor _        = error "not a key"


isKey (Key _) = True
isKey _       = False


isDoor (Door _) = True
isDoor _        = False
