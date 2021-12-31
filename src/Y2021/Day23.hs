{-# LANGUAGE ImportQualifiedPost, NamedFieldPuns, TupleSections #-}
module Y2021.Day23 (parts) where

import Data.Bool            (bool)
import Data.Function        (on)
import Data.HashMap.Strict  qualified as M
import Data.Hashable        (Hashable, hashWithSalt, hashUsing)
import Data.List            (nub, sort, (\\))
import Data.Maybe           (fromJust)
import Data.PQueue.Prio.Min qualified as Q
import Data.Vector          qualified as V
import Parser
import Util.CharMap2D


parts = ( (part1, Just "16506")
        , (part2, Just "48304")
        , mapToBurrow . parseAndFix (map2dParser cell)
        )


-- TODO: Slow
part1 :: Burrow -> String
part1 b = show . fromJust . findLeastEnergy $ newSearch b


-- TODO: Slow
part2 :: Burrow -> String
part2 b = show . fromJust . findLeastEnergy $ newSearch b'
  where
    b' = unfoldBurrow $ extract foldedPart
    (dx, dy) = head . sort $ roomCoords b

    foldedPart = parseAndFix (map2dParser cell) extraLines
    extraLines = unlines [ "  #D#C#B#A#"
                         , "  #D#B#A#C#"
                         ]

    unfoldBurrow :: M.HashMap Amphipod [MapCoord] -> Burrow
    unfoldBurrow mid = b{ depth = depth'
                        , amphipods = amphipods''
                        , roomCoords = rooms
                        }
      where
        depth'      = depth b + mapHeight foldedPart
        amphipods'  = fmap push `M.map` amphipods b
        push (x,y)  = (x, bool y depth' $ y == depth b)
        amphipods'' = M.unionWith (++) amphipods' mid
        rooms       = sort $ (concat $ M.elems amphipods'')

    extract m = M.fromListWith (++)
              $ (\y -> fst $ foldl (add y) ([],0) [0 .. mapWidth m - 1])
                `concatMap` [0 .. mapHeight m - 1]
      where
        add y (acc,x') x = case map2dCell m (x,y) of
                             Room (Just c) -> ((c, [(x'+dx, y+dy+1)]):acc, x'+2)
                             _             -> (acc, x')


type SeenMap = M.HashMap Burrow Energy
data Search = Search Queue SeenMap (Maybe Energy)
type Queue  = Q.MinPQueue (Int,Energy) Burrow
type Energy = Int
type Map = CharMap2D Cell
data Cell = Wall
          | Floor
          | Room (Maybe Amphipod)
          deriving (Eq, Show)


data Amphipod = A | B | C | D deriving (Eq, Enum, Show)

instance Hashable Amphipod where hashWithSalt = hashUsing fromEnum

data Burrow = Burrow{ hallLength :: Int
                    , roomCols   :: (Int, Int, Int, Int)
                    , amphipods  :: M.HashMap Amphipod [MapCoord]
                    , hallCoords :: [MapCoord]
                    , roomCoords :: [MapCoord]
                    , depth      :: Int
                    }

instance Eq       Burrow where (==)         = (==) `on` amphipods
instance Hashable Burrow where hashWithSalt = hashUsing amphipods


newSearch :: Burrow -> Search
newSearch b = Search (Q.singleton (0,0) b) M.empty Nothing


findLeastEnergy :: Search -> Maybe Energy
findLeastEnergy (Search queue seen prevBest) =
    case Q.minViewWithKey queue of
      Just (((n,e), b), q) -> find' n e b q
      Nothing              -> prevBest
  where
    find' :: Int -> Energy -> Burrow -> Queue -> Maybe Energy
    find' n e b q
        | seenCost <= e = findLeastEnergy $ Search q  seen  prevBest
        | isDone b      = findLeastEnergy $ Search q  seen  best
        | otherwise     = findLeastEnergy $ Search q' seen' prevBest
      where
        seenCost = M.findWithDefault maxBound b seen
        seen' = M.insert b e seen
        best = Just $ maybe e (\e' -> minimum [e, e']) prevBest
        opts = amphipodMoveOptions b
        q'   = foldr mkState q opts

        mkState (amph, from, xys) acc = foldr mkState' acc xys
          where
            mkState' xy acc' = bool acc'' acc' $ worseThanBest (e + cost)
              where
                (cost, b') = move b amph from xy
                acc'' = Q.insert (pred n, e + cost) b' acc'

    worseThanBest :: Energy -> Bool
    worseThanBest e = maybe False (e >) prevBest


move :: Burrow -> Amphipod -> MapCoord -> MapCoord -> (Energy, Burrow)
move b amph from to = (cost, b{ amphipods = amphipods' })
  where
    cost       = moveCost amph from to
    amphipods' = M.insert amph elems (amphipods b)
    elems      = to : filter (/= from) (amphipods b M.! amph)


amphipodMoveOptions :: Burrow -> [(Amphipod, MapCoord, [MapCoord])]
amphipodMoveOptions b@Burrow{ amphipods } =
    uncurry entries `concatMap` M.toList amphipods
  where
    entries amph = fmap $ (amph,,) <*> moveOptions b amph


energy :: Amphipod -> Int
energy A = 1
energy B = 10
energy C = 100
energy D = 1000


moveOptions :: Burrow -> Amphipod -> MapCoord -> [MapCoord]
moveOptions b amph (x,y)
    | alreadyHome                       = []
    | inSomeRoom && someoneAbove (x,y)  = []
    | all (flip elem reachable) ownRoom = target
    | otherwise                         = reachable
  where
    alreadyHome  = fromOwnRoom && withFellows
    fromOwnRoom  = isOwnRoom b amph (x,y)
    withFellows  = all (flip elem fellowCoords) [(x,y') | y' <- [y .. depth b]]
    fellowCoords = amphipods b M.! amph
    inSomeRoom   = not $ inHall (x,y)
    ownRoom      = ownRoomCoords b amph
    someoneAbove = flip any occupied . isDirectlyAbove
    occupied     = occupiedCoords b
    reachable    = filter canReach emptySpaces
    target       = take 1 $ ownRoom \\ occupied

    emptySpaces  = if inSomeRoom
                   then hallCoords b \\ occupied
                   else target

    canReach (x',y') = not $ or [hasAliens, roomBlocked, hallBlocked]
      where
        someRoomDest   = not $ inHall (x',y')
        roomBlocked    = someRoomDest && someoneAbove (x',y')
        hallBlocked    = any between $ filter inHall occupied
        between (xm,_) = xm > minimum [x,x'] && xm < maximum [x,x']
        hasAliens      = someRoomDest
                      && any (not . (== amph)) (roomOccupants b x')


isOwnRoom :: Burrow -> Amphipod -> MapCoord -> Bool
isOwnRoom b a (x,y) = x == amphRoomCol b a && not (inHall (x,y))


roomOccupants :: Burrow -> Int -> [Amphipod]
roomOccupants Burrow { amphipods } col = M.foldrWithKey f [] amphipods
  where
    f amph xys acc = if any (\(x,y) -> x == col && not (inHall (x,y))) xys
                     then amph : acc
                     else acc


inOwnRoomCount :: Burrow -> Int
inOwnRoomCount b = M.foldrWithKey count 0 (amphipods b)
  where
    count  amph xys acc = foldr (atHome amph) acc xys
    atHome amph xy acc' = acc' + bool 0 1 (isOwnRoom b amph xy)


amphRoomCol :: Burrow -> Amphipod -> Int
amphRoomCol Burrow{ roomCols = (a,_,_,_) } A = a
amphRoomCol Burrow{ roomCols = (_,b,_,_) } B = b
amphRoomCol Burrow{ roomCols = (_,_,c,_) } C = c
amphRoomCol Burrow{ roomCols = (_,_,_,d) } D = d


isDone :: Burrow -> Bool
isDone b = all (\(a,cs) -> all (isOwnRoom b a) cs) $ M.toList (amphipods b)


moveCost :: Amphipod -> MapCoord -> MapCoord -> Int
moveCost a (x,y) (x',y')
    | x == x'      = dist * e
    | inHall (x,y) = dist * e
    | otherwise    = moveCost a (x,y) (x,0) + moveCost a (x,0) (x',y')
  where
    dist = abs (x-x') + abs (y-y')
    e    = energy a


inHall :: MapCoord -> Bool
inHall (_,0) = True
inHall _     = False


-- | Is the second MapCoord directly above the first?
isDirectlyAbove :: MapCoord -> MapCoord -> Bool
isDirectlyAbove (x,y) (x',y') = x' == x && y' < y


ownRoomCoords :: Burrow -> Amphipod -> [MapCoord]
ownRoomCoords b amph = reverse $ (amphRoomCol b amph,) <$> [1 .. depth b]


occupiedCoords :: Burrow -> [MapCoord]
occupiedCoords = M.foldr (++) [] . amphipods


-- | Convert the 2D ASCII map into a Burrow.
mapToBurrow :: Map -> Burrow
mapToBurrow m = Burrow{ hallLength = hall
                      , roomCols   = (a,b,c,d)
                      , amphipods  = M.fromListWith (++) entries
                      , hallCoords = xysHall
                      , roomCoords = xysRoom
                      , depth      = length xysRoom `div` 4
                      }
  where
    hall           = measureHall 0 (V.toList $ mapCells m)
    (hallX, hallY) = map2dIndexToCoord m
                   . fromJust $ V.findIndex (== Floor) (mapCells m)
    (a:b:c:d:_)    = xRoom
    xRoom          = nub . sort $ (fmap fst . snd) `concatMap` entries

    xysRoom = [(x,y) | x <- xRoom, y <- [1,2] ]
    xysHall = [(x,0) | x <- [0 .. hall - 1], not (elem x xRoom)]

    measureHall n (Floor:cs) = measureHall (succ n) cs
    measureHall 0 (_:cs)     = measureHall 0        cs
    measureHall n _          = n

    entries = V.ifoldr addEntry [] (mapCells m)
    addEntry i (Room (Just amph)) acc = (amph, [(x',y')]) : acc
      where
        (x,y)   = map2dIndexToCoord m i
        (x',y') = (x-hallX, y-hallY)
    addEntry _ _               acc = acc


-- | The input text isn't perfectly square.
parseAndFix :: Parser Map -> String -> Map
parseAndFix p = fixLength . parse p
  where
    fixLength (m@CharMap2D{ mapCells = cs }) = m{ mapCells = mapCells' }
      where
        mapCells' = cs V.++ (V.replicate (len - V.length cs) Wall)
        len       = mapWidth m * mapHeight m


cell :: Parser Cell
cell = symbol Wall            (char '#')
   <|> symbol Wall            (char ' ')
   <|> symbol Floor           (char '.')
   <|> symbol (Room (Just A)) (char 'A')
   <|> symbol (Room (Just B)) (char 'B')
   <|> symbol (Room (Just C)) (char 'C')
   <|> symbol (Room (Just D)) (char 'D')
