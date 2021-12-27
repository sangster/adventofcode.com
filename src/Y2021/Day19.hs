{-# LANGUAGE ImportQualifiedPost, NamedFieldPuns #-}
module Y2021.Day19 (parts) where

import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.HashSet        qualified as S
import Data.Hashable
import Data.List
import Data.Maybe
import Parser


parts = ( (part1, Just "335")
        , (part2, Just "10864")
        , fixCoords . (parse $ splitSome (char '\n') scanner)
        )


part1 :: [Scanner] -> String
part1 ss = show . length $ foldr (\s acc -> S.union acc (beacons s)) S.empty ss


part2 :: [Scanner] -> String
part2 ss = show $ foldr (\(a,b) acc -> maximum [distance a b, acc]) 0 pairs
  where
    pairs = [(a,b) | a <- ss, b <- ss, a /= b]

    distance a b = manhattenDistance a' b'
      where
        a' = fromJust $ scannerCoords a
        b' = fromJust $ scannerCoords b


manhattenDistance :: Coord -> Coord -> Int
manhattenDistance (x,y,z) (x',y',z') = abs (x-x') + abs (y-y') + abs (z-z')


-- | Changes the Coords of every scanner's beacons to be relative to the first
--   and calculate the Coords of each Scanner.
fixCoords :: [Scanner] -> [Scanner]
fixCoords ss = fst (remap head' ([], (tail ss)))
  where
    head' = (head ss){ scannerCoords = Just (0,0,0) }

    remap :: Scanner -> ([Scanner], [Scanner]) -> ([Scanner], [Scanner])
    remap d (solved, []) = (d:solved, [])
    remap d (solved, q)  = remap (head solved') (tail solved' ++ [d], q')
      where
        solved' = solved ++ (catMaybes $ fixCoordsSingle d <$> q)
        q'      = q \\ solved'


-- | Changes the Coords of a single scanner's beacons, if possible, to be
--   relative to the first and calculate the Coords of each Scanner.
fixCoordsSingle :: Scanner -> Scanner -> Maybe Scanner
fixCoordsSingle root s = do f <- findRotationAndOffset root s
                            pure s{ beacons = S.map f (beacons s)
                                  , scannerCoords = Just $ f (0,0,0)
                                  , distances = distances' f
                                  }
  where
    distances' f = (\(a,b) -> (f a, f b)) `M.map` distances s


-- | Iterate over pairs of beacons, likely to be the same, from the two give
--   Scanners and return the first rotation function that makes at least 12
--   beacons match.
--
--   TODO: This is a bit slow.
findRotationAndOffset :: Scanner -> Scanner -> Maybe (Coord -> Coord)
findRotationAndOffset src dst = find' $ candidateBeacons src dst
  where
    find' :: [(Coord, Coord)] -> Maybe (Coord -> Coord)
    find' []            = Nothing
    find' ((s, d):rest) = findRotation d <|> find' rest
      where
        originS = translateF s `S.map` beacons src

        findRotation :: Coord -> Maybe (Coord -> Coord)
        findRotation b' = findRotation' rotationsF
          where
            originB' = translateF b' `S.map` beacons dst

            findRotation' :: [(Coord -> Coord)] -> Maybe (Coord -> Coord)
            findRotation' [] = Nothing
            findRotation' (f:fs) =
                if hasOverlappingCount originS 12 $ f <$> S.toList originB'
                then Just $ translate' . f . translateF b'
                else findRotation' fs
              where
                translate' (x,y,z) = (x+dx, y+dy, z+dz)
                (dx, dy, dz) = s

        -- | Translate a Coord so that the first Coord is its origin.
        translateF :: Coord -> Coord -> Coord
        translateF  (dx,dy,dz) = (\(x,y,z) -> (x-dx, y-dy, z-dz))


-- | Group beacons from the two given Scanners that are probably the same.
candidateBeacons :: Scanner -> Scanner -> [(Coord, Coord)]
candidateBeacons a b = same $ M.toList (distances a)
  where
    same :: [(Int, (Coord, Coord))] -> [(Coord, Coord)]
    same [] = []
    same ((k,v):es) = case distances b M.!? k of
                        Just v' -> (fst v, fst v') : same es
                        Nothing -> same es


-- | Do at least N elements in the list of Coords exist in the set?
hasOverlappingCount :: S.HashSet Coord -> Int -> [Coord] -> Bool
hasOverlappingCount _ 0 _  = True
hasOverlappingCount _ _ [] = False
hasOverlappingCount src n (c:cs)
  | S.member c src = hasOverlappingCount src (n-1) cs
  | otherwise      = hasOverlappingCount src n cs


-- | 24 Cube rotation functions.
rotationsF :: [Coord -> Coord]
rotationsF = rotateAroundZ >>= (<$> rotateAroundFaces) . (.)
  where
    rotateAroundFaces = [ id
                        -- Rotate around X
                        , (\(x,y,z) -> ( x,-z, y))
                        , (\(x,y,z) -> ( x,-y,-z))
                        , (\(x,y,z) -> ( x, z,-y))
                        -- Rotate around Y
                        , (\(x,y,z) -> (-z, y, x))
                        , (\(x,y,z) -> ( z, y,-x))
                        ]
    rotateAroundZ = [ id
                    , (\(x,y,z) -> (-y, x, z))
                    , (\(x,y,z) -> (-x,-y, z))
                    , (\(x,y,z) -> ( y,-x, z))
                    ]


type Coord = (Int, Int, Int) -- ^ (X, Y, Z)
data Scanner = Scanner { sid :: Int
                       , beacons :: S.HashSet Coord
                       , distances :: M.HashMap Int (Coord, Coord)
                       , scannerCoords :: Maybe Coord
                       } deriving Show

instance Eq       Scanner where (==) = (==) `on` sid
instance Hashable Scanner where hashWithSalt n = hashWithSalt n . sid


scanner :: Parser Scanner
scanner = do s  <- string "--- scanner " >> natural
             bs <- string " ---\n" >> splitSome (char '\n') beacon
             pure $ Scanner { sid = s
                            , beacons = S.fromList bs
                            , scannerCoords = Nothing
                            , distances = M.fromList (distances' bs)
                            }
  where
    beacon = liftM (\(x:y:z:_) -> (x,y,z)) $ splitSome (char ',') number
    distances' bs = (\(a,b) -> (manhattenDistance a b, (a,b))) <$> pairs bs
    pairs bs = [(a,b) | a <- bs, b <- bs, a /= b]
