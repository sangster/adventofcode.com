module Y2022.Day14 (parts) where

import Parser

parts = ( (part1, Just "825")
        , (part2, Nothing)
        , fmap segment
        . parse (splitSome (char '\n') $ splitSome (string " -> ") xy)
        )
  where
    xy :: Parser XY
    xy = (,) <$> natural <*> (char ',' >> natural)

    segment :: [XY] -> [Segment]
    segment [] = []
    segment [_] = []
    segment (a:b:rest) = (a,b) : segment (b:rest)


part1 :: [Path] -> String
part1 paths = show $ countDrops 0 paths
  where
    bottom = findBottom paths
    countDrops :: Int -> [Path] -> Int
    countDrops acc ps = case fullDrop bottom ps origin of
                     Nothing -> acc
                     Just xy -> countDrops (succ acc) ([(xy,xy)]:ps)


-- low: 26728
part2 :: [Path] -> String
part2 paths = show $ countDrops 0 paths'
  where
    paths' = [((minBound, floorY), (maxBound, floorY))] : paths
    floorY = findBottom paths + 2

    countDrops :: Int -> [Path] -> Int
    countDrops acc ps = case fullDrop floorY ps origin of
                     Nothing -> acc
                     Just xy -> if xy == origin
                                then acc
                                else countDrops (succ acc) ([(xy,xy)]:ps)


type Path = [Segment]
type Segment = (XY, XY)
type XY = (X,Y)
type X = Int
type Y = Int




fullDrop :: Y -> [Path] -> XY -> Maybe XY
fullDrop bottom ps xy =
    case drop' ps xy of
      Nothing    -> Just xy
      Just (x,y) -> if y == bottom
                    then Nothing
                    else fullDrop bottom ps (x,y)


drop' :: [Path] -> XY -> Maybe XY
drop' ps (x,y) = if canMove (x, y+1) ps
                 then Just (x, y+1)
                 else if canMove (x-1, y+1) ps
                      then Just (x-1, y+1)
                      else if canMove (x+1, y+1) ps
                           then Just (x+1, y+1)
                           else Nothing


canMove :: XY -> [Path] -> Bool
canMove _     []     = True
canMove (x,y) (p:ps) = not (insidePath p) && canMove (x,y) ps
  where
    insidePath [] = False
    insidePath (((sx,sy), (sx',sy')):ss) = intersect || insidePath ss
      where
        intersect = (x == sx && x == sx' && between (sy, sy') y)
                 || (y == sy && y == sy' && between (sx, sx') x)
        between (b1,b2) v = (b1 <= b2 && v >= b1 && v <= b2)
                         || (v <= b1 && v >= b2)


origin :: XY
origin = (500,0)


findBottom :: [Path] -> Y
findBottom [] = snd origin
findBottom (ss : rest) = maximum [findBottom' ss, findBottom rest]
  where
    findBottom' [] = snd origin
    findBottom' ((a,b):rest') = maximum [snd a, snd b, findBottom' rest']
