module Y2019.Day03 (parts) where

import Data.Bool   (bool)
import Data.Maybe  (catMaybes)
import Data.List   (sort, tails)
import Prelude     hiding (Left, Right)

import Parser


parts = ( (part1, Just "209")
        , (part2, Just "43258")
        , parseWires
        )


part1 :: [Wire] -> String
part1 wires = show $ minimum dists
  where
    dists = distance (0, 0) <$> intersections wires


part2 :: [Wire] -> String
part2 wires = show $ minimum dists
  where
    dists   = sum' <$> intersections wires
    sum' xy = sum $ (flip wireLength xy) <$> wires


-- Models
type Wire        = [LineSegment]
type LineSegment = ((X, Y), (X, Y)) -- horizontal or vertical segments only
type X           = Int
type Y           = Int

-- Input Types
type Path     = [Move]
type Distance = Int
data Move = Up    Distance
          | Down  Distance
          | Left  Distance
          | Right Distance
  deriving Show


-- | The length of a Wire, from its origin to the given point.
wireLength :: Wire
           -> (X, Y)
           -> Int
wireLength (s:ss) xy
  | inSegment s xy = distance (fst s) xy
  | otherwise      = distance (fst s) (snd s) + wireLength ss xy
  where
    inSegment ((x1, y1), (x2, y2)) (x, y)
      | x1 == x2 && x1 == x = between y1 y2 y
      | y1 == y2 && y1 == y = between x1 x2 x
      | otherwise           = False


-- | Translate a list of move instructions into line segments.
segments :: Path
         -> [LineSegment]
segments = fst . (segments' (0, 0))
  where
    segments' prev []     = ([], prev)
    segments' prev (m:ms) = ((prev, next) : fst (segments' next ms), next)
      where next = eval prev m


-- | Apply the given Move instruction.
eval :: (X, Y)
     -> Move
     -> (X, Y)
eval (x, y) (Up    d) = (x    , y + d)
eval (x, y) (Down  d) = (x    , y - d)
eval (x, y) (Left  d) = (x - d, y    )
eval (x, y) (Right d) = (x + d, y    )


-- | The coordinates at which any of the given wires intersect.
intersections :: [Wire]
              -> [(X,Y)]
intersections wires = catMaybes $ wirePairs >>= uncurry intersect
  where wirePairs       = [(x,y) | (x:rest) <- tails wires, y <- rest]
        intersect wa wb = do { a <- wa; b <- wb; pure $ intersection a b }


-- | The coordinates at which two line segments intersect, if there is one.
intersection :: LineSegment
             -> LineSegment
             -> Maybe (X, Y)
intersection a b
  | isHorz a && isHorz b = Nothing
  | isVert a && isVert b = Nothing
  | otherwise            = (bool (flip intersect) intersect $ isHorz a) a b
  where
    isHorz                    = not . isVert
    isVert ((x1, _), (x2, _)) = x1 == x2
    intersect ((x1, y), (x2, _)) ((x, y1), (_, y2))
      | between x1 x2 x = bool Nothing (Just (x, y)) $ between y1 y2 y
      | otherwise       = Nothing


-- | @true@ if @x@ is between two bounds.
between :: (Ord a) => a -> a -> a -> Bool
between b1 b2 x = x >= b1' && x <= b2' where [b1', b2'] = sort [b1, b2]


-- | The manhattan distance between two points.
distance (x1, y1) (x2, y2) = (abs $ x2 - x1) + (abs $ y2 - y1)


--
-- Unnecessary parser stuff
--

-- | Return a list of Wires as described by the given text.
-- Each line describes a single Wire, comprising a comma-separated list of Move
-- instructions. Each Move is identified by a letter defining its direction,
-- followed by a positive integer defining a distance.
parseWires :: String
            -> [Wire]
parseWires = fmap segments . (parse $ some path)


-- | Parse a single comma-separated list of moves
path :: Parser Path
path = do moves <- some move
          spaces
          pure moves


-- | Parse a single move expression.
move :: Parser Move
move = do d <- direction
          n <- natural
          many $ char ','
          pure $ d n


-- | Parse a direction token.
direction :: Parser (Distance -> Move)
direction  = prefixOp "U" Up
         <|> prefixOp "D" Down
         <|> prefixOp "L" Left
         <|> prefixOp "R" Right
  where
    prefixOp x f = string x >> pure f
