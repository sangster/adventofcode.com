module Day03 (parts, part1, part2, intersection) where

import Util.Parser

import Control.Applicative
import Data.Bool
import Data.Maybe
import Data.List
import Prelude hiding (Left, Right)


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "209")
        , (part2, Just "43258")
        ]


part1 input = return . show $ minimum dists
  where
    dists = distance (0, 0) <$> intersections wires
    wires = parseWires input


part2 input = return . show $ minimum dists
  where
    dists   = sum' <$> intersections wires
    sum' xy = sum $ (flip wireLength xy) <$> wires
    wires   = parseWires input


-- Models
type Wire        = [LineSegment]
type LineSegment = ((X, Y), (X, Y))
type X           = Int
type Y           = Int

-- Input Types
type Path     = [Move]
type Distance = Int
data Move = Up    Distance
          | Down  Distance
          | Left  Distance
          | Right Distance


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
        intersect wa wb = do { a <- wa; b <- wb; return $ intersection a b }


-- | The coordinates at which two line segments intersect, if there is one.
intersection :: LineSegment
             -> LineSegment
             -> Maybe (X, Y)
intersection a b
  | isVert a && isVert b = Nothing
  | isHorz a && isHorz b = Nothing
  | otherwise            = (bool (flip intersect) intersect $ isHorz a) a b
  where
    isVert ((x1, _), (x2, _)) = x1 == x2
    isHorz = not . isVert
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
parseWires = (fmap segments) . (runParser $ some path)


-- | Parse a single comma-separated list of moves
path :: Parser Path
path = do moves <- some move
          spaces
          return moves


-- | Parse a single move expression.
move :: Parser Move
move = do d <- direction
          n <- natural
          many $ char ','
          return $ d n


-- | Parse a direction token.
direction :: Parser (Distance -> Move)
direction  = prefixOp "U" Up
         <|> prefixOp "D" Down
         <|> prefixOp "L" Left
         <|> prefixOp "R" Right
  where
    prefixOp x f = string x >> return f