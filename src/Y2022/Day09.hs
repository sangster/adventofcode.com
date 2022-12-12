{-# LANGUAGE ImportQualifiedPost #-}
module Y2022.Day09 (parts) where

import Data.Bool (bool)
import Data.HashSet qualified as S
import Parser

parts = ( (part1, Just "6284")
        , (part2, Just "2661")
        , parse $ splitSome (char '\n') movement
        )
  where
    movement :: Parser Move
    movement = (,) <$> dir <*> (char ' ' >> natural)
      where
        dir = symbol U (char 'U')
          <|> symbol R (char 'R')
          <|> symbol D (char 'D')
          <|> symbol L (char 'L')


part1 :: [Move] -> String
part1 = show . S.size . scanMoves (newRope 2)


part2 :: [Move] -> String
part2 = show . S.size . scanMoves (newRope 10)


type Move = (Dir, Int)
data Dir = U | R | D | L deriving (Show)
type Rope = [Position]
type Position = (Int, Int)


-- | Create a 'Rope' of a given length.
newRope :: Int -> Rope
newRope = flip replicate (0,0)


-- | Return the set of positions that the knot in a 'Rope' passes through, given
-- a list of moves.
scanMoves :: Rope -> [Move] -> S.HashSet Position
scanMoves origin = scan' (S.singleton $ last origin) origin
  where
    scan' :: S.HashSet Position -> Rope -> [Move] -> S.HashSet Position
    scan' acc _ [] = acc
    scan' acc rope ((d,n):ms) = scan' acc' rope' ms'
      where
        acc'  = last rope' `S.insert` acc
        rope' = step rope d
        ms'   = bool ms ((d, pred n):ms) $ n /= 1


-- | Change all positions in a 'Rope', by moving once in the given direction.
step :: Rope -> Dir -> Rope
step r d = h' : followTail h' (tail r)
  where
    h' = step' (head r) d

    step' (hx,hy) U = (hx  , hy-1)
    step' (hx,hy) R = (hx+1, hy  )
    step' (hx,hy) D = (hx  , hy+1)
    step' (hx,hy) L = (hx-1, hy  )

    followTail x (p:ps) = p' : followTail p' ps where p' = follow x p
    followTail _ []     = []

    follow :: Position -> Position -> Position
    follow (hx,hy) (tx,ty) | diag || dist < 2 = (tx, ty)
                           | dist == 2        = (pull tx hx, pull ty hy)
                           | otherwise        = dovetail
      where
        dist = dx + dy
        diag = dx == 1 && dy == 1
        dx   = abs $ hx - tx
        dy   = abs $ hy - ty
        pull x x' = case compare x x' of LT -> x' - 1
                                         EQ -> x'
                                         GT -> x' + 1
        dovetail = ( bool (pred tx) (succ tx) (hx>tx)
                   , bool (pred ty) (succ ty) (hy>ty)
                   )
