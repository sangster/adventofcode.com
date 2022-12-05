{-# LANGUAGE ImportQualifiedPost #-}
module Y2022.Day05 (parts) where

import Data.Char (isUpper)
import Data.Foldable (foldl')
import Data.IntMap.Strict qualified as M
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import Parser

parts = ( (part1, Just "VRWBSFZWM")
        , (part2, Just "RBTWJWMCF")
        , parse stacksAndMoves
        )

part1 :: (Cargo, [Move]) -> [Crate]
part1 = stackTops . uncurry (foldl' move9000)
  where
    move9000 = moveF reverse


part2 :: (Cargo, [Move]) -> [Crate]
part2 = stackTops . uncurry (foldl' move9001)
  where
    move9001 = moveF id


type Cargo = M.IntMap Stack
type Stack = [Crate]
type Crate = Char

type Move    = (Int, StackId, StackId)
type StackId = Int


-- | Apply a 'Move' order to a 'Cargo', modifying the order of the moved crates
-- with @f@.
moveF :: (Stack -> Stack) -> Cargo -> Move -> Cargo
moveF f cargo (n, from, to) = M.insert from src' $ M.insert to dst' cargo
  where
    src = cargo M.! from
    dst = cargo M.! to
    (moved, src') = splitAt n src
    dst' = f moved ++ dst


-- | Return the top-most 'Crate' of each non-empty 'Stack'.
stackTops :: Cargo -> [Crate]
stackTops = fmap (head . snd) . filter (not . null) . M.toList


stacksAndMoves :: Parser (Cargo, [Move])
stacksAndMoves = (,) <$> cargo <*> (string "\n\n" >> moves)
  where
    cargo :: Parser Cargo
    cargo = toCargo <$> units <*> ids
      where
        units = splitSome (oneOf " \n") (crate <|> air)
        crate = do ch <- char '[' >> satisfy isUpper
                   char ']' >> pure (Just ch)
        air   = symbol Nothing $ string "   "
        ids   = splitSome pad (pad >> natural)
        pad   = many $ char ' '

        toCargo units' ids' = M.fromList $ zip ids' stacks
          where
            stacks = catMaybes <$> transpose (chunksOf (length ids') units')

    moves :: Parser [Move]
    moves = splitSome (char '\n') move
      where
        move  = (,,) <$> count <*> from <*> to
        count = reserved "move" >> natural
        from  = char ' ' >> reserved "from" >> natural
        to    = char ' ' >> reserved "to"   >> natural
