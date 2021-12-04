module Y2021.Day04 (parts) where

import Data.Bool
import Data.List
import Data.List.Split (chunksOf)
import Parser hiding (col)

parts = ( (part1, Just "35670")
        , (part2, Just "22704")
        , parse game
        )


part1 :: Game -> String
part1 = show . score . playFullGame


part2 :: Game -> String
part2 = show . score . last . findAllWinners


data Game = Game { calls  :: [CalledNum] -- ^ Numbers called during the game.
                 , boards :: [Board]     -- ^ Every game board being played.
                 } deriving Show

data Board = Board { cells  :: [Cell] -- ^ Each number listed on this board.
                   , width  :: Int    -- ^ The count of numbers in each row.
                   , height :: Int    -- ^ The count of numbers in each column.
                   } deriving (Eq, Show)

data Marker    = Unmarked | Marked     deriving (Eq, Show)
data Cell      = Cell CalledNum Marker deriving (Eq, Show)
type CalledNum = Int
type Winner    = (CalledNum, Board)


-- | Play the game until a winning board is found, returning the board and
--   winning number.
playFullGame :: Game -> Winner
playFullGame g = case callNextNumber g of
                   (g', [])    -> playFullGame g'
                   (_,  win:_) -> win


-- | Find every winner, in the order that they won.
findAllWinners :: Game -> [Winner]
findAllWinners Game { calls = [] } = []
findAllWinners g = winners ++ findAllWinners g'
  where
    (g', winners) = callNextNumber g


-- | Mark every board with the first number, and remove that number from the
-- game. Returning the new game state and any winners.
callNextNumber :: Game -> (Game, [Winner])
callNextNumber g@Game { calls = cs, boards = bs } =
    (newGameState, (,) num <$> winners)
  where
    num = head cs
    newGameState = g{ calls = tail cs, boards = losers }
    (winners, losers) = partition isWinner $ markBoard num <$> bs


-- | Is this board a winner?
isWinner :: Board -> Bool
isWinner = any winningLine . boardLines
  where
    winningLine = all (\(Cell _ mark) -> mark == Marked)


-- | Return the horizontal and vertical lines to be scored.
boardLines :: Board -> [[Cell]]
boardLines b = rows ++ cols
 where
   cs = cells b
   rows = chunksOf (width b) cs
   cols = getCol <$> [0.. width b - 1]
   getCol n = col (drop n cs) (height b)
     where
       col _   0 = []
       col cs' y = head cs' : col (drop (width b) cs') (y-1)


-- | Mark the given board, if it contains the called number.
markBoard :: CalledNum -> Board -> Board
markBoard n b = b{ cells = cells' }
  where
    cells' = markCell <$> cells b
    markCell c@(Cell n' _) = bool c (Cell n' Marked) (n == n')


score :: Winner -> Int
score (num, b) = num * sum unmarkedNums
  where
    unmarkedNums = [n | Cell n mark <- cells b, mark == Unmarked]


-- | Parse the input text into a Game object.
game :: Parser Game
game = do calledNums <- splitSome (char ',') natural
          _ <- spaces
          boards' <- splitSome spaces board
          pure $ Game { calls = calledNums, boards = boards' }
  where
    board = mkBoard <$> rows
    mkBoard rows' = Board { cells = cells', width = width', height = height' }
     where
       cells'  = concat rows'
       width'  = length $ head rows'
       height' = length rows'

    rows = splitSome (char '\n') row
    row  = splitSome (char ' ') cell
    cell = flip Cell Unmarked <$> ((char ' ' >> natural) <|> natural)
