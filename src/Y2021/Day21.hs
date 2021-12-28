{-# LANGUAGE DeriveAnyClass, DeriveGeneric, NamedFieldPuns #-}
module Y2021.Day21 (parts) where

import Control.Parallel.Strategies (NFData, using, parList, rdeepseq)
import Data.Bool (bool)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Parser


parts = ( (part1, Just "906093")
        , (part2, Just "274291038026362")
        , parse $ liftM (\(a:b:_) -> (a,b)) (splitSome (char '\n') player)
        )


part1 :: Players -> String
part1 ps = show $ minimum (score <$> ps') * count
  where
    (ps', count) = playDeterministic 100 ps


-- | TODO: Slow and parallel.
part2 :: Players -> String
part2 ps = show $ maximum [wins a, wins b]
  where
    (a, b) = playQuantum ps First


type Players = (Player, Player)
data Player = Player { name  :: Int
                     , score :: Int
                     , pos   :: Int
                     , wins  :: Int
                     } deriving (Generic, NFData, Show)
data Turn = First
          | Second deriving Eq


instance Show Turn where
  show First = "1st"
  show _     = "2nd"

type Die a = State (Int, Int, Int) a


playDeterministic :: Int -> Players -> (Players, Int)
playDeterministic maxDie ps = (ps', count)
  where
    (ps', (_, _, count)) = runState (deterministicPlay ps) (1, maxDie, 0)

    deterministicPlay (a, b) =
        do num <- deterministicRollN 3
           let a' = move a num
           bool (deterministicPlay (b, a')) (pure (a', b)) $ score a' >= 1000

    deterministicRollN :: Int -> Die Int
    deterministicRollN = liftM sum . flip replicateM deterministicRoll
      where
        deterministicRoll = state f
          where
            f (i, max', c) | i == max' = (i, (1  , max', succ c))
                           | otherwise = (i, (i+1, max', succ c))


move :: Player -> Int -> Player
move p@Player{ score, pos } n = p{ score = score + pos' + 1, pos = pos' }
  where
    pos' = (pos + n) `mod` 10


playQuantum :: Players -> Turn -> Players
playQuantum ps turn = combineWins (multiverseResults `using` parList rdeepseq)
  where
    select = bool swap  id     $ turn == First
    turn'  = bool First Second $ turn == First

    multiverseResults = uncurry doTurn <$> combos3d3
    (p, other) = select ps

    doTurn :: Int -> Int -> Players
    doTurn n fact = if score p' >= 21
                    then select (p'{ wins = fact }, other)
                    else multiply $ playQuantum (select (p', other)) turn'
      where
        p' = move p n
        multiply (a,b) = (a{ wins = wins a * fact }, b{ wins = wins b * fact })

    -- | 3d3 combos (roll, ways).
    combos3d3 = [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]

    combineWins :: [Players] -> Players
    combineWins = foldr1 $ \(a, b) (a', b') -> ( a{ wins = wins a + wins a' }
                                               , b{ wins = wins b + wins b' }
                                               )


player :: Parser Player
player = do id'  <- string "Player " >> natural
            pos' <- string " starting position: " >> natural
            pure $ Player id' 0 (pos' - 1) 0
