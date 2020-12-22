module Y2020.Day22 (parts) where

import           Data.Bool (bool)
import qualified Data.Vector.Unboxed as V
import qualified Data.HashSet as S
import           Parser
import           Data.Hashable


parts = ( (part1, Just "32783")
        , (part2, Just "33455")
        , parse $ splitSome spaces deck
        )


part1 :: [Deck] -> String
part1 decks = show . score $ playCombat a b
  where
    [a, b] = decks


-- part2 _ = ""

part2 :: [Deck] -> String
part2 decks = show . score $ playRecursiveCombat S.empty a b
  where
    [a, b] = decks


type Card    = Int
type History = S.HashSet Int

data Deck = Deck{ player :: Int
                , cards  :: V.Vector Card
                } deriving (Eq, Show)


instance Hashable Deck where
  hashWithSalt s d = hashWithSalt s (V.toList $ cards d)


deck :: Parser Deck
deck = do string "Player "
          name <- natural
          string ":\n"
          cs <- some $ token natural
          pure $ Deck{ player = name, cards = V.fromList cs }


playCombat :: Deck -> Deck -> Deck
playCombat a b
  | nullDeck a = b
  | nullDeck b = a
  | otherwise  = playCombat a' b'
  where
    a' = a{ cards = bool aRest aRest' (aTop > bTop) }
    b' = b{ cards = bool bRest bRest' (bTop > aTop) }

    (aTop, aRest) = (V.head $ cards a, V.tail $ cards a)
    (bTop, bRest) = (V.head $ cards b, V.tail $ cards b)

    aRest' = aRest V.++ V.fromList [aTop, bTop]
    bRest' = bRest V.++ V.fromList [bTop, aTop]


playRecursiveCombat :: History -> Deck -> Deck -> Deck
playRecursiveCombat history a b
  | nullDeck a  = b
  | nullDeck b  = a
  | repeatGame  = a
  | canRecurse  = if recurseWinner == player a
                  then playRecursiveCombat history' aWin  bLose
                  else playRecursiveCombat history' aLose bWin
  | aTop > bTop = playRecursiveCombat history' aWin  bLose
  | otherwise   = playRecursiveCombat history' aLose bWin
  where
    repeatGame    = S.member (hash (a,b)) history
    history'      = S.insert (hash (a,b)) history
    canRecurse    = V.length aRest >= aTop && V.length bRest >= bTop
    recurseWinner = player $ playRecursiveCombat S.empty aRecurse bRecurse

    aWin  = a{ cards = aRest V.++ V.fromList [aTop, bTop] }
    bWin  = b{ cards = bRest V.++ V.fromList [bTop, aTop] }
    aLose = a{ cards = aRest }
    bLose = b{ cards = bRest }

    (aTop, aRest) = (V.unsafeHead $ cards a, V.unsafeTail $ cards a)
    (bTop, bRest) = (V.unsafeHead $ cards b, V.unsafeTail $ cards b)

    aRecurse = a{ cards = V.take aTop aRest }
    bRecurse = b{ cards = V.take bTop bRest }


nullDeck :: Deck -> Bool
nullDeck = V.null . cards


score :: Deck -> Int
score d = V.foldr (\(a,b) -> (+) (a*b)) 0 pairs
  where
    pairs = V.fromList [1 .. V.length (cards d)] `V.zip` (V.reverse $ cards d)
