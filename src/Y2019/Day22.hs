module Y2019.Day22 (parts) where
{- |

Part 1 is "solving for x" in the old @y = mx + b@ equation.

Part 2 really kicked my butt and I plucked away at this one for a full
week. I quickly tuned into the fact that the shuffle process was a linear
equation, modulo the numbers of cards; however, my math isn't quite practised
enough that I ever would have identified part 2 as a geometric series. Reddit
user "mcpower_" tipped me off that the correct formula can be found on Wikipedia
and that was the last piece.

References

 - https://rkrishnan.org/posts/2017-06-20-typesafe-modulus-in-haskell.html
 - https://mortberg.wordpress.com/2010/08/12/basic-ring-theory-in-haskell
 - https://www.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/fbnkaju/
 - https://www.youtube.com/watch?v=_feEMKq_Uik (Inverse modulus)
 - https://www.youtube.com/watch?v=-5kIBPR2Npk (Geometric series)
-}

import Data.List
import Parser
import Math.NumberTheory.Moduli.Class
import Numeric.Natural


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "2604")
        , (part2, Just "79608410258462")
        ]


part1 input = return . render $ (target - card0) / delta
  where
    numCards       = 10007
    target         = 2019
    shuffleProcess = parse (some move) input
    (delta, card0) = shuffle numCards shuffleProcess


part2 input = return . render $ (card0 * geometricSeries) + (target * deltaExp)
  where
    numCards        = 119315717514047
    iterations      = 101741582076661
    target          = 2020
    shuffleProcess  = parse (some move) input
    (delta, card0)  = shuffle numCards shuffleProcess
    deltaExp        = powSomeMod delta iterations
    geometricSeries = (1 - deltaExp) / (1 - delta)


data Move
  = Cut  Integer
  | Deal Integer
  | New
  deriving Show


type Deck  = [SomeMod]
type Order = (SomeMod, SomeMod) -- fst * x + snd


shuffle :: Natural -> [Move] -> Order
shuffle numCards = foldl' technique (1 `modulo` numCards, 0 `modulo` numCards)


technique :: Order -> Move -> Order
technique (delta, card0) (Cut  n) = (delta                , card0 + fromInteger n * delta)
technique (delta, card0) (Deal n) = (delta / fromInteger n, card0)
technique (delta, card0) New      = (negate delta         , card0 - delta)


move :: Parser Move
move = do
    move' <- cutMove <|> dealMove <|> newMove
    spaces
    return move'
  where
    cutMove  = string "cut "                 >> number  >>= return . Cut
    dealMove = string "deal with increment " >> natural >>= return . Deal
    newMove  = string "deal into new stack"  >> return New


render :: SomeMod -> String
render mod =
    case mod of
        SomeMod sm -> show $ getVal sm
        _          -> error "unknown modulo"
