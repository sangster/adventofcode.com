module Y2019.Day21 (parts) where

import Data.Char
import Util.InstructionSet
import Util.Program                hiding (Run)
import Control.Monad.Trans.Except


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "19357761")
        , (part2, Just "1142249706")
        ]


part1 input = runST $ do
    prog   <- program input
    status <- runExceptT $ runScript script prog
    case status of
        Left  video   -> pure video
        Right hullDmg -> pure $ show hullDmg
  where
    -- Jump if hole at 1 or 3, but not 4
    script =
      [ not' 1 J
      , not' 3 T
      , Or   T J
      , and' 4 J
      , Walk
      ]


part2 input = runST $ do
    prog   <- program input
    status <- runExceptT $ runScript script prog
    case status of
        Left  video   -> pure video
        Right hullDmg -> pure $ show hullDmg
  where
    script =
      [ not'  1 J
      , not'  2 T
      , Or    T J
      , not'  3 T
      , Or    T J -- J = ~1 | ~2 | ~3                   : a reason to jump
      , and'  4 J -- J = (~1 | ~2 | ~3) & 4             : a place to land
      , not'  8 T
      , flip' T
      , or'   5 T -- T = 5 | 8
      , And   T J -- J = ((~1 | ~2 | ~3) & 4) & (5 | 8) : space to walk or jump after
      , Run
      ]


data Inst
  = Walk
  | Run
  | And Reg Reg
  | Or  Reg Reg
  | Not Reg Reg


instance Show Inst where
  show Walk      = "WALK"
  show Run       = "RUN"
  show (And a b) = "AND "++show a++" "++show b
  show (Or  a b) = "OR " ++show a++" "++show b
  show (Not a b) = "NOT "++show a++" "++show b


data Reg = T | J
         | A | B | C | D | E | F | G | H | I deriving (Eq, Show)


-- Some pointless helpers for readability

and'  a b = And (toReg a) b
or'   a b = Or  (toReg a) b
not'  a b = Not (toReg a) b
flip' a   = Not a a

toReg 1 = A
toReg 2 = B
toReg 3 = C
toReg 4 = D
toReg 5 = E
toReg 6 = F
toReg 7 = G
toReg 8 = H
toReg 9 = I

encode = map ord
decode = map chr


runScript :: PrimMonad m => [Inst] -> Program' m -> ExceptT String m Data
runScript script p = do
    results <- lift $ executeUntilHalt p (encode $ command script)
    if any (> ord maxBound) results
      then pure   $ last   results
      else throwE $ decode results


command xs =
    if length xs <= 15
      then concat $ (++"\n") . show <$> xs
      else error "too many instructions"


program :: PrimMonad m => String -> m (Program' m)
program = (fmap $ Program aoc19Set) . parseRAM
