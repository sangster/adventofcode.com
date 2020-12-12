{-# LANGUAGE ExistentialQuantification #-}

module Solution
    ( Solution
    , Solveable(..)
    , DaySolution(..)
    , parseInput
    , partA
    , partB
    , solve
    ) where


class Solution s where
  parseInput :: s a -> String -> a
  partA      :: s a -> (a -> String, Maybe String)
  partB      :: s a -> (a -> String, Maybe String)


instance Solution DaySolution where
  parseInput (DaySolution _ _ f) str = f str
  partA      (DaySolution a _ _)     = a
  partB      (DaySolution _ b _)     = b


data Solveable s = forall a . Solution s => MkSolveable (s a)


data DaySolution a = DaySolution ((a -> String), Maybe String)
                                 ((a -> String), Maybe String)
                                 (String -> a)


solve :: ( ((a -> String), Maybe String)
         , ((a -> String), Maybe String)
         , (String -> a)
         )
      -> Solveable DaySolution
solve (a, b, f) = MkSolveable $ DaySolution a b f
