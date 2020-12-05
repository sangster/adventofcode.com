module Y2015.Day03 (parts) where

import qualified Data.HashSet as S


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "2565")
        , (part2, Just "2639")
        ]


part1 input = show . S.size $ move (S.singleton (0,0)) (0,0) input


part2 input = show . S.size $ S.union santaMap roboSantaMap
  where
    santaMap     = move (S.singleton (0,0)) (0,0) santaInput
    roboSantaMap = move (S.singleton (0,0)) (0,0) roboSantaInput
    (santaInput, roboSantaInput) = splitInput ("","") input

    splitInput (a,b) []        = (reverse a, reverse b)
    splitInput (a,b) (c:[])    = splitInput (c:a, b) []
    splitInput (a,b) (c:c':cs) = splitInput (c:a, c':b) cs


diff :: (Int,Int) -> Char -> (Int,Int)
diff (x,y) '^' = (x  , y-1)
diff (x,y) '>' = (x+1, y  )
diff (x,y) 'v' = (x  , y+1)
diff (x,y) '<' = (x-1, y  )


move :: S.HashSet (Int,Int) -> (Int,Int) -> String -> S.HashSet (Int,Int)
move set _ [] = set
move set xy (c:cs) = move (S.insert xy' set) xy' cs
  where xy' = diff xy c
