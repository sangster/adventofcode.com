module Y2015.Day01 (parts) where


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "232")
        , (part2, Just "1783")
        ]


part1 input = show $ move input
  where
    move [] = 0
    move ('(':xs) = 1 + move xs
    move (')':xs) = (-1) + move xs


part2 input = show $ basePos 0 (zip input [1..])
  where
    basePos (-1) ((_,i):_) = i - 1
    basePos floor (('(',i) : xs) = basePos (floor+1) xs
    basePos floor ((')',_) : xs) = basePos (floor-1) xs
