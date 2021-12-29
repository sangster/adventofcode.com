module Y2021.Day22 (parts) where

import Parser
import Data.Maybe (catMaybes, maybe)
import Data.Bool
import Data.List


parts = ( (part1, Just "561032")
        , (part2, Just "1322825263376414")
        , parse $ splitSome (char '\n') step
        )


part1 :: [Step] -> String
part1 = show . countOn . compile . filter (inRange (-50, 50))
  where
    inRange (min',max') (Step s (x1,x2) (y1,y2) (z1,z2))
        | x2 < min' || y2 < min' || z2 < min' = False
        | x1 > max' || y1 > max' || z1 > max' = False
        | otherwise                           = True


-- TODO: A little slow.
part2 :: [Step] -> String
part2 = show . countOn . compile



data Step = Step Status Range Range Range deriving (Eq, Ord, Show)
data Status = On | Off deriving (Eq, Ord, Show)
type Range = (Int, Int)


-- | Cound the number of individual 1x1x1 cubes turned on.
countOn :: [Step] -> Int
countOn [] = 0
countOn ((Step Off _ _ _):_) = error "Can't count Offs."
countOn ((Step _ (x1,x2) (y1,y2) (z1,z2)):ss) = count + countOn ss
  where
    count = (succ x2 - x1) * (succ y2 - y1) * (succ z2 - z1)


-- | Convert a list of On/Off Steps into only a list of non-overlapping On
--   steps.
compile :: [Step] -> [Step]
compile = compile' . reverse
  where
    compile' [] = []
    compile' (s@(Step st (x1,x2) (y1,y2) (z1,z2)):ss)
        | st == Off = subtract' s rest
        | otherwise = join'     s rest
      where
        rest = compile' ss


-- | Join the given Step, after removing any overlapping parts.
join' :: Step -> [Step] -> [Step]
join' (Step Off _ _ _) _ = error "Can't join' Off steps."
join' s [] = [s]
join' s@(Step st (x1,x2) (y1,y2) (z1,z2)) (s':ss) = r
  where
    r =
     case divide s s' of
        Just res -> s' : foldr join' ss res
        Nothing  -> s' : join' s ss


-- | Subtract the given Step for a list of On Steps.
subtract' :: Step -> [Step] -> [Step]
subtract' (Step On _ _ _) _ = error "Can't subtract On steps."
subtract' _ []      = []
subtract' s (s':ss)
    | overlap s s' = maybe (subtract' s ss) (++ subtract' s ss) $ divide s' s
    | otherwise    = s' : subtract' s ss


-- | Split src into up-to 6 sub-cubes, after subtracting rem.
divide :: Step -> Step -> Maybe [Step]
divide src@(Step s (x1 ,x2)  (y1 ,y2)  (z1 ,z2))
       rem@(Step _ (x1',x2') (y1',y2') (z1',z2'))
    | overlap src rem = Just sides'
    | otherwise       = Nothing
  where
    sides' = catMaybes $ check <$> sides
    sides  = [below, above, left, right, behind, front]

    -- The possible cubes after subtracting rem:
    below  = Step s (x1,x2)   (y1,y2'') (z1,z2)
    above  = Step s (x1,x2)   (y1'',y2) (z1,z2)
    left   = Step s (x1,x2'') (bot,top) (z1,z2)
    right  = Step s (x1'',x2) (bot,top) (z1,z2)
    behind = Step s (l,r)     (bot,top) (z1,z2'')
    front  = Step s (l,r)     (bot,top) (z1'',z2)

    x1'' = maximum [x1, x2' + 1]
    x2'' = minimum [x2, x1' - 1]
    y1'' = maximum [y1, y2' + 1]
    y2'' = minimum [y2, y1' - 1]
    z1'' = maximum [z1, z2' + 1]
    z2'' = minimum [z2, z1' - 1]
    bot  = maximum [y1, y2'' + 1]
    top  = minimum [y2, y1'' - 1]
    l    = maximum [x1, x2'' + 1]
    r    = minimum [x2, x1'' - 1]


-- | Return Nothing if the given Step is inside-out.
check :: Step -> Maybe Step
check src@(Step _ (x1,x2)  (y1,y2)  (z1,z2))
    = bool Nothing (Just src) $ x1 <= x2 && y1 <= y2 && z1 <= z2


-- Does the two given cubes overlap?
overlap :: Step -> Step -> Bool
overlap (Step _ (x1 ,x2)  (y1 ,y2)  (z1 ,z2))
        (Step _ (x1',x2') (y1',y2') (z1',z2'))
    | (x1 > x1' || x1' > x2) && (x1' > x1 || x1 > x2') = False
    | (y1 > y1' || y1' > y2) && (y1' > y1 || y1 > y2') = False
    | (z1 > z1' || z1' > z2) && (z1' > z1 || z1 > z2') = False
    | otherwise                                        = True


step :: Parser Step
step = do s <- status
          x <- string " x=" >> range
          y <- string ",y=" >> range
          z <- string ",z=" >> range
          pure $ Step s x y z
  where
    status = symbol On (string "on") <|> symbol Off (string "off")
    range = do a <- number
               b <- string ".." >> number
               pure (a, b)
