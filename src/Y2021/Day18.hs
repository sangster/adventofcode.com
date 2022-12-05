module Y2021.Day18 (parts) where

import Parser
import Data.Maybe (fromJust)
import Data.Function (on)


parts = ( (part1, Just "4243")
        , (part2, Just "4701")
        , parse $ some snailNum
        )


part1 :: [SnailNum] -> String
part1 nums = show . magnitude $ foldl1 add nums


part2 :: [SnailNum] -> String
part2 nums = show $ foldr maxMagnitude 0 $ pairs nums
  where
    pairs ps = [(x,y) | x <- ps, y <- ps, x /= y]
    maxMagnitude (a, b) mag = maximum [mag, magnitude $ add a b]


data SnailNum = RegNum Int
              | Pair SnailNum SnailNum
              deriving (Eq, Show)


add :: SnailNum -> SnailNum -> SnailNum
add a b = reduce $ Pair a b


reduce :: SnailNum -> SnailNum
reduce num = fromJust $ liftM reduce (explode num)
                    <|> liftM reduce (split   num)
                    <|> Just num


explode :: SnailNum -> Maybe SnailNum
explode num = fst <$> explodeN (0 :: Int) num
  where
    explodeN 4     (Pair a b) = Just (RegNum 0, (Just a, Just b))
    explodeN depth (Pair a b) = liftM left  (explodeN (depth+1) a)
                            <|> liftM right (explodeN (depth+1) b)
      where
        left  (new, (a', b')) = (Pair new (pushRight b b'), (a', Nothing))
        right (new, (a', b')) = (Pair (pushLeft a a') new,  (Nothing, b'))

    explodeN _ _ = Nothing

    pushLeft (RegNum x) (Just (RegNum dx)) = RegNum $ x + dx
    pushLeft (Pair a b) dx                 = Pair a (pushLeft b dx)
    pushLeft x          _                  = x

    pushRight (RegNum x) (Just (RegNum dx)) = RegNum $ x + dx
    pushRight (Pair a b) dx                 = Pair (pushRight a dx) b
    pushRight x          _                  = x


split :: SnailNum -> Maybe SnailNum
split (Pair a b) = liftM (flip Pair b) (split a)
               <|> liftM (Pair a)      (split b)
split (RegNum n) | n >= 10   = Just $ (Pair `on` RegNum) (floor n') (ceiling n')
                 | otherwise = Nothing
  where
    n' = fromIntegral n / 2 :: Float


snailNum :: Parser SnailNum
snailNum = whitespace >> pair <|> liftM RegNum natural
  where
    pair = do x <- char '[' >> snailNum
              y <- char ',' >> snailNum
              char ']' >> pure (Pair x y)


magnitude :: SnailNum -> Int
magnitude (RegNum n) = n
magnitude (Pair a b) = magnitude a * 3 + magnitude b * 2
