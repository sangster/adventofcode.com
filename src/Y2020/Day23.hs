module Y2020.Day23 (parts) where

import Data.Bool (bool)
import Control.Monad.ST (ST(..), runST)
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad


parts = ( (part1, Just "96342875")
        , (part2, Just "563362809504")
        , fmap (\c -> read [c]) :: String -> [Cup]
        )


part1 :: [Cup] -> String
part1 cups = scoreGame cups 100
  where
    scoreGame cs n = runST $ do
      arr <- buildArray cs
      play arr (head cs) n
      score arr


part2 :: [Cup] -> String
part2 cups = show . uncurry (*) $ findStars cups' 10000000
  where
    cups' = cups ++ [maximum cups + 1 .. 1000000]

    findStars cs n = runST $ do
      arr <- buildArray cs
      play arr (head cups) n
      a <- readArray arr 1
      b <- readArray arr a
      pure (a,b)


type Cup = Int


buildArray :: [Cup] -> ST s (STUArray s Int Cup)
buildArray cs = do
    arr <- newArray_ bounds
    forM_ (pairs cs) $ uncurry (writeArray arr)
    writeArray arr (last cs) (head cs)
    pure arr
  where
    bounds = (1, length cs)
    pairs (_:[])    = []
    pairs (x:x':xs) = (x,x') : pairs (x':xs)


play :: STUArray s Int Cup
     -> Cup
     -> Int
     -> ST s Cup
play cups start nRounds = iterateM nRounds (move cups) (pure start)


move :: STUArray s Int Cup
     -> Cup
     -> ST s Cup
move arr curr = do
    pickup <- next3
    maxCup <- snd <$> getBounds arr
    let dest = findDest pickup maxCup (curr-1)

    next <- readArray arr (last pickup)
    writeArray arr curr next

    readArray arr dest >>= writeArray arr (last pickup)
    writeArray arr dest (head pickup)
    pure next
  where
    next3 = do a <- readArray arr curr
               b <- readArray arr a
               c <- readArray arr b
               pure [a,b,c]
    findDest cs max 0 = findDest cs max max
    findDest cs max n = bool n (findDest cs max $ n-1) $ elem n cs


iterateM :: Monad m => Int -> (a -> m a) -> m a -> m a
iterateM 0 _ mx = mx
iterateM n f mx = iterateM (n-1) f (mx >>= f)


score :: STUArray s Int Cup
      -> ST s String
score = score' 1
  where
    score' n arr = do
      next <- readArray arr n
      if next == 1
      then pure ""
      else (show next ++) <$> score' next arr
