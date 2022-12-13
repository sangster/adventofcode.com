{-# LANGUAGE ImportQualifiedPost #-}
module Y2022.Day11 (parts) where

import Data.List (foldl', sort)
import Data.IntMap.Strict qualified as M
import Parser

parts = ( (part1, Just "182293")
        , (part2, Just "54832778815")
        , toMonkeyMap . parse (splitSome (string "\n\n") monkey)
        )
  where
    toMonkeyMap :: [Monkey] -> MonkeyMap
    toMonkeyMap = M.fromList . fmap (\m -> (mId m, m))

    monkey :: Parser Monkey
    monkey = Monkey 0
         <$> (reserved "Monkey" >> natural)
         <*> (reserved ":" >> reserved "Starting items:" >> items)
         <*> (whitespace >> reserved "Operation: new =" >> op)
         <*> (whitespace >> reserved "Test: divisible by" >> natural)
         <*> (whitespace >> reserved "If true: throw to monkey" >> natural)
         <*> (whitespace >> reserved "If false: throw to monkey" >> natural)
      where
        items = splitSome (string ", ") natural
        op = Op <$> opArg <*>  (spaces >> opF) <*>  (spaces >> opArg)
        opArg = symbol Old (string "old") <|> (Val <$> natural)
        opF   = symbol opAdd (char '+') <|> symbol opMul (char '*')

        opAdd old a b = resolve a old + resolve b old
        opMul old a b = resolve a old * resolve b old

        resolve :: OpArg -> Int -> Int
        resolve Old     = id
        resolve (Val n) = const n


part1 :: MonkeyMap -> String
part1 = show . monkeyBusiness . iterateMonkeyMap 3 20


part2 :: MonkeyMap -> String
part2 = show . monkeyBusiness . iterateMonkeyMap 1 10000


type MonkeyMap = M.IntMap Monkey
data Monkey = Monkey { mCount :: Int
                     , mId :: Id
                     , mItems :: [Int]
                     , mOp :: Operation
                     , mTestDiv :: Int
                     , mTrue :: Id
                     , mFalse :: Id
                     } deriving Show
type Id = Int
data Operation = Op OpArg (Int -> OpArg -> OpArg -> Int) OpArg
data OpArg = Old | Val Int deriving Show
instance Show Operation where show _ = ""


monkeyBusiness :: MonkeyMap -> Int
monkeyBusiness = product . take 2 . reverse . sort . fmap mCount . M.elems


-- | Iterate over a given 'MonkeyMap', @n@ times and return the resulting map.
-- The worry of each item is divided by @wd@ before the item is passed to
-- another monkey.
iterateMonkeyMap :: Int -> Int -> MonkeyMap -> MonkeyMap
iterateMonkeyMap wd n = last . take (n+1) . scanRounds wd


-- | Iterate over the 'MonkeyMap' a given number of times, returning a list of
-- the map after each iteration.
scanRounds :: Int -> MonkeyMap -> [MonkeyMap]
scanRounds n mmap = iterate (round' n) mmap
  where
    ids = M.keys mmap
    ring = product $ mTestDiv <$> M.elems mmap

    round' :: Int -> MonkeyMap -> MonkeyMap
    round' worryDivisor m = foldl' next m ids
      where
        next m' id' = case mItems (m' M.! id') of
                        [] -> m'
                        _  -> next (examine m' id') id'

        examine m' id' | null (mItems src) = m'
                       | otherwise         = M.insert id' src'
                                           $ M.insert dstId dst' m'
          where
            src = m' M.! id'
            dst = m' M.! dstId
            (i:is) = mItems src
            i' = (f (mOp src) `div` worryDivisor) `mod` ring
              where
                f (Op a op b) = op i a b

            src' = src{ mItems = is, mCount = succ (mCount src) }
            dst' = dst{ mItems = mItems dst ++ [i'] }
            dstId = if (i' `mod` mTestDiv src) == 0
                    then mTrue src
                    else mFalse src
