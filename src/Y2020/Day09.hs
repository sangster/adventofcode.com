module Y2020.Day09 (parts) where

import qualified Data.Dequeue as Q
import qualified Data.HashSet as S
import           Data.List
import           Data.Maybe


type Preamble = (S.HashSet Int, Q.BankersDequeue Int)
type Queue    = [Int]


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "23278925")
        , (part2, Just "4011064")
        ]


part1 input = show $ findMissingSum pre qs
  where
    (pre, qs) = preambleAndQueue 25 $ read <$> lines input


part2 input = show $ head contiguous + last contiguous
  where
    contiguous = sort . fromJust $ findContiguous missing items
    missing    = findMissingSum pre qs
    (pre, qs)  = preambleAndQueue 25 items
    items      = read <$> lines input


preambleAndQueue :: Int -> [Int] -> (Preamble, Queue)
preambleAndQueue n items = (q, rest)
  where
    q = (S.fromList pre, Q.fromList pre)
    (pre, rest) = splitAt n items


findMissingSum :: Preamble -> Queue -> Int
findMissingSum (set, deq) (q:qs)
  | hasMatch  = findMissingSum (push (set, deq) q) qs
  | otherwise = q
  where
    hasMatch = isJust $ findSumOf (set, deq) q


findSumOf :: Preamble -> Int -> Maybe (Int, Int)
findSumOf p@(set, deq) n
  | Q.null deq = Nothing
  | includes p' target = Just (n', target)
  | otherwise = findSumOf p' n
  where
    (n', p') = pop p
    target = n - n'


push :: Preamble -> Int -> Preamble
push p n = (S.insert n s', Q.pushBack d' n)
  where
    (_, (s', d')) = pop p


pop :: Preamble -> (Int, Preamble)
pop (s, d) = (n, (s', d'))
  where
    (n, d') = fromJust $ Q.popFront d
    s'      = S.delete n s


includes :: Preamble -> Int -> Bool
includes (s, _) n = S.member n s


findContiguous :: Int -> Queue -> Maybe [Int]
findContiguous target []    = Nothing
findContiguous target queue = case find' target queue of
                                Just n  -> Just n
                                Nothing -> findContiguous target (tail queue)
  where
    find' _ [] = Nothing
    find' t (q:qs) | q < t     = (q :) <$> find' (t-q) qs
                   | q > t     = Nothing
                   | otherwise = Just [q]
