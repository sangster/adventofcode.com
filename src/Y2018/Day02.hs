module Y2018.Day02 (parts, part1, part2) where

import Data.List (group, sort)
import Data.Maybe (catMaybes)
import Safe (headMay)


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Nothing)
        , (part2, Nothing)
        ]


type TwoCount   = Int
type ThreeCount = Int

part1 content = show $ has2 * has3
  where
    (has2, has3) = countRepeats . lines $ content
    countRepeats :: [String] -> (TwoCount, ThreeCount)
    countRepeats [] = (0, 0)
    countRepeats (x:xs) =
        (has2' + (if any (== 2) counts then 1 else 0),
         has3' + (if any (== 3) counts then 1 else 0))
      where
        (has2', has3') = countRepeats xs
        counts = countLetters x
        countLetters :: String -> [Int]
        countLetters = fmap length . group . sort

type Prefix = String
type Suffix = String

pairs :: [a] -> [(a, a)]
pairs (x1:x2:[]) = [(x1,x2)]
pairs (x1:tail@(x2:_)) = [(x1,x2)] ++ (pairs tail)


part2 content =
    case headMay oneDiffs of
        Just (prefix, suffix) -> prefix ++ (tail suffix)
        _                     -> "???"
  where
    oneDiffs = catMaybes $ (uncurry common) <$> pairs sorted
    sorted = sort . lines $ content

    common :: String -> String -> Maybe (Prefix, Suffix)
    common xs ys =
        common' xs ys (Just ("", ""))
      where
        common' :: String -> String -> Maybe (Prefix, Suffix) -> Maybe (Prefix, Suffix)
        common' "" _ Nothing = Nothing
        common' _ "" Nothing = Nothing
        common' "" _ (Just (ps, "")) = Nothing
        common' _ "" (Just (ps, ss)) = Just (ps, ss)

        common' xxs@(x:xs) (y:ys) (Just (ps, ""))
          | x /= y    = common' xs ys (Just (ps, [x]))
          | otherwise = common' xs ys (Just (ps ++ [x], ""))

        common' (x:xs) (y:ys) (Just (ps, ss))
          | x /= y    = Nothing
          | otherwise = common' xs ys (Just (ps, ss ++ [x]))
