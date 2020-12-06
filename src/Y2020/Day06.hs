module Y2020.Day06 (parts) where

import qualified Data.HashSet as S
import           Parser


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "7128")
        , (part2, Just "3640")
        ]


part1 input = show . sum $ (S.size . S.unions) <$> groups
  where
    groups = parse (some group) input


part2 input = show . sum $ (S.size . intersect) <$> groups
  where
    intersect (x:xs) = foldr S.intersection x xs
    groups = parse (some group) input


type Answers = S.HashSet Char
type Group = [Answers]


group :: Parser Group
group = do ans <- some answers
           char '\n' <|> eof
           pure ans


answers :: Parser Answers
answers = do ans <- some (oneOf ['a'..'z'])
             char '\n' <|> eof
             pure $ S.fromList ans
