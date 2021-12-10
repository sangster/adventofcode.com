module Y2021.Day10 (parts) where

import Data.List  (findIndex, sort)
import Data.Maybe (fromJust)
import Parser

parts = ( (part1, Just "311949")
        , (part2, Just "3042730309")
        , parse $ splitSome spaces (some $ oneOf (opens++closes))
        )


part1 :: [String] -> String
part1 lines = show . sum $ score . check <$> lines
  where
    score (Corrupt ')') = 3
    score (Corrupt ']') = 57
    score (Corrupt '}') = 1197
    score (Corrupt '>') = 25137
    score _             = 0


part2 :: [String] -> String
part2 lines = show $ scores !! (length scores `div` 2)
  where
    scores = sort . map (score . complete) . filter isIncomplete
           $ check <$> lines

    isIncomplete (Incomplete _) = True
    isIncomplete _              = False

    complete (Incomplete [])     = []
    complete (Incomplete (x:xs)) = close x : complete (Incomplete xs)
      where
        close x = closes !! fromJust (findIndex (== x) opens)

    score = foldl (\acc ch -> acc * 5 + scoreCh ch) 0
    scoreCh ')' = 1
    scoreCh ']' = 2
    scoreCh '}' = 3
    scoreCh '>' = 4


data Result = Ok                -- ^ Found no errors.
            | Corrupt Char      -- ^ Encountered an unexpected +closes+ char.
            | Incomplete String -- ^ A list of unclocked +opens+.
            deriving Show


opens  = "([<{" -- ^ Opens a chunk.
closes = ")]>}" -- ^ Closes a chunk.


-- | Check the given string for syntax errors.
check :: String -> Result
check    = check' []
  where
    check' []     []     = Ok
    check' ss     []     = Incomplete ss
    check' []     (x:xs) | isOpen x  = check' [x] xs
                         | otherwise = Corrupt x
    check' (s:ss) (x:xs) | isPair s x = check' ss xs
                         | isOpen x   = check' (x:s:ss) xs
                         | otherwise  = Corrupt x

    isPair '(' ')' = True
    isPair '[' ']' = True
    isPair '<' '>' = True
    isPair '{' '}' = True
    isPair _   _   = False

    isOpen = flip elem opens
