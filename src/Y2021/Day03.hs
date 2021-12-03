module Y2021.Day03 (parts) where

import Data.Bits
import Data.Bool (bool)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Word
import Parser
import Prelude hiding (words)

parts = ( (part1, Just "3959450")
        , (part2, Just "7440311")
        , parseDiagnostic
        )


part1 :: Diagnostic -> String
part1 diag = show $ gamma * epsilon
  where
    gamma = countToGamma $ countBits (words diag) <$> bigEndianRange diag
    epsilon = foldr (flip complementBit) gamma (bigEndianRange diag)
    countToGamma = bitsToWord . fmap (\(t,f) -> bool 0 1 $ t > f)


part2 :: Diagnostic -> String
part2 diag = show $ oxygenRating * co2Rating
  where
    oxygenRating = rate (>=) (bigEndianRange diag) (words diag)
    co2Rating    = rate (<)  (bigEndianRange diag) (words diag)

    -- | Find the given word that represents a life support rating. The given
    --   function compares the number of set and unset bits at each position,
    --   keeping those words that return true.
    rate :: (Int -> Int -> Bool) -> [Int] -> [Word] -> Word
    rate _ _ (w:[]) = w
    rate _ [] _ = error "no solution!"
    rate cmp (p:ps) words' = rate cmp ps
                           $ filter (\b -> testBit b p == expectedBit) words'
      where
        expectedBit = numSet `cmp` numUnset
        (numSet, numUnset) = countBits words' p


-- | Represents the input data.
data Diagnostic = Diagnostic { width :: Int
                             , words :: [Word]
                             }


-- | Return the range of bit indicies, in big-endian order.
bigEndianRange :: Diagnostic -> [Int]
bigEndianRange d = reverse [0 .. width d - 1]


-- | Return a Diagnostic containing the word length and a list of each
--   bit-string represented on each line of the given string.
parseDiagnostic :: String -> Diagnostic
parseDiagnostic input = Diagnostic { width = width', words = words' }
  where
    width' = fromJust $ findIndex (== '\n') input
    words' = parse (splitSome (char '\n') bitString) input
    bitString = bitsToWord <$> many bitChar
    bitChar = symbol 0 (char '0') <|> symbol 1 (char '1')


-- | Convert a list of 0's and 1's into a Word (big-endian).
bitsToWord :: [Word] -> Word
bitsToWord = foldl (\bits -> (.|.) $ shift bits 1) zeroBits


-- | Return a pair counting the number of set and unset bits at the given
--   position.
countBits :: [Word]
          -> Int
          -> (Int, Int)
countBits words' pos = foldr count (0,0) words'
  where
    count :: Word -> (Int, Int) -> (Int, Int)
    count bits (t, f) = bool (t, f+1) (t+1, f) (testBit bits pos)
