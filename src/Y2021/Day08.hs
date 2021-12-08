module Y2021.Day08 (parts) where

import Data.Char  (chr, ord)
import Data.List
import Data.Maybe (fromJust)
import Parser

parts = ( (part1, Just "525")
        , (part2, Just "1083859")
        , parse $ splitSome (char '\n') entry
        )


part1 :: [Entry] -> String
part1 = show . length . filter isUnique . fmap length . concatMap snd
  where
    isUnique = flip elem [2, 3, 4, 7]


part2 :: [Entry] -> String
part2 es = show . sum $ toInt . unscrambleOutput <$> es
  where
    -- | Convert a series of segment digits into a single int.
    toInt :: [Digit] -> Int
    toInt ds = foldl1 (\acc n -> acc * 10 + n)
             $ (\d -> fromJust $ findIndex (== d) digitSegments) <$> ds

    -- | Return the output segment digits from Entry, but unscrambled.
    unscrambleOutput :: Entry -> [Digit]
    unscrambleOutput e = unscramble <$> snd e
      where
        segMap = findSegments e
        unscramble out = sort $ fixChar <$> out
          where
            fixChar ch = chr $ ord 'a' + fromJust (findIndex (== ch) segMap)


type Digit = String
type Entry = ([Digit], [Digit])


-- | Unscrambles the segments from the given Entry. The returned array contains
--   7 chars, where each char represents the new character for 'a'..'z'. For
--   example, if the second element is 'g', then 'g' in this entry maps to 'b'
--   (the second letter) in the following segment diagram:
--
--    aa
--   b  c
--    dd
--   e  f
--    gg
findSegments :: Entry -> [Char]
findSegments es = [a, b, c, d, e, f, g]
  where
    allSegments = ['a' .. 'g']
    digits = fst es ++ snd es
    one   = fromJust $ find ((== 2) . length) digits
    four  = fromJust $ find ((== 4) . length) digits
    seven = fromJust $ find ((== 3) . length) digits
    eight = fromJust $ find ((== 7) . length) digits

    len5  = filter ((== 5) . length) digits
    len6  = filter ((== 6) . length) digits

    a = head $ seven \\ one
    b = head $ bd \\ dg
    c = head $ (allSegments \\ foldr1 intersect len6) \\ [d,e]
    d = head $ delete b bd
    e = head $ delete g eg
    f = head $ allSegments \\ [a,b,c,d,e,g]
    g = head $ delete d dg

    bd = four \\ one
    eg = eight \\ (a : four)
    dg = delete a $ foldr1 intersect len5


entry :: Parser Entry
entry = do signals <- splitSome (char ' ') segment
           _ <- string "| "
           outputs <- splitSome (char ' ') segment
           pure (signals, outputs)
  where
    segment = sort <$> some (satisfy isDigitChar)
    isDigitChar c = n >= ord 'a' && n <= ord 'g' where n = ord c



digitSegments :: [Digit]
digitSegments = [ "abcefg"
                , "cf"
                , "acdeg"
                , "acdfg"
                , "bcdf"
                , "abdfg"
                , "abdefg"
                , "acf"
                , "abcdefg"
                , "abcdfg"
                ]
