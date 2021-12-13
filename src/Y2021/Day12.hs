module Y2021.Day12 (parts) where

import           Data.Maybe
import           Data.Char
import           Data.Bool
import           Data.List
import           Data.Foldable
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import           Parser

import Debug.Trace

parts = ( (part1, Just "3679")
        , (part2, Just "107395")
        , parseCaveTree
        )


part1 :: Cave -> String
part1 start = show . length . catMaybes
            $ findPaths [] start
  where
    findPaths :: [Cave] -> Cave -> [Maybe [Cave]]
    findPaths = walk
      where
        walk :: [Cave] -> Cave -> [Maybe [Cave]]
        walk []   c@(Start cs)   = concat $ walk [c] <$> cs
        walk _    (Start _)      = [Nothing]
        walk path End            = [Just path]
        walk path c@(Big   cs _) = concat $ walk (c:path) <$> cs
        walk path c@(Small cs _)
          | elem c path = [Nothing]
          | otherwise   = concat $ walk (c:path) <$> cs


part2 :: Cave -> String
part2 start = show . length . catMaybes
            $ findPaths [] start
  where
    findPaths :: [Cave]
              -> Cave
              -> [Maybe [Cave]]
    findPaths = walk False
      where
        walk :: Bool -> [Cave] -> Cave -> [Maybe [Cave]]
        walk b []   c@(Start cs)   = concat $ walk b [c] <$> cs
        walk _ _    (Start _)      = [Nothing]
        walk _ path End            = [Just path]
        walk b path c@(Big   cs _) = concat $ walk b (c:path) <$> cs
        walk b path c@(Small cs _)
          | elem c path  = bool (concat $ walk True (c:path) <$> cs) [Nothing] b
          | otherwise    = concat $ walk b (c:path) <$> cs



data Cave = Start [Cave]
          | Big   [Cave] String
          | Small [Cave] String
          | End

instance Eq Cave where
  (Start _)   == (Start _)   = True
  (Big   _ a) == (Big   _ b) = a == b
  (Small _ a) == (Small _ b) = a == b
  End         == End         = True
  _           == _           = False

label (Start _)     = "Start"
label (Big   _ lbl) = lbl
label (Small _ lbl) = lbl
label End           = "End!"


instance Show Cave where
  show (Start cs) = "Start: " ++ show (label <$> cs)
  show (End) = "End!"
  show (Big cs lbl) = lbl ++ ": " ++ show (label <$> cs)
  show (Small cs lbl) = lbl ++ ": " ++ show (label <$> cs)


-- | Parse the tree and return the Start cave.
-- parseCaveTree :: String -> Cave
parseCaveTree :: String -> Cave
parseCaveTree str = mapToTree
                  $ foldr link M.empty
                  $ parse (splitSome (char '\n') vertex) str
  where
    link (a, b) = link' (a, b) . link' (b, a)
      where
        link' (a', b') = M.insertWith S.union a' $ S.singleton b'

    vertex = do { from <- node; char '-'; to <- node; pure (from, to) }
    node   = some $ satisfy isAlphaNum


-- See https://wiki.haskell.org/Tying_the_Knot
mapToTree :: M.HashMap String (S.HashSet String) -> Cave
mapToTree strMap = getCave "start"
  where
    caveMap = M.mapWithKey mkCave strMap
    mkCave  lbl adj = parse (cave $ getCave <$> S.toList adj) lbl
    getCave lbl     = caveMap M.! lbl

    -- Parsers for the various cave types
    cave  cs = asum [start cs, end, small cs, big cs]
    start cs = symbol (Start cs) (string "start")
    end      = symbol End (string "end")
    big   cs = some (satisfy isAsciiUpper) >>= pure . Big   cs
    small cs = some (satisfy isAsciiLower) >>= pure . Small cs
