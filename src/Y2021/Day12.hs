module Y2021.Day12 (parts) where

import           Data.Bool (bool)
import           Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper)
import           Data.Foldable (asum)
import           Data.Maybe (catMaybes)
import           Parser
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

parts = ( (part1, Just "3679")
        , (part2, Just "107395")
        , parseCaveTree
        )


part1 :: Cave -> String
part1 start = show . length . catMaybes
            $ findPaths [] start
  where
    findPaths :: [Cave] -> Cave -> [Maybe [Cave]]
    findPaths []   c@(Start cs)   = concat $ findPaths [c] <$> cs
    findPaths _    (Start _)      = [Nothing]
    findPaths path End            = [Just path]
    findPaths path c@(Big   cs _) = concat $ findPaths (c:path) <$> cs
    findPaths path c@(Small cs _)
      | elem c path = [Nothing]
      | otherwise   = concat $ findPaths (c:path) <$> cs


part2 :: Cave -> String
part2 start = show . length . catMaybes
            $ findPaths [] start
  where
    findPaths :: [Cave] -> Cave -> [Maybe [Cave]]
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
          | End deriving Show


instance Eq Cave where
  (Start _)   == (Start _)   = True
  (Big   _ a) == (Big   _ b) = a == b
  (Small _ a) == (Small _ b) = a == b
  End         == End         = True
  _           == _           = False


-- | Parse the tree and return the Start cave.
parseCaveTree :: String -> Cave
parseCaveTree str = mapToTree
                  $ foldr link M.empty
                  $ parse (splitSome (char '\n') vertex) str
  where
    vertex = do { from <- node; to <- (char '-' >> node); pure (from, to) }
    node   = some $ satisfy isAlphaNum
    link (a,b) = f (a,b) . f (b,a)
      where
        f (x,y) = M.insertWith S.union x $ S.singleton y


-- See https://wiki.haskell.org/Tying_the_Knot
mapToTree :: M.HashMap String (S.HashSet String) -> Cave
mapToTree strMap = getCave "start"
  where
    getCave lbl     = caveMap M.! lbl
    caveMap         = M.mapWithKey mkCave strMap
    mkCave  lbl adj = parse (cave $ getCave <$> S.toList adj) lbl

    -- Parsers for the various cave types
    cave  cs = asum [start cs, end, small cs, big cs]
    start cs = symbol (Start cs) (string "start")
    end      = symbol End (string "end")
    big   cs = some (satisfy isAsciiUpper) >>= pure . Big   cs
    small cs = some (satisfy isAsciiLower) >>= pure . Small cs
