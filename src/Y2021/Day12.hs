module Y2021.Day12 (parts) where

import           Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper)
import           Data.Foldable (asum)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import           Data.Maybe (catMaybes)
import           Parser

parts = ( (part1, Just "3679")
        , (part2, Just "107395")
        , parseCaveTree
        )


part1 :: Cave -> String
part1 start = show . length . catMaybes $ allPaths (Part1Visitor []) start


part2 :: Cave -> String
part2 start = show . length . catMaybes $ allPaths (Part2Visitor [] False) start



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


class CaveState a where
  addCave :: a -> Cave -> a    -- ^ Add a new cave to the current path.
  atStart :: a -> Bool         -- ^ Are we still at the "start" cave?
  path    :: a -> [Cave]       -- ^ Return the path Travelled so far.
  inPath  :: a -> Cave -> Bool -- ^ Have we already visited this cave?

  -- | A function to call when we revisit a Small cave.
  revisitSmall :: a -> Cave -> [Maybe [Cave]]


data Part1Visitor = Part1Visitor [Cave]

instance CaveState Part1Visitor where
  addCave (Part1Visitor cs) c = Part1Visitor (c:cs)
  atStart (Part1Visitor cs)   = null cs
  path    (Part1Visitor cs)   = cs
  inPath  (Part1Visitor cs) c = elem c cs
  revisitSmall _ _ = [Nothing]


data Part2Visitor = Part2Visitor [Cave] -- ^ Caves visited so far.
                                 Bool   -- ^ Have we visited a Small twice?

instance CaveState Part2Visitor where
  addCave (Part2Visitor cs b) c = Part2Visitor (c:cs) b
  atStart (Part2Visitor cs _)   = null cs
  path    (Part2Visitor cs _)   = cs
  inPath  (Part2Visitor cs _) c = elem c cs

  revisitSmall (Part2Visitor path' False) c@(Small cs _) =
    concat $ allPaths (Part2Visitor (c:path') True) <$> cs
  revisitSmall _ _ = [Nothing]


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


-- | Return all paths through the cave system, including dead-ends. Use
--   catMaybes to filter only valid paths to "end".
allPaths :: CaveState a => a -> Cave -> [Maybe [Cave]]
allPaths s c@(Start cs) | atStart s = concat $ allPaths (addCave s c) <$> cs
                     | otherwise = [Nothing]
allPaths s End          = [Just $ path s]
allPaths s c@(Big   cs _) = concat $ allPaths (addCave s c) <$> cs
allPaths s c@(Small cs _) | inPath s c = revisitSmall s c
                       | otherwise  = concat $ allPaths (addCave s c) <$> cs
