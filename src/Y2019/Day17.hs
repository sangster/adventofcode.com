{-# LANGUAGE BangPatterns #-}
module Y2019.Day17 (parts) where

import           Data.Bool
import           Data.Char
import           Data.List
import           Data.List.Split   (chunksOf)
import qualified Data.Vector       as V
import           Data.Maybe
import           Prelude           hiding (Left, Right)

import Util.InstructionSet
import Util.Program


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "5740")
        , (part2, Just "1022165")
        ]


part1 input = fmap (show . sum)
            $ return . alignments
            =<< parseMap input []


-- | The "alignments" for every intersection in the map.
alignments :: Map -> [Int]
alignments map' = alignment <$> filter (isIntersection map') coords'
  where
    alignment = uncurry (*)
    coords'   = coords map'


part2 input = do
    -- Parse Map and create movement orders.
    m <- parseMap input []
    let solution = renderCommands . extractFunctions $ path m

    -- Execute movement orders.
    prog <- program input
    writeData (mem prog) 0 2
    show <$> executeUntilHalt' prog (ord <$> solution)
  where
    extractFunctions p =
      ( fromJust $ functionSequence p comps
      , comps
      ) where comps = components p

    renderCommands (indicies, functions) =
        concat $ mainRoutine:"\n":(functions >>= command):[videoFeed:"\n"]
      where
        mainRoutine  = intercalate "," routineNames
        routineNames = return . chr . (ord 'A' +) <$> indicies
        videoFeed    = 'n'


data Dir  = Up | Down | Left | Right   deriving Eq
data Move = CCW | CW | Forward Int     deriving Eq


instance Show Move where
  show CCW         = "L"
  show CW          = "R"
  show (Forward n) = show n


-- | @Map@
--
-- A two-dimensional vector of @Tiles@.
--------------------------------------------------------------------------------
data Map = Map
  { tiles :: V.Vector Tile
  , width :: Int
  }


-- | Return a @Map@, a two dimensional vector), as represented by the ASCII
--   drawing rendered by the given IntCode program.
parseMap :: String -> [Data] -> IO Map
parseMap input io = do
  cc <- fmap chr <$> (program input >>= (flip executeUntilHalt) io)

  return $ Map
    { tiles = V.fromList (readTile <$> (filter (/= '\n') cc))
    , width = fromJust $ findIndex (== '\n') cc
    }


-- | The height of the @Map@.
height :: Map -> Int
height m = V.length (tiles m) `div` width m


-- | Every (X,Y) coordinate in the @Map@.
coords m = [(x,y) | y <- [0 .. (height m) - 1], x <- [0.. (width m) - 1]]


-- | Is the given coordinate occupied by a @Scaffold@.
isIntersection m (x,y) = all isScaffold [(0,0), (-1,0), (0,-1), (1,0), (0,1)]
  where
    isScaffold (x',y') = m `idx` (x+x', y+y') == Scaffold


-- | The @Tile@ contained at the given (X,Y) coordinate.
m `idx` (x,y) = fromMaybe Open $ (tiles m) V.!? ((y * width m) + x)


-- | The @Tile@ in the @Map@ a given distance away.
tileInDir m dir n (x,y) = m `idx` (forward dir n (x,y))


-- | @Tile@
--
-- The units which occupy X,Y positions in the @Map@.
--------------------------------------------------------------------------------

data Tile
  = Open                   -- ^ Untraversable space.
  | Scaffold               -- ^ Part of the bath the bot can move on.
  | Bot { botDir :: Dir }  -- ^ The Bot, with its direction.
  deriving Eq


isBot (Bot _) = True
isBot _       = False


-- | Return the given list in the "command string" format.
command :: Show a => [a] -> String
command mm = intercalate "," (show <$> mm) ++ "\n"


-- | Convert a @Char@ into the @Tile@ it represents.
readTile '.' = Open
readTile '#' = Scaffold
readTile '^' = Bot Up
readTile 'v' = Bot Down
readTile '<' = Bot Left
readTile '>' = Bot Right


-- | Extract a path from the bot's current location that will traverse every
--   @Scaffold@ in the @Map@.
path :: Map -> [Move]
path m = path' (coords m !! botIndex) (botDir $ tiles m V.! botIndex) []
  where
    botIndex  = fromJust $ V.findIndex isBot (tiles m)
    scaffolds = filter ((Scaffold ==) . idx m) $ coords m

    path' (x,y) dir seen = maybe [] followPath $ findCandidate neighbors
      where
        findCandidate = listToMaybe
                      . filter (\c -> elem c scaffolds && not (elem c seen))
        neighbors = (\d -> forward d 1 (x,y)) <$> [Up, Down, Left, Right]

        followPath next = [rotate dir turnDir, Forward forDist] ++ rest
          where
            turnDir = dirOf (x,y) next

            forDist = pred . fromJust $ find isOpenDistance [1..]
            isOpenDistance = (== Open) . flip (tileInDir m turnDir) (x,y)

            rest = path' (forward turnDir forDist (x,y)) turnDir (seen ++ seen')
            seen' = (\n -> forward turnDir n (x,y)) <$> [1 .. forDist]



-- | The coordinates after moving in the given direction.
forward Up    n (x,y) = (x  , y-n)
forward Down  n (x,y) = (x  , y+n)
forward Left  n (x,y) = (x-n, y  )
forward Right n (x,y) = (x+n, y  )


-- | The @Turn@ required to change directions.
rotate Up    Right = CW
rotate Up    Left  = CCW
rotate Down  Left  = CW
rotate Down  Right = CCW
rotate Left  Up    = CW
rotate Left  Down  = CCW
rotate Right Down  = CW
rotate Right Up    = CCW


-- | The direction to the second from first location.
dirOf (x,y) (x',y')
    | x < x' = Right
    | x > x' = Left
    | y < y' = Down
    | y > y' = Up
    | otherwise = error "same location"


program :: String -> IO Program'
program = fmap (Program aoc19Set) . parseRAM


-- | Can the a list be built with these components? If so return the sequence of
--   indicies.
functionSequence :: (Show a, Eq a)
                 => [a]
                 -> [[a]]
                 -> Maybe [Int]
functionSequence []  _     = Just []
functionSequence !xs comps = maybe Nothing seq
                           $ findIndex (flip isPrefixOf xs) comps
  where
    seq   idx = (idx:) <$> functionSequence (xsOff idx) comps
    xsOff idx = drop (length $ comps !! idx) xs


-- | Return the sublists which occur more than once in the given list.
components :: Eq a => [a] -> [[a]]
components [] = []
components xs = bool [] (pre : (components $ remove pre xs)) $ (not . null) pre
  where
    pre = longestPrefix xs
    remove _ [] = []
    remove w s@(c:cs) | w `isPrefixOf` s = remove w (drop (length w) s)
                      | otherwise        = c : remove w cs


-- | The longest prefix that occurs in the list at lest twice.
longestPrefix :: Eq a => [a] -> [a]
longestPrefix [] = []
longestPrefix xs = fromMaybe [] $ find isRepeat prefixes
  where
    prefixes   = reverse . inits $ xs
    count sub  = length . catMaybes . map (stripPrefix sub) . tails
    isRepeat p = (count p $ drop (length p) xs) > 0
