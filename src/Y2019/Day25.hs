module Y2019.Day25 (parts) where

import Data.Bool
import Data.Char
import Data.Function
import Data.List
import Data.Maybe

import Parser hiding (execStateT, runStateT)
import Util.InstructionSet
import Util.OpCode
import Util.Program hiding (name)


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "278664")
        , (part2, Just "Merry Christmas!")
        ]


-- | Strategy:
--     1. Maps the ship and where all the items are.
--     2. Create a list of all permutations of the items.
--     3. Try to get past the Security Checkpoint with each permutation,
--        filtering out permutations that contain sublists that are already too
--        heavy.
part1 input = runST $ do
    rooms <- mapShip input
    prog  <- program input
    findKeycode prog rooms (itemSequences rooms)
  where
    program :: PrimMonad m => String -> m (Program' m)
    program       = fmap (Program set') . parseRAM
    itemSequences = fmap sort . subsequences . filter safeItem . concatMap items


part2 _ = "Merry Christmas!"


-- | Map the rooms of the ship, and the items they contain.
mapShip :: PrimMonad m => String -> m [Room]
mapShip input = program input >>= explore
  where
    program :: PrimMonad m => String -> m (Program m (Search m))
    program = fmap (Program exploreSet) . parseRAM


-- | Trying different combinations of items, find the correct weight needed to
-- pass the Security Checkpoint.
findKeycode :: PrimMonad m => Program' m -> [Room] -> [[Item]] -> m String
findKeycode _    _     []           = pure ""
findKeycode prog rooms (items:rest) = do
    dialogue <- execCmd steps
    if "Santa" `isInfixOf` dialogue
      then pure . show $ parse keypadCode dialogue
      else findKeycode prog rooms (rest' dialogue)
  where
    execCmd cmds = fmap chr <$> exec prog (concat $ command <$> cmds)
    steps        = compressSteps $ concat stepsFull ++ final
    stepsFull    = sortBy (compare `on` length) $ takeAndReturn rooms <$> items
    final        = Go <$> pathToSanta rooms
    keypadCode   :: Parser Int
    keypadCode   = some (noneOf "1234567890") >> natural
    restLighter  = filter (\r -> not $ items `isInfixOf` r) rest
    rest'        = bool rest restLighter . tooHeavy
    tooHeavy str = not (checkpointRoomName `isInfixOf` str) ||
                   "Droids on this ship are lighter" `isInfixOf` str


-- | Minimise the number of steps the droid must take by reducing the number of
-- times the bot backtraces between previously visited rooms.
compressSteps :: [Command] -> [Command]
compressSteps steps = compress [] steps
  where
    compress :: [Command] -> [Command] -> [Command]
    compress xs     []     = reverse xs
    compress []     (y:ys) = compress [y] ys
    compress (x:xs) (y:ys) =
        case y of
          Go dir ->
            case x of
              Go dir' -> bool rest (compress xs ys) $ dir' == opposite dir
              _       -> rest
          _      -> rest
      where
        rest = compress (y:x:xs) ys


safeItem = not . flip elem blacklist
  where
    blacklist =
      [ "infinite loop" -- A funny item which causes an actual infinite loop.
      ]


type Item    = String
data Dir     = North | South | East | West deriving (Eq, Show)
data Command = Go Dir | Take String


data Room = Room
  { name  :: String
  , doors :: [Dir]
  , back  :: Maybe (Dir, Room) -- Toward the entrance
  , items :: [Item]
  } deriving Show


data Search m = Search
  { seen :: [Room]
  , next :: [(Process m (Search m), Dir)]
  , curr :: Maybe Room
  , move :: Maybe Dir
  }


instance Eq Room where
  (==) = (==) `on` name
  -- a == b = (name a) == (name b)


opposite North = South
opposite South = North
opposite East  = West
opposite West  = East


pathToRoom :: Room -> [Dir]
pathToRoom = reverse . fmap opposite . pathToEntrance
  where pathToEntrance = maybe [] (\(d, r) -> d:pathToEntrance r) . back


pathToItem :: [Room] -> Item -> [Dir]
pathToItem rooms it = pathToRoom . fromJust $ find (elem it . items) rooms


pathToSanta :: [Room] -> [Dir]
pathToSanta rooms = path ++ [forwardDir]
  where
    path       = pathToRoom checkpoint
    checkpoint = fromJust $ find ((== checkpointRoomName) . name) rooms
    backDir    = fst . fromJust $ back checkpoint
    forwardDir = head . delete backDir $ doors checkpoint

takeAndReturn :: [Room] -> Item -> [Command]
takeAndReturn rooms it = (Go <$> forward) ++ [Take it] ++ (Go <$> backward)
  where
    forward  = pathToItem rooms it
    backward = reverse $ opposite <$> forward

command :: Command -> [Data]
command cmd = ord <$> (toS cmd ++ "\n")
  where
    toS (Go North)    = "north"
    toS (Go South)    = "south"
    toS (Go East)     = "east"
    toS (Go West)     = "west"
    toS (Take s) = "take " ++ s


checkpointRoomName = "Security Checkpoint"


exec :: PrimMonad m => Program' m -> [Data] -> m [Data]
exec prog' io' = loop prog' io' [] 0 0
  where
    loop p i o c b = do
        (dat, proc') <- runStateT (execute >> stdout <$> get) proc
        act (pure dat) (rerun (dat, proc')) proc'
      where
        proc = (load' p){ action = Run c, stdin = i, stdout = o, base = b }

    rerun (dat, proc') c = loop (prog proc') (stdin proc') dat c (base proc')


explore :: PrimMonad m => Program m (Search m) -> m [Room]
explore prog' = exploreLoop $ load prog' search
  where
    search = Search
      { seen = []
      , next = []
      , curr = Nothing
      , move = Nothing
      }


exploreLoop :: PrimMonad m => Process m (Search m) -> m [Room]
exploreLoop p = do
    proc <- execStateT execute p
    case action proc of
      Run _ -> exploreLoop  proc
      _     -> handleSignal proc

  where
    handleSignal proc = do
      let next' = next $ user proc

      if not $ null next'
        then do
          let ((proc', dir):rest) = next'
          exploreLoop $ proc'
            { stdin = command $ Go dir
            , user = (user proc'){ next = rest, move = Just dir, seen = (joinSeen proc proc') }
            }
        else pure $ seen (user proc)


joinSeen a b = nub $ (seen $ user a) ++ (seen $ user b)


set' :: PrimMonad m => InstructionSet m a
set' = mergeSets aoc19Set [mystore' "STOR" 3]
  where
    mystore' n = mkInstruction 1 store' n
      where
        store' assocs = do
          dat <- stdin <$> get
          out <- pop
          case dat of
            []     -> modify $ \p -> p{ action = Halt, stdout = out }
            (i:io) -> do modeDest assocs 0 >>= flip write i
                         modify $ \p -> p{ stdin = io }
                         relativeJump 2


-- | An instruction set used to explore Santa's ship and extract the Rooms
-- discovered along the way.
exploreSet :: PrimMonad m => InstructionSet m (Search m)
exploreSet = mergeSets aoc19Set [storeRoom "STOR" 3, outputAndContinue " OUT" 4]
  where
    storeRoom n = mkInstruction 1 store' n
      where
        store' assocs = do
          pop >>= \out -> unless (null out) (parseRoom out)

          dat <- stdin <$> get
          case dat of
            []     -> cursor >>= \c -> modify $ \p -> p{ action = Signal c }
            (i:io) -> do modeDest assocs 0 >>= flip write i
                         modify $ \p -> p{ stdin = io }
                         relativeJump 2

        parseRoom out = do
          search <- userState
          let from = (,) <$> move search <*> curr search
          maybe (pure ()) continueSearch $ parse (room from) (chr <$> out)

    outputAndContinue = mkInstruction 1 output'
      where
        output' assocs = do
          cur <- cursor
          dat <- modeRead assocs 0
          out <- stdout <$> get

          modify $ \p -> p{ action = Run (cur + 2), stdout = out ++ [dat] }


continueSearch :: PrimMonad m => Room -> Runtime m (Search m) ()
continueSearch room = userState >>= \s -> unless (elem room $ seen s) (continue room)


continue :: PrimMonad m => Room -> Runtime m (Search m) ()
continue room = do
    modify $ \p -> p{ user = (user p){ curr = Just room, seen = room:(seen $ user p) } }

    proc' <- get
    let search = user proc'

    dirs <- lift . sequence $ nextSearch proc' <$> forwardDoors search -- doors room

    let next' = bool (dirs ++ next search) (next search)
              $ name room == checkpointRoomName
        search' = search{ next = next' }

    modify $ \p -> p{ user = search' }
  where
    nextSearch p door = cpyProc p >>= pure . flip (,) door
    forwardDoors search = maybe dirs (flip delete dirs . fst) $ back room
      where dirs = doors room


-- | Parse a Room record from its description.
room :: Maybe (Dir, Room) -> Parser (Maybe Room)
room from = (command >> pure Nothing) <|> room' from
  where
    command = spaces >> reserved "Command?"

    room' from = do
      n <- name
      description
      d <- doors
      let b = Nothing
      i <- items <|> pure []
      command

      pure . Just $ Room
        { name = n
        , doors = d
        , back = maybe Nothing (\(d, r) -> Just (opposite d, r)) from
        , items = i
        }

    name = do
      spaces
      reserved "=="
      n <- some $ satisfy (/= '=')
      reserved "=="
      spaces
      pure $ init n

    description = do
      desc <- many $ satisfy (/= '\n')
      spaces
      pure desc

    doors = reserved "Doors here lead:" >> many dir
      where
        dir = do
          reserved "-"
          d <- north <|> south <|> east <|> west
          spaces
          pure d

        north = reserved "north" >> pure North
        south = reserved "south" >> pure South
        east  = reserved "east"  >> pure East
        west  = reserved "west"  >> pure West

    items = reserved "Items here:" >> many item
      where
        item = do
          reserved "-"
          i <- some $ satisfy (/= '\n')
          spaces
          pure i
