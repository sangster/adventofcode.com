module Y2019.Day25 (parts) where

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe

import Parser hiding (execStateT, runStateT)
import Util.InstructionSet
import Util.OpCode
import Util.Program hiding (name)

import Debug.Trace


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "278664")
        , (part2, Just "Merry Christmas!")
        ]


part1 input = runST $ do
    rooms <- mapShip input
    prog  <- program input
    show <$> findKeycode prog rooms (itemSequences rooms)
  where
    itemSequences = fmap sort . subsequences . filter safeItem . concatMap items
    program :: PrimMonad m => String -> m (Program' m)
    program = (fmap $ Program set') . parseRAM


execCommands :: PrimMonad m => Program' m -> [Command] -> m String
execCommands prog commands = do
    fmap chr <$> exec prog (concat $ command <$> commands)


part2 input = "Merry Christmas!"


findKeycode :: PrimMonad m => Program' m -> [Room] -> [[Item]] -> m Int
findKeycode _ _ [] = pure 99
findKeycode prog rooms (items:rest) = do
    dialogue <- execCommands prog steps
    if "Santa" `isInfixOf` dialogue
      then pure $ parse keypadCode dialogue
      else
        if (not $ checkpointRoomName `isInfixOf` dialogue) ||
           ("Droids on this ship are lighter" `isInfixOf` dialogue)
          then findKeycode prog rooms restLighter
          else findKeycode prog rooms rest

  where
    steps = compressSteps $ (concat $ takeAndReturn rooms <$> items) ++ final
    final = Go <$> pathToSanta rooms
    keypadCode :: Parser Int
    keypadCode = some (noneOf "1234567890") >> natural
    restLighter = filter (\r -> not $ items `isInfixOf` r) rest


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


mapShip :: PrimMonad m => String -> m [Room]
mapShip input = program input >>= explore
  where
    program :: PrimMonad m => String -> m (Program m (Search m))
    program = fmap (Program exploreSet) . parseRAM


safeItem = not . flip elem blacklist
  where
    blacklist =
      [ "infinite loop" -- A funny item which causes an actual infinite loop.
      ]


type Item = String
data Dir = North | South | East | West deriving Eq


opposite North = South
opposite South = North
opposite East  = West
opposite West  = East


instance Show Dir where
  show North = "N"
  show South = "S"
  show East  = "E"
  show West  = "W"


data Command
  = Go Dir
  | Take String
  | Drop String
  | Inv
  deriving Show

data Room = Room
  { name  :: String
  , doors :: [Dir]
  , back  :: Maybe (Dir, Room) -- Toward the entrance
  , items :: [Item]
  }

data Search m = Search
  { seen :: [Room]
  , next :: [(Process m (Search m), Dir)]
  , curr :: Maybe Room
  , move :: Maybe Dir
  }

instance Show Room where
  show room = concat $
    [ name room ++ " :: "
    , "["++(concat $ show <$> doors room)++"]"
    , maybe "" (\(d, r) -> " (back: "++show d++" -> "++name r++")") (back room)
    , bool "" (" items: " ++ (show $ items room)) (not . null $ items room)
    , " path: " ++ (concat $ show <$> pathToRoom room)
    ]


instance Eq Room where
  a == b = (name a) == (name b)


pathToEntrance, pathToRoom :: Room -> [Dir]
pathToEntrance = maybe [] (\(d, r) -> d:pathToEntrance r) . back
pathToRoom     = reverse . fmap opposite . pathToEntrance


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
    toS (Drop s) = "drop " ++ s
    toS Inv      = "inv"


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
set' =
  [ halt    "HALT" 99
  , math    " ADD"  1 (+)
  , math    "MULT"  2 (*)
  , mystore'   "STOR"  3
  , output  " OUT"  4
  , jump    " JEQ"  5 (/= 0)
  , jump    "JNEQ"  6 (== 0)
  , cmp     "  LT"  7 (<)
  , cmp     "  EQ"  8 (==)
  , newBase "BASE"  9
  ]

mystore' n = mkInstruction 1 store' n
  where
    store' assocs = do
      dat <- stdin <$> get
      out <- pop
      case dat of
        []     -> modify $ \p -> p{ action = Halt, stdout = out }
        (i:io) -> do dst <- modeDest assocs 0
                     write dst i
                     modify $ \p -> p{ stdin = io }
                     relativeJump 2


exploreSet :: PrimMonad m => InstructionSet m (Search m)
exploreSet =
  [ halt    "HALT" 99
  , math    " ADD"  1 (+)
  , math    "MULT"  2 (*)
  , mystore   "STOR"  3
  , myoutput  " OUT"  4
  , jump    " JEQ"  5 (/= 0)
  , jump    "JNEQ"  6 (== 0)
  , cmp     "  LT"  7 (<)
  , cmp     "  EQ"  8 (==)
  , newBase "BASE"  9
  ]


mystore n = mkInstruction 1 store' n
  where
    store' assocs = do
      dat <- stdin <$> get
      out <- pop

      when (null dat && (not $ null out)) $ parseRoom out

      case dat of
        []     -> cursor >>= \c -> (modify $ \p -> p{ action = Signal c })
        (i:io) -> do dst <- modeDest assocs 0
                     write dst i
                     modify $ \p -> p{ stdin = io }
                     relativeJump 2

    parseRoom out = do
      search <- userState
      let from = (,) <$> move search <*> curr search
      maybe (pure ()) continueSearch (parse (room from) (chr <$> out))


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


cpyProc :: PrimMonad m => Process m a -> m (Process m a)
cpyProc p = memcpy (mem $ prog p) >>= \m -> pure p{ prog = (prog p){ mem = m } }


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


myoutput :: PrimMonad m => String -> OpCode -> Instruction m a
myoutput = mkInstruction 1 output'
  where
    output' assocs = do
      dat <- modeRead assocs 0
      cur <- cursor
      out <- stdout <$> get

      modify $ \p -> p{ action = Run (cur + 2)
                      , stdout = out ++ [dat]
                      }
