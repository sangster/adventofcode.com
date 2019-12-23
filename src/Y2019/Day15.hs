module Y2019.Day15 (parts) where

import qualified Data.HashMap.Strict as M
import           Data.Maybe
import           Data.Bool
import           Data.Default

import Util.InstructionSet
import Util.Program


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "354")
        , (part2, Just "370")
        ]


part1 input = do status <- program input >>= dispatchDroid
                 return . show $ status
  where
    dispatchDroid p = fmap fromJust $ evalStateT startSearch (load' p)
    startSearch = do
        distances <- catMaybes <$> mapM (flip findOxygen 0) movements
        return $ bool (Just $ minimum distances) Nothing $ null distances


part2 input = do shipMap <- program input >>= dispatchDroid
                 show <$> gasTime shipMap M.empty [queue shipMap] 0
  where
    queue = head . M.keys . M.filter (== System)
    dispatchDroid p = evalStateT startSearch (load' p)
    startSearch = do
        mapM_ mapShip movements
        seen <$> userState


gasTime :: Seen -> Seen -> [(X,Y)] -> Int -> IO Int
gasTime _       _      []     acc = return acc
gasTime shipMap gassed qq acc = do
    if M.size hollows == M.size gassed
        then return $ acc
        else gasTime shipMap newGassed (qq >>= options) (succ acc)
  where
    hollows     = M.filter (flip elem [Open, Origin]) shipMap
    options   q = filter (not . flip M.member gassed) (neighbors q)
    neighbors q = filter (flip M.member hollows) (flip move q <$> movements)
    newGassed   = foldr (\loc -> M.insert loc $ (M.!) shipMap loc) gassed qq


mapShip :: Movement -> Runtime Search Bool
mapShip dir = do
    status <- doMove dir

    case status of
        Blocked -> do setUserState $ \s -> s { loc = move (reverse' dir) (loc s) }
                      return False
        _       -> do (Search here seen') <- userState
                      deadEnd <- not . or <$> mapM mapShip (unseen here seen')
                      case deadEnd of
                        True  -> doMove (reverse' dir) >> return False
                        False -> return True


findOxygen :: Movement -> Int -> Runtime Search (Maybe Int)
findOxygen dir acc = do
    status <- doMove dir

    case status of
        Arrived -> do seen' <- seen <$> userState
                      --lift . putStrLn $ Draw.showHashMap Unknown seen'
                      return . Just . succ $ acc
        Blocked -> do setUserState $ \s -> s { loc = move (reverse' dir) (loc s) }
                      return Nothing
        Moved   -> do (Search here seen') <- userState
                      distances <- catMaybes <$> mapM (flip findOxygen $ acc+1)
                                                      (unseen here seen')
                      case null distances of
                        True  -> doMove (reverse' dir) >> return Nothing
                        False -> return $ Just (minimum distances)


doMove :: Movement -> Runtime Search (Status)
doMove dir = do
    push $ fromEnum dir
    execute
    status <- toEnum . head <$> pop
    setUserState $ update (fromStatus status)
    return status
  where
    update status s = s{ loc = here, seen = M.insert here status (seen s) }
      where here = move dir (loc s)


type X = Int
type Y = Int


data Search = Search
  { loc :: (X,Y)
  , seen :: Seen
  }


instance Default Search where
  def = Search
    { loc = (0,0)
    , seen = M.fromList [((0,0), Origin)]
    }


type Seen = M.HashMap (X,Y) Tile


data Movement = North | South | West | East  deriving (Bounded, Show)


instance Enum Movement where
  toEnum 1 = North
  toEnum 2 = South
  toEnum 3 = West
  toEnum 4 = East

  fromEnum North = 1
  fromEnum South = 2
  fromEnum West  = 3
  fromEnum East  = 4


movements        = [minBound .. maxBound]
unseen here seen = filter (\m -> not $ M.member (move m here) seen) movements


move North (x,y) = (x  , y-1)
move South (x,y) = (x  , y+1)
move West  (x,y) = (x-1, y  )
move East  (x,y) = (x+1, y  )


reverse' North = South
reverse' South = North
reverse' West  = East
reverse' East  = West


data Status = Blocked | Moved | Arrived                             deriving Enum
data Tile   = Unknown | Gas | Open | Wall | System | Origin | Droid deriving Eq


fromStatus Blocked = Wall
fromStatus Moved   = Open
fromStatus Arrived = System


program :: String -> IO (Program Search)
program = (fmap $ Program aoc19Set) . parseRAM
