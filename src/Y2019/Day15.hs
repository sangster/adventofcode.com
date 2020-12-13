module Y2019.Day15 (parts) where

import qualified Data.HashMap.Strict as M
import           Data.Maybe
import           Data.Bool
import           Data.Default

import Util.InstructionSet
import Util.Program


parts = ( (part1, Just "354")
        , (part2, Just "370")
        , id
        )


part1 :: String -> String
part1 input = runST $ do
    status <- program input >>= dispatchDroid
    pure . show $ status
  where
    dispatchDroid :: PrimMonad m => Program m Search -> m Int
    dispatchDroid p = fmap fromJust $ evalStateT startSearch (load' p)
    startSearch :: PrimMonad m => StateT (Process m Search) m (Maybe Int)
    startSearch = do
        distances <- catMaybes <$> mapM (flip findOxygen 0) movements
        pure $ bool (Just $ minimum distances) Nothing $ null distances


part2 :: String -> String
part2 input = runST $ do
    shipMap <- program input >>= evalStateT startSearch . load'
    show <$> gasTime shipMap M.empty [queue shipMap] 0
  where
    queue = head . M.keys . M.filter (== System)
    startSearch :: PrimMonad m => StateT (Process m Search) m Seen
    startSearch = mapM_ mapShip movements >> seen <$> userState


gasTime :: PrimMonad m => Seen -> Seen -> [(X,Y)] -> Int -> m Int
gasTime _       _      []     acc = pure acc
gasTime shipMap gassed qq acc = do
    if M.size hollows == M.size gassed
        then pure $ acc
        else gasTime shipMap newGassed (qq >>= options) (succ acc)
  where
    hollows     = M.filter (flip elem [Open, Origin]) shipMap
    options   q = filter (not . flip M.member gassed) (neighbors q)
    neighbors q = filter (flip M.member hollows) (flip move q <$> movements)
    newGassed   = foldr (\loc -> M.insert loc $ (M.!) shipMap loc) gassed qq


mapShip :: PrimMonad m => Movement -> Runtime m Search Bool
mapShip dir = do
    status <- doMove dir

    case status of
        Blocked -> do setUserState $ \s -> s { loc = move (reverse' dir) (loc s) }
                      pure False
        _       -> do (Search here seen') <- userState
                      deadEnd <- not . or <$> mapM mapShip (unseen here seen')
                      case deadEnd of
                        True  -> doMove (reverse' dir) >> pure False
                        False -> pure True


findOxygen :: PrimMonad m => Movement -> Int -> Runtime m Search (Maybe Int)
findOxygen dir acc = do
    status <- doMove dir

    case status of
        Arrived -> do seen' <- seen <$> userState
                      --lift . putStrLn $ Draw.showHashMap Unknown seen'
                      pure . Just . succ $ acc
        Blocked -> do setUserState $ \s -> s { loc = move (reverse' dir) (loc s) }
                      pure Nothing
        Moved   -> do (Search here seen') <- userState
                      distances <- catMaybes <$> mapM (flip findOxygen $ acc+1)
                                                      (unseen here seen')
                      case null distances of
                        True  -> doMove (reverse' dir) >> pure Nothing
                        False -> pure $ Just (minimum distances)


doMove :: PrimMonad m => Movement -> Runtime m Search (Status)
doMove dir = do
    push $ fromEnum dir
    execute
    status <- toEnum . head <$> pop
    setUserState $ update (fromStatus status)
    pure status
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


program :: PrimMonad m => String -> m (Program m Search)
program = (fmap $ Program aoc19Set) . parseRAM
