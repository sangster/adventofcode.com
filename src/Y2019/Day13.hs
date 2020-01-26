module Y2019.Day13 (parts) where

import qualified Data.Map as M

import Util.Program
import Util.InstructionSet


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "306")
        , (part2, Just "15328")
        ]


part1 input = runST $ do
    screen <- program input >>= gameProgram
    pure . show . M.size $ M.filter (== Block) screen


part2 input = runST $ do
    score <- program input >>= withQuarters 2 >>= play
    pure . show $ score


withQuarters :: PrimMonad m => Int -> GameProgram m -> m (GameProgram m)
withQuarters n prog = do { writeData (mem prog) 0 n; pure prog }


play :: PrimMonad m
     => GameProgram m
     -> m Score
play prog = exec 0 $ load prog M.empty
  where exec s proc = do (score, proc') <- runStateT (gameState s) proc
                         act (pure score) (\_-> exec score proc') proc


gameState :: PrimMonad m
          => Score
          -> GameRuntime m Score
gameState score = do
    execute >> execute >> execute
    output <- pop
    case output of
        [-1,0,s] -> pure s
        [x,y,t]  -> do scrn <- userState
                       modify $ \p -> p{ user = (M.insert (x,y) (toEnum t) scrn) }
                       pure score
        _        -> pure score


type Screen = M.Map (X,Y) Sprite

type GameProgram m   = Program m Screen
type GameRuntime m a = Runtime m Screen a

data Sprite = Empty
            | Wall
            | Block
            | Paddle
            | Ball
  deriving (Enum, Eq, Ord)


data Tile = Tile { pos    :: (X, Y)
                 , sprite :: Sprite
                 }


instance Eq Tile where
    (Tile a _) == (Tile b _) = a == b


instance Ord Tile where
    (Tile (x,y) _) <= (Tile (x',y') _) = (y,x) <= (y',x')


type X     = Int
type Y     = Int
type Score = Int


gameProgram :: PrimMonad m
            => GameProgram m
            -> m Screen
gameProgram prog = exec M.empty $ load prog M.empty
  where exec es proc = do (es', proc') <- runStateT (buildSprites es) proc
                          act (pure es') (\_-> exec es' proc') proc'


buildSprites :: PrimMonad m
             => Screen
             -> GameRuntime m Screen
buildSprites es = do execute >> execute >> execute
                     output <- pop
                     case output of
                         [x,y,t] -> pure $ M.insert (x,y) (toEnum t) es
                         _       -> pure es


program :: PrimMonad m
        => String -> m (GameProgram m)
program = (fmap $ Program instructions) . parseRAM
  where instructions :: PrimMonad m => InstructionSet m Screen
        instructions = [ halt    "HALT" 99
                       , math    " ADD"  1 (+)
                       , math    "MULT"  2 (*)
                       , joy     " JOY"  3        -- Swap out STOR operation
                       , output  " OUT"  4
                       , jump    " JEQ"  5 (/= 0)
                       , jump    "JNEQ"  6 (== 0)
                       , cmp     "  LT"  7 (<)
                       , cmp     "  EQ"  8 (==)
                       , newBase "BASE"  9
                       ]


-- | An operation that replaces the normal STOR operation.
-- Instead of storing data from the state's FIFO, this version will store the
-- direction to move toward the ball
joy :: PrimMonad m => String -> Data -> Instruction m Screen
joy = mkInstruction 1 joy'
 where
   joy' assocs = do
     screen <- userState
     dst    <- modeDest assocs 0
     write dst $ joyDirection screen
     relativeJump 2


-- | Return the unit which will move the paddle toward the ball's X location.
joyDirection screen =
    case uncurry compare (findX screen) of
        LT -> 1
        EQ -> 0
        GT -> (-1)


-- | Return a tuple of the X locations of the @Paddle@ and @Ball@.
findX :: Screen -> (X,X)
findX = M.foldrWithKey query (0,0)
  where
    query (x,_) Paddle (_,b) = (x,b)
    query (x,_) Ball   (p,_) = (p,x)
    query _     _      prev  = prev
