module Day13 (parts) where

import qualified Data.Map as M

import Util.Program
import Util.InstructionSet


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "306")
        , (part2, Just "15328")
        ]


part1 input = do screen <- program input >>= gameProgram
                 return . show . M.size $ M.filter (== Block) screen


part2 input = do score <- program input >>= withQuarters 2 >>= play
                 return . show $ score


withQuarters :: Int -> GameProgram -> IO GameProgram
withQuarters n prog = do { writeData (mem prog) 0 n; return prog }


play :: GameProgram -> IO Score
play prog = exec 0 $ load' prog M.empty
  where exec s proc = do (score, proc') <- runStateT (updateScreen s) proc
                         case action proc' of
                             Halt -> return score
                             _    -> exec score proc'


updateScreen :: Score -> GameRuntime Score
updateScreen score = do
    execute >> execute >> execute
    output <- pop
    case output of
        [-1,0,s] -> return s
        [x,y,t]  -> do scrn <- userState
                       modify $ \p -> p{ user = (M.insert (x,y) (toEnum t) scrn) }
                       return score
        _        -> return score


type Screen = M.Map (X,Y) Sprite

type GameProgram   = UserProgram Screen
type GameRuntime a = UserRuntime Screen a

data Sprite = Empty
            | Wall
            | Block
            | Paddle
            | Ball
  deriving (Enum, Eq, Ord)


data Tile = Tile { pos  :: (X, Y)
                 , sprite :: Sprite
                 }


instance Eq Tile where
    (Tile a _) == (Tile b _) = a == b


instance Ord Tile where
    (Tile (x,y) _) <= (Tile (x',y') _) = (y,x) <= (y',x')


type X     = Int
type Y     = Int
type Score = Int

gameProgram :: GameProgram -> IO Screen
gameProgram prog = do exec M.empty $ load' prog M.empty
  where exec es proc = do (es', proc') <- runStateT (buildSprites es) proc
                          case action proc' of
                              Halt -> return es'
                              _    -> exec es' proc'


buildSprites :: Screen -> GameRuntime Screen
buildSprites es = do execute >> execute >> execute
                     output <- pop
                     case output of
                         [x,y,t] -> return $ M.insert (x,y) (toEnum t) es
                         _       -> return es



program :: String -> IO GameProgram
program = (fmap $ UserProgram instructions) . parseRAM
  where instructions :: UserInstructionSet Screen
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
joy = mkInstruction 1 joy'
 where joy' assocs = do screen <- userState
                        dst    <- modeDest assocs 0
                        write dst $ joyDirection screen
                        relativeJump 2


-- | Return the unit which will move the paddle toward the ball's X location.
joyDirection screen = case uncurry compare (findX screen) of
                          LT -> 1
                          EQ -> 0
                          GT -> (-1)


-- | Return a tuple of the X locations of the @Paddle@ and @Ball@.
findX :: Screen -> (X,X)
findX = M.foldrWithKey query (0,0)
  where query (x,_) Paddle (_,b) = (x,b)
        query (x,_) Ball   (p,_) = (p,x)
        query _     _      prev  = prev
