module Day11 (parts) where

import qualified Data.Map.Strict   as M
import           Data.Bool  (bool)
import           Data.List
import           Prelude    hiding (Left, Right)

import Util.InstructionSet
import Util.Program

import Debug.Trace

parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "2276")
        , (part2, Just part2Expected)
        ]

part2Expected = unlines [ "  ██  ███  █    ███    ██ ████  ██  █  █   "
                        , " █  █ █  █ █    █  █    █    █ █  █ █  █   "
                        , " █    ███  █    █  █    █   █  █    █  █   "
                        , " █    █  █ █    ███     █  █   █    █  █   "
                        , " █  █ █  █ █    █    █  █ █    █  █ █  █   "
                        , "  ██  ███  ████ █     ██  ████  ██   ██    "
                        ]


part1 input = do colors <- program input >>= dispatchBot M.empty
                 return . show $ M.size colors


part2 input = do colors <- program input >>= dispatchBot map
                 return $ draw colors
  where map = M.insert (0,0) White M.empty


type X = Int
type Y = Int
type ColorMap = M.Map (X, Y) Color


data Robot = Robot { proc' :: Process
                   , dir   :: Direction
                   , loc   :: (X,Y)
                   }


data Color = Black
           | White
    deriving Eq


instance Show Color where
    show White = "█"
    show _     = " "


data Direction = North
               | East
               | South
               | West
    deriving Show


data Turn = Left
          | Right
    deriving Show


dispatchBot :: ColorMap -> Program -> IO ColorMap
dispatchBot m p = bot m Robot{ proc' = load p, dir = North, loc = (0,0) }
  where bot map b =
            do ((c, t), proc'') <- runStateT runBot (proc' b){ fifo = [input map b] }
               case action proc'' of
                   Halt -> return map
                   _    -> do let map' = M.insert (loc b) c map
                              bot map' (move . (turn t) $ b{ proc' = proc'' })
        input map b    = bool 0 1 $ White == colorAt map (loc b)
        colorAt cm key = maybe Black id $ M.lookup key cm


runBot :: Runtime (Color, Turn)
runBot = do dat <- do { execute; execute }
            return (parseColor $ head dat, parseTurn $ head . tail $ dat)
  where
    parseColor 0 = Black
    parseColor _ = White
    parseTurn  0 = Left
    parseTurn  _ = Right


turn :: Turn -> Robot -> Robot
turn Left  r@Robot{ dir = North } = r{ dir = West  }
turn Left  r@Robot{ dir = East  } = r{ dir = North }
turn Left  r@Robot{ dir = South } = r{ dir = East  }
turn Left  r@Robot{ dir = West  } = r{ dir = South }
turn Right r@Robot{ dir = North } = r{ dir = East  }
turn Right r@Robot{ dir = East  } = r{ dir = South }
turn Right r@Robot{ dir = South } = r{ dir = West  }
turn Right r@Robot{ dir = West  } = r{ dir = North }


move :: Robot -> Robot
move r@Robot{ loc = (x,y), dir = North } = r{ loc = (x  , y+1) }
move r@Robot{ loc = (x,y), dir = East  } = r{ loc = (x+1, y  ) }
move r@Robot{ loc = (x,y), dir = South } = r{ loc = (x  , y-1) }
move r@Robot{ loc = (x,y), dir = West  } = r{ loc = (x-1, y  ) }


draw :: ColorMap -> String
draw colors = unlines lines
  where getC x y  = maybe Black id $ M.lookup (x,y) colors
        lines  = reverse [(concat) [show (getC x y) | x <- xrange] | y <- yrange]
        xrange = [xMin .. xMax]
        yrange = [yMin .. yMax]
        (xMin, yMin, xMax, yMax) = foldr (\c b -> bounds b c) (0,0,0,0) (M.keys colors)
        bounds (xMin', yMin', xMax', yMax') (x', y') = ( minimum [xMin', x']
                                                       , minimum [yMin', y']
                                                       , maximum [xMax', x']
                                                       , maximum [yMax', y']
                                                       )


program :: String -> IO (UserProgram ())
program = (fmap $ UserProgram instructions) . parseRAM
  where instructions :: UserInstructionSet ()
        instructions = [ halt    "HALT" 99
                       , math    " ADD"  1 (+)
                       , math    "MULT"  2 (*)
                       , store   "STOR"  3
                       , output  " OUT"  4
                       , jump    " JEQ"  5 (/= 0)
                       , jump    "JNEQ"  6 (== 0)
                       , cmp     "  LT"  7 (<)
                       , cmp     "  EQ"  8 (==)
                       , newBase "BASE"  9
                       ]
