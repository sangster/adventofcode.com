module Y2019.Day25 (parts) where

import Data.Char

import Parser hiding (runStateT)
import Util.InstructionSet
import Util.Program

parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "278664")
        , (part2, Just "Merry Christmas!")
        ]


-- TODO: automate this lol
part1 input = show $ runST $ do
    prog     <- program input
    dialogue <- fmap chr <$> exec prog stdin'
    pure . head $ parse keypadCode dialogue
  where
    stdin' = concat . fmap command $
      [ North, South, South      -- To Sick Bay
      , Take "fuel cell"
      , North                    -- Back to Hull Breach
      , West                     -- To Hot Chocolate Fountain
      , Take "mutex"
      , South, South             -- To Corridor
      , Take "coin"
      , North, East              -- To Science Lab
      , Take "cake"
      , North, West, South, West -- To Pressure-Sensitive Floor
      ]

    keypadCode = some (noneOf "1234567890") >> many natural


part2 input = "Merry Christmas!"


data Command
  = North
  | South
  | East
  | West
  | Take String
  | Drop String
  | Inv


command :: Command -> [Data]
command cmd = ord <$> (toS cmd ++ "\n")
  where
    toS North    = "north"
    toS South    = "south"
    toS East     = "east"
    toS West     = "west"
    toS (Take s) = "take " ++ s
    toS (Drop s) = "drop " ++ s
    toS Inv      = "inv"


program :: PrimMonad m => String -> m (Program' m)
program = (fmap $ Program aoc19Set) . parseRAM


exec :: PrimMonad m => Program' m -> [Data] -> m [Data]
exec prog' io' = loop prog' io' [] 0 0
  where
    loop p i o c b = do
        (dat, proc') <- runStateT (execute >> stdout <$> get) proc
        act (pure dat) (rerun (dat, proc')) proc'
      where
        proc = (load' p){ action = Run c, stdin = i, stdout = o, base = b }

    rerun (dat, proc') c = loop (prog proc') (stdin proc') dat c (base proc')
