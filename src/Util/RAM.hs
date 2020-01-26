module Util.RAM
  ( RAM
  , Index
  , Data
  , parseRAM
  , readData
  , writeData
  , memcpy
  , grow
  , memlen
  , allData
  , dumpRAM
  ) where


import           Control.Monad.Primitive
import           Data.List
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as U
import           Text.Printf

import Parser hiding (token)


type RAM s = M.MVector s Data
type Index = Int
type Data  = Int


-- | Convert the input file into a Read/Write buffer of memory.
parseRAM :: PrimMonad m
         => String
         -> m (RAM (PrimState m))
parseRAM input = do
    vec <- M.new $ length codes
    sequence $ uncurry (M.write vec) <$> zip [0..] codes
    pure vec
  where
    codes   = parse cells input
    cells   = many $ token cell
    cell    = (some $ satisfy (/= ',')) >>= pure . read
    token p = do { t <- p; many (char ','); pure t }


readData :: PrimMonad m
         => RAM (PrimState m)
         -> Index
         -> m Data
readData = M.read


writeData :: PrimMonad m
          => RAM (PrimState m)
          -> Index
          -> Data
          -> m ()
writeData = M.write


-- Increase the amount of available memory so that the given index can be
-- addressed.
--
-- Don't do this too often as we need to send an intern to BestBuy each time.
grow :: PrimMonad m
     => RAM (PrimState m)
     -> Index
     -> m (RAM (PrimState m))
grow src n = M.grow src $ n - len + 1
  where len = M.length src


memlen :: RAM s
       -> Int
memlen = M.length


memcpy :: PrimMonad m
       => RAM (PrimState m)
       -> m (RAM (PrimState m))
-- memcpy src = M.copy dst src
--   where
--     dst = M.new $ M.length src
memcpy src = do { dst <- M.new $ M.length src; M.copy dst src; pure dst }
-- TODO: memcpy = M.clone

allData :: PrimMonad m
        => RAM (PrimState m)
        -> m [Data]
allData src = U.freeze src >>= pure . U.toList


-- | Render the memory into rows and columns, with the given column width.
dumpRAM :: PrimMonad m
        => Int
        -> RAM (PrimState m)
        -> m String
dumpRAM w mem = do
    dat <- allData mem
    let rows = fmt <$> group w dat
    pure $ unlines $ uncurry label <$> zip [0..] rows
  where group _ [] = []
        group n xs = take n xs : group n (drop n xs)
        fmt ds = intercalate " " $ printf "% 6d" <$> ds
        label n row = (printf "% 4d: " $ n * w) ++ row
        label :: Int -> String -> String
