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

import Control.Monad
import Data.Bool
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as U
import Data.List
import Text.Printf
import Debug.Trace
import Util.Parser hiding (token)


type RAM   = M.IOVector Data
type Index = Int
type Data  = Int


-- | Convert the input file into a Read/Write buffer of memory.
parseRAM :: String
         -> IO RAM
parseRAM input = do vec <- M.new $ length codes
                    sequence $ uncurry (M.write vec) <$> zip [0..] codes
                    return vec
  where codes   = parse cells input
        cells   = many $ token cell
        cell    = (some $ satisfy (/= ',')) >>= return . read
        token p = do { t <- p; many (char ','); return t }


readData :: RAM
         -> Index
         -> IO Data
readData  = M.read


writeData :: RAM
          -> Index
          -> Data
          -> IO ()
writeData = M.write


grow :: RAM
     -> Index
     -> IO RAM
grow src n = M.grow src $ n - len + 1
  where len = M.length src


memlen :: RAM
       -> Int
memlen = M.length


memcpy :: RAM
       -> IO RAM
memcpy src = do { dst <- M.new $ M.length src; M.copy dst src; return dst }


allData :: RAM
        -> IO [Data]
allData src = U.freeze src >>= return . U.toList


-- | Render the memory into rows and columns, with the given column width.
dumpRAM :: Int
        -> RAM
        -> IO String
dumpRAM w mem = do dat <- allData mem
                   let rows = fmt <$> group w dat
                   return $ unlines $ uncurry label <$> zip [0..] rows

  where group _ [] = []
        group n xs = take n xs : group n (drop n xs)
        fmt ds = intercalate " " $ printf "% 6d" <$> ds
        label n row = (printf "% 4d: " $ n * w) ++ row
        label :: Int -> String -> String
