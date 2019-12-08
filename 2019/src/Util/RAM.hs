module Util.RAM
    ( RAM
    , Index
    , Data
    , parseRAM
    , readData
    , writeData
    , memcpy
    , allData
    , dumpRAM
    ) where


import Data.Array.IO
import Data.List
import Text.Printf

import Util.Parser hiding (token)


type RAM   = IOUArray Index Data
type Index = Int
type Data  = Int


-- | Convert the input file into a Read/Write buffer of memory.
parseRAM :: String
         -> IO RAM
parseRAM input = newListArray (0, length codes - 1) codes
  where codes   = runParser cells input
        cells   = many $ token cell
        cell    = (some $ satisfy (/= ',')) >>= return . read
        token p = do { t <- p; many (char ','); return t }


readData :: RAM
         -> Index
         -> IO Data
readData  = readArray


writeData :: RAM
          -> Index
          -> Data
          -> IO ()
writeData = writeArray


memcpy :: RAM
       -> IO RAM
memcpy src = do { b <- getBounds src; e <- getElems src; newListArray b e }


allData :: RAM
        -> IO [Data]
allData = getElems


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
