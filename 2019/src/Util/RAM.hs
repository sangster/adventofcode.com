module Util.RAM
    ( RAM
    , Index
    , Data
    , parseRAM
    , readData
    , writeData
    , memcpy
    ) where

import Data.Array.IO

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
