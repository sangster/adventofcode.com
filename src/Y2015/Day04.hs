module Y2015.Day04 (parts) where

import           Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import           Data.List (find)
import           Data.Maybe (fromJust)


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "117946")
        , (part2, Just "3938038")
        ]


part1 input = show . fromJust $ find (fiveZeros . md5 . key) [0..]
  where
    key i = input ++ show i

    fiveZeros ('0':'0':'0':'0':'0':_) = True
    fiveZeros _                       = False


part2 input = show . fromJust $ find (sixZeros . md5 . key) [0..]
  where
    key i = input ++ show i

    sixZeros ('0':'0':'0':'0':'0':'0':_) = True
    sixZeros _                           = False


md5 :: String -> String
md5 = BS.unpack . digestToHexByteString . hash' . BS.pack
  where
    hash' s = hash s :: Digest MD5
