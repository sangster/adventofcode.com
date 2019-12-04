{-# LANGUAGE TemplateHaskell #-}
module Input where

import qualified Data.ByteString as BS
import Data.ByteString.UTF8 (toString)
import qualified Data.FileEmbed as FE


inputsDir :: [(FilePath, BS.ByteString)]
inputsDir = $(FE.embedDir "data/inputs")


lookupInput :: FilePath -> Maybe String
lookupInput fp = lookup fp inputsDir >>= pure . toString
