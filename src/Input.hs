{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module : Input
--
-- Embed input files directly into the executable file.
module Input
  ( lookupInput
  ) where

import qualified Data.ByteString       as BS
import           Data.ByteString.UTF8  (toString)
import qualified Data.FileEmbed        as FE


inputsDir :: [(FilePath, BS.ByteString)]
inputsDir = $(FE.embedDir "inputs") -- TemplateHaskell syntax


lookupInput :: FilePath -> Maybe String
lookupInput fp = lookup fp inputsDir >>= pure . toString
