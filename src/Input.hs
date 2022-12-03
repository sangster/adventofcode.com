{-# LANGUAGE ImportQualifiedPost, TemplateHaskell #-}

-- |
-- Module : Input
--
-- Embed input files directly into the executable file.
module Input
  ( lookupInput
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.UTF8 (toString)
import Data.FileEmbed qualified as FE


inputsDir :: [(FilePath, BS.ByteString)]
inputsDir = $(FE.makeRelativeToProject "inputs" >>= FE.embedDir)


lookupInput :: FilePath -> Maybe String
lookupInput fp = toString <$> lookup fp inputsDir
