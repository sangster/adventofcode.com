{-# LANGUAGE TemplateHaskell #-}
module Input where

import qualified Data.ByteString as BS
import Data.ByteString.UTF8 (toString)
import qualified Data.FileEmbed as FE


inputsDir :: [(FilePath, BS.ByteString)]
inputsDir = $(FE.embedDir "data/inputs")


lookupInput :: FilePath -> Maybe String
lookupInput filename =
    case maybeData of
      Just dat -> Just $ toString dat
      Nothing  -> Nothing
  where maybeData = (lookup filename inputsDir)
