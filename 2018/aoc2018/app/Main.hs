{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import Days (callDay)


main :: IO ()
main = do
    day <- (!! 0) <$> getArgs
    putStrLn $
        case callDay day of
            Just results -> intercalate "\n"
                                $ uncurry formatPart <$> zip [1..] results
            Nothing      -> "Unknown day: '" ++ day ++ "'"


formatPart number result
  | shortEnough && singleLine = show number ++ ": " ++ result
  | otherwise = unlines [ "==========="
                        , show number
                        , "==========="
                        , result
                        ]
  where
    shortEnough = length result < 70
    singleLine  = not $ elem '\n' result
