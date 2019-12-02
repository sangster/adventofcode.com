{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List (intercalate)
import System.Environment (getArgs)

import Days (callDay)


main :: IO ()
main = do
    day <- (!! 0) <$> getArgs
    maybe (toError day) renderResults (callDay day) >>= putStrLn
  where
    toError day = return $ "Unknown day: '" ++ day ++ "'"
    renderResults results = reports >>= return . intercalate "\n"
      where reports = sequence $ (uncurry fmtIO) <$> (zip [1..] results)
    fmtIO n (ioResult, e) = ioResult >>= return . formatPart n e


formatPart :: Int
           -> Maybe String
           -> String
           -> String
formatPart number expected result
  | shortEnough && singleLine = valid ++ " " ++ show number ++ ": " ++ result
  | otherwise = unlines [ valid ++ " " ++ show number ++ ": ⏬"
                        , result
                        , "==========="
                        ]
  where
    shortEnough = length result < 70
    singleLine  = not $ elem '\n' result
    valid = maybe "?" isSuccess expected
    isSuccess ex = if result == ex then " " else "X"
