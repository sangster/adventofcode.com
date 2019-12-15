{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List          (intercalate)
import System.Environment (getArgs)

import Days


main :: IO ()
main = do
    args <- getArgs
    case args of
        [year, "all"] -> appAllDays year
        [year, day]   -> appSingleDay year day
        _             -> error $ "expected onen argument, got " ++ show args


appAllDays :: String -> IO ()
appAllDays year = mapM_ renderDay (fst <$> parts)
  where
    renderDay (y,d) = do
        putStr $ y ++ "-12-" ++ d ++ "\n==========\n"
        appSingleDay year d
        putChar '\n'


appSingleDay :: String -> String -> IO ()
appSingleDay year day = maybe error' renderResults (callDay year day) >>= putStrLn
  where
    error' = return $ "source or input missing: " ++ year ++ "-12-" ++ day
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
                        , strip result
                        , "==========="
                        ]
  where
    shortEnough = length result < 70
    singleLine  = not $ elem '\n' result
    valid = maybe "?" isSuccess expected
    isSuccess ex = if result == ex then " " else "X"
    strip  []       = []
    strip ('\n':[]) = []
    strip (c:cc)    = c:(strip cc)
