{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List (intercalate)
import System.Environment (getArgs)

import Days


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["all"] -> appAllDays
        [day]   -> appSingleDay day
        _       -> error $ "expected onen argument, got " ++ show args


appAllDays :: IO ()
appAllDays = mapM_ renderDay (fst <$> days)
  where renderDay d = do putStr $ "Day " ++ d ++ "\n======\n"
                         appSingleDay d
                         putChar '\n'


appSingleDay :: String -> IO ()
appSingleDay day = maybe error' renderResults (callDay day) >>= putStrLn
  where
    error' = return $ "Unknown day: '" ++ day ++ "' (or input file missing)"
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
                        , result ++ "==========="
                        ]
  where
    shortEnough = length result < 70
    singleLine  = not $ elem '\n' result
    valid = maybe "?" isSuccess expected
    isSuccess ex = if result == ex then " " else "X"
