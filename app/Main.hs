module Main where

import Data.List          (filter, intercalate)
import Days
import System.Environment (getArgs)
import System.TimeIt


main :: IO ()
main = do
    args <- getArgs
    case args of
        [year, "all"] -> appAllDays year
        [year, day]   -> putStrLn $ appSingleDay year day
        _             -> error $ "expected onen argument, got " ++ show args


appAllDays :: String -> IO ()
appAllDays year = mapM_ renderDay (filter (\(y,_) -> year == y) $ fst <$> parts)
  where
    renderDay (y,d) = do
      putStr $ y++"-12-"++d++"\n"++footer
      timeItNamed footer $ putStrLn $ appSingleDay year d
      putStr $ footer ++ "\n\n"


appSingleDay :: String -> String -> String
appSingleDay year day = maybe error' renderResults (callDay year day)
  where
    error' = "source or input missing: " ++ year ++ "-12-" ++ day
    renderResults results = intercalate "\n" reports
      where
        reports = uncurry fmt <$> zip [1..] results
        fmt n (result, e) = formatPart n e result


formatPart :: Int
           -> Maybe String
           -> String
           -> String
formatPart number expected result
  | shortEnough && singleLine = valid++" "++show number++": "++result
  | otherwise = valid++" "++show number++": ⏬\n"++strip result
  where
    shortEnough     = length result < 70
    singleLine      = not $ elem '\n' result
    valid           = maybe "?" isSuccess expected
    isSuccess ex    = if result == ex then " " else "X"
    strip  []       = []
    strip ('\n':[]) = []
    strip (c:cc)    = c:(strip cc)


footer :: String
footer = "===========\n"
