module Main where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List (intercalate)
import Days
import System.Environment (getArgs)
import Text.Printf (printf)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [year, "all"] -> appAllDays year
        [year, day]   -> (runMaybeT $appSingleDay year day) >>= printOrError year day
        _             -> error $ "expected one argument, got " ++ show args
  where
    printOrError _ _ (Just str) = putStrLn str
    printOrError y d Nothing    = printf "source or input missing: %s-12-%s" y d


appAllDays :: String
           -> IO ()
appAllDays year = mapM_ renderDay (filter ((year ==) . fst) $ fst <$> parts)
  where
    renderDay (y, d) = do
      dayResult <- runMaybeT (appSingleDay year d)
      case dayResult of
        Nothing     -> pure ()
        Just report -> printf "%s-12-%s\n%s%s\n" y d footer report


appSingleDay :: MonadIO m
             => String
             -> String
             -> MaybeT m String
appSingleDay year day = do
    res <- runMaybeT $ callDayTimed year day
    MaybeT . pure $ maybe Nothing (Just . renderResults) res
  where
    renderResults (timeP, timeA, timeB, results) =
        intercalate "\n" reports ++ printf "\n%s%s\n" footer (timing :: String)
      where
        reports = uncurry fmt <$> zip [1..] results
        fmt n (result, e) = formatPart n e result
        timing = printf "%.2fs = %.2fs + %.2fs + %.2fs\n"
                        (timeP + timeA + timeB) timeP timeA timeB


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
footer = "=================================\n"
