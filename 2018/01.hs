#!/bin/env runhaskell
module Main where

import System.Environment
-- import qualified Data.Map as M

parse ('+':str) = read str
parse ('-':str) = 0 - read str

-- type Count = M.Map String Int

part1 numbers =
  sum numbers

-- part2 numbers =

main = do
  args <- getArgs
  content <- readFile $ args !! 0

  let numbers = map parse $ lines content

  putStrLn $ "1: " ++ (show $ part1 numbers)
  -- putStrLn $ "2: " ++ (show $ part2 content)
