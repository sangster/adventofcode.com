{-# LANGUAGE QuasiQuotes                      #-}

module Y2018.Day05 (parts, part1, part2) where

parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "9704")
        , (part2, Just "6942")
        ]


-- part1 content = return $ (foldl process [head content] $ tail content)
--   where
--     process a (b:bs)

part1 input = return ""
part2 input = return ""
