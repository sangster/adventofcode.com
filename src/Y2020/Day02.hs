module Y2020.Day02 (parts) where

import Parser


data Password = Password Int Int Char String  deriving Show


parts = [ (part1, Just "465")
        , (part2, Just "294")
        ]


part1 :: String -> String
part1 input = show . length $ filter valid passwords
  where
    passwords = parse (some password) input

    valid (Password min max _ [])
      | min > 0   = False
      | max < 0   = False
      | otherwise = True
    valid (Password min max ch (x:xs))
      | x == ch   = valid $ Password (min-1) (max-1) ch xs
      | otherwise = valid $ Password min      max    ch xs


part2 :: String -> String
part2 input = show . length $ filter valid passwords
  where
    passwords = parse (some password) input

    valid (Password i j ch pass) = (ci == ch) /= (cj == ch)
      where
        ci = pass !! (i-1)
        cj = pass !! (j-1)


password :: Parser Password
password = do min <- natural
              char '-'
              max <- natural
              spaces
              ch <- item
              string ": "
              pass <- word
              spaces
              return $ Password min max ch pass
