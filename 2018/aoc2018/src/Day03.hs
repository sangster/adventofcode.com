module Day03 (parts, part1, part2, part2new) where

import Data.Char (isDigit)
import qualified Data.List       as L
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

parts = [part1, part2]


data Claim = Claim { elfId, x, y, w, h :: !Int }
    deriving Show


squares :: Claim -> [(Int, Int)]
squares (Claim _ x y w h) = [(x', y') | x' <- [x..(x+w-1)], y' <- [y..(y+h-1)]]


mkClaims :: String -> [Claim]
mkClaims content = map mkClaim rows
  where
    rows           = map ints $ lines content
    ints line      = map read (words $ map filterDigit $ line) :: [Int]
    filterDigit x  = if isDigit x then x else ' '
    mkClaim row = Claim (row !! 0) (row !! 1) (row !! 2) (row !! 3) (row !! 4)


part1 :: String -> String
part1 content = show $ length overlaps
  where
      overlaps = filter ((> 1) . length) groups
      groups = L.group . L.sort $ mkClaims content >>= squares


part2 :: String -> String
part2 content =
    case maybeWinner of
        Just winner -> show (elfId winner)
        Nothing -> "fail"
  where
      maybeWinner = L.find allIn allClaims
      allIn x = all (\y -> y `S.member` noOverlaps) (squares x)
      noOverlaps = S.fromAscList . concat $ filter ((== 1) . length) groups
      groups = L.group . L.sort $ allClaims >>= squares
      allClaims = mkClaims content


part2new :: String -> String
part2new content =
    case maybeWinner of
        Just winner -> show (elfId winner)
        Nothing -> "fail"
  where
      maybeWinner = L.find allIn allClaims
      allIn x = all (\y -> y `S.member` noOverlaps) (squares x)
      noOverlaps = S.fromAscList . concat $ filter ((== 1) . length) groups
      groups = L.group . L.sort $ allClaims >>= squares
      allClaims = mkClaims content
