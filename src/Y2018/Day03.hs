module Y2018.Day03 (parts, part1, part2, part2new) where

import           Data.Char  (isDigit)
import qualified Data.List  as L
import qualified Data.Set   as S


parts = ( (part1, Nothing)
        , (part2, Nothing)
        , mkClaims
        )


data Claim = Claim { elfId, x, y, w, h :: !Int }
    deriving Show


squares :: Claim -> [(Int, Int)]
squares (Claim _ x y w h) = [(x', y') | x' <- [x..(x+w-1)], y' <- [y..(y+h-1)]]


mkClaims :: String -> [Claim]
mkClaims input = map mkClaim rows
  where
    rows           = map ints $ lines input
    ints line      = map read (words $ map filterDigit $ line) :: [Int]
    filterDigit x  = if isDigit x then x else ' '
    mkClaim row = Claim (row !! 0) (row !! 1) (row !! 2) (row !! 3) (row !! 4)


part1 :: [Claim] -> String
part1 claims = show $ length overlaps
  where
      overlaps = filter ((> 1) . length) groups
      groups = L.group . L.sort $ claims >>= squares


part2 :: [Claim] -> String
part2 claims =
    case maybeWinner of
        Just winner -> show (elfId winner)
        Nothing -> "fail"
  where
      maybeWinner = L.find allIn claims
      allIn x = all (\y -> y `S.member` noOverlaps) (squares x)
      noOverlaps = S.fromAscList . concat $ filter ((== 1) . length) groups
      groups = L.group . L.sort $ claims >>= squares


part2new :: String -> String
part2new input =
    case maybeWinner of
        Just winner -> show (elfId winner)
        Nothing -> "fail"
  where
      maybeWinner = L.find allIn allClaims
      allIn x = all (\y -> y `S.member` noOverlaps) (squares x)
      noOverlaps = S.fromAscList . concat $ filter ((== 1) . length) groups
      groups = L.group . L.sort $ allClaims >>= squares
      allClaims = mkClaims input
