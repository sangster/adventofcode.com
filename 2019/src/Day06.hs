module Day06 (parts) where

import Control.Monad
import Data.Char
import Data.List
import Data.Map.Strict
import Util.Parser


type ObjectId = String
type OrbitMap = Map ObjectId [ObjectId]
data Orbit    = Orbit ObjectId ObjectId      deriving Show


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "162439")
        , (part2, Just "367")
        ]


part1 input = return . show $ sum $ elems counts
  where
    counts = countDirectAndIndirectOrbits orbits
    orbits = orbitMap $ runParser (some orbit) input


part2 input = return . show $ transfers $ path orbits "YOU" "SAN"
 where
    orbits    = orbitMap $ runParser (some orbit) input
    transfers = (flip (-) 2) . length


orbit :: Parser Orbit
orbit = do a <- id
           char ')'
           b <- id
           many $ oneOf " \n"; return $ Orbit a b
  where
    id = some . satisfy $ liftM2 (||) isAsciiUpper isDigit


orbitMap :: [Orbit] -> OrbitMap
orbitMap []                    = empty
orbitMap ((Orbit body sat):os) = insertWith (++) body [sat]
                               $ insertWith (++) sat  []
                               $ orbitMap os


countDirectAndIndirectOrbits :: OrbitMap
                             -> Map ObjectId Int
countDirectAndIndirectOrbits orbits = fromList $ countPair <$> assocs orbits
  where countPair assoc = (fst assoc, count assoc)
        count (body, [])   = 0
        count (body, sats) = length sats + rest
          where rest = sum $ count <$> [(s, orbits ! s) | s <- sats]


path :: OrbitMap
     -> ObjectId
     -> ObjectId
     -> [ObjectId]
path orbits from to = reverse up ++ [rootPath from !! length up] ++ down ++ [to]
  where (up, down)  = trim (reverse $ rootPath from) (reverse $ rootPath to)
        sat id body = elem id $ orbits ! body
        parent      = (flip find $ keys orbits) . sat
        rootPath    = (maybe [] (\p -> p : rootPath p)) . parent


trim (a:aa) (b:bb) | a == b    = trim aa bb
                   | otherwise = (a:aa, b:bb)
