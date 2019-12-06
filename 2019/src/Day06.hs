module Day06 (parts, part1, part2) where

import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map.Strict as Map
import           Util.Parser


type ObjectId = String
type OrbitMap = Map.Map ObjectId [ObjectId]
data Orbit    = Orbit ObjectId ObjectId      deriving Show


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "162439")
        , (part2, Just "367")
        ]


part1 input = return . show $ sum $ Map.elems counts
  where
    counts = countDirectAndIndirectOrbits orbits
    orbits = buildOrbitMap $ runParser (some orbit) input


part2 input = return . show $ transfers $ path orbits "YOU" "SAN"
 where
    orbits    = buildOrbitMap $ runParser (some orbit) input
    transfers = (flip (-) 2) . length


orbit :: Parser Orbit
orbit = do a <- id
           char ')'
           b <- id
           many $ oneOf " \n"; return $ Orbit a b
  where
    id = some . satisfy $ liftM2 (||) isAsciiUpper isDigit


buildOrbitMap :: [Orbit] -> OrbitMap
buildOrbitMap []                    = Map.empty
buildOrbitMap ((Orbit body sat):os) = Map.insertWith (++) body [sat]
                                    $ Map.insertWith (++) sat  []
                                    $ buildOrbitMap os


countDirectAndIndirectOrbits :: OrbitMap
                             -> Map.Map ObjectId Int
countDirectAndIndirectOrbits orbits = Map.fromList $ countPair <$> Map.assocs orbits
  where countPair assoc = (fst assoc, count assoc)
        count (body, [])   = 0
        count (body, sats) = length sats + rest
          where rest = sum $ count <$> [(s, (Map.!) orbits s) | s <- sats]


path :: OrbitMap
     -> ObjectId
     -> ObjectId
     -> [ObjectId]
path orbits from to = reverse in' ++ [rootPath from !! length in'] ++ out' ++ [to]
   where (in', out')   = trim (reverse $ rootPath from) (reverse $ rootPath to)
         sats id       = (Map.!) orbits id
         isSat body id = elem id $ sats body
         parent id     = find (flip isSat id) $ Map.keys orbits
         root          = root' from where root' id = maybe id root' $ parent id
         rootPath id   = maybe [] (\p -> p : rootPath p) $ parent id


trim (a:aa) (b:bb) | a == b    = trim aa bb
                   | otherwise = (a:aa, b:bb)
