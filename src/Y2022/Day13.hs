module Y2022.Day13 (parts) where

import Parser
import Data.List (elemIndex, sort)
import Data.Maybe (catMaybes)


parts = ( (part1, Just "5717")
        , (part2, Just "25935")
        , parse (splitSome (string "\n\n") packetPair)
        )
  where
    packetPair :: Parser (Packet, Packet)
    packetPair = (,) <$> packet <*> (char '\n' >> packet)
      where
        packet = list
        list = char '[' >> splitMany (char ',') element
           >>= (char ']' >>) . pure
        element = List <$> list
              <|> Num <$> natural


part1 :: [(Packet, Packet)] -> String
part1 pairs = show . sum
            $ fst <$> filter (uncurry (<=) . snd) ([1 :: Int ..] `zip` pairs)


part2 :: [(Packet, Packet)] -> String
part2 pairs = show . product
            $ (+1) <$> catMaybes (flip elemIndex (sort allPackets) <$> dividers)
  where
    allPackets = foldr (\(a,b) acc -> a:b:acc) dividers pairs
    dividers = [ [List [Num 2]]
               , [List [Num 6]]
               ]


type Packet = [Element]
data Element = Num Int
             | List [Element]
             deriving (Eq, Show)


instance Ord Element where
  compare (Num  a) (Num  b) = a `compare` b
  compare (List a) (List b) = a `compare` b

  compare (List a) (Num  b) = List a       `compare` List [Num b]
  compare (Num  a) (List b) = List [Num a] `compare` List b
