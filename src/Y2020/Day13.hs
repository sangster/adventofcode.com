module Y2020.Day13 (parts) where

import Data.Bool (bool)
import Data.Maybe (catMaybes, fromJust, isJust)
import Parser


parts = ( (part1, Just "4938")
        , (part2, Just "230903629977901")
        , parse notes
        )

part1 :: Notes -> String
part1 (earliest, buses) = show . uncurry (*) $ foldr1 smaller waits
  where
    smaller (b,w) (b',w') = bool (b',w') (b,w) (w < w')
    waits = waitMinutes <$> catMaybes buses
    waitMinutes n = (n, n - earliest `mod` n)


part2 :: Notes -> String
part2 (_, buses) = show $ findSebsequentDepartures firstCycle (tail buses')
  where
    firstCycle = snd $ head buses'
    buses' = foldr append [] $ zip [0..] buses
    append (n,b) bs = bool bs ((n, fromJust b) : bs) $ isJust b


type Notes     = (Offset, [Maybe Departure])
type Offset    = Int
type Departure = Int


notes :: Parser Notes
notes = do earliest <- natural
           spaces
           buses <- splitSome (char ',') maybeNatural
           pure (earliest, buses)
  where
    maybeNatural = symbol Nothing (char 'x') <|> (Just <$> natural)


findSebsequentDepartures :: Departure -> [(Offset, Departure)] -> Departure
findSebsequentDepartures initMultiple = fst . foldl findMultiple (0, initMultiple)
  where
    findMultiple (earliest, mult) (off, dep)
      | canDepart earliest (off, dep) = (earliest, mult * dep)
      | otherwise = findMultiple (earliest + mult, mult) (off, dep)

    canDepart earliest (off, dep) = (earliest + off) `mod` dep == 0
