module Y2019.Day01 (parts) where


parts = ( (part1, Just "3152919")
        , (part2, Just "4726527")
        , readMasses
        )


part1 :: [Int] -> String
part1 masses = show . sum $ fuelRequirement <$> masses


part2 :: [Int] -> String
part2 masses = show . sum $ totalRequirement <$> masses


readMasses :: String -> [Int]
readMasses = fmap read . lines


fuelRequirement :: Int -> Int
fuelRequirement mass = (floor $ fromIntegral mass / 3.0) - 2


totalRequirement :: Int -> Int
totalRequirement mass | fuel <= 0 = 0
                      | otherwise = fuel + totalRequirement fuel
  where fuel = fuelRequirement mass
