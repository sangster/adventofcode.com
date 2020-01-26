module Y2019.Day01 (parts) where


parts = [ (part1, Just "3152919")
        , (part2, Just "4726527")
        ]


part1 :: String -> String
part1 content = show . sum $ fuelRequirement <$> readMasses content


readMasses :: String -> [Int]
readMasses = fmap read . lines


fuelRequirement :: Int -> Int
fuelRequirement mass = (floor $ fromIntegral mass / 3.0) - 2


part2 :: String -> String
part2 content = show . sum $ totalRequirement <$> readMasses content


totalRequirement :: Int -> Int
totalRequirement mass | fuel <= 0 = 0
                      | otherwise = fuel + totalRequirement fuel
  where fuel = fuelRequirement mass
