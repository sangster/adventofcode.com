module Y2015.Day02 (parts) where

import Parser


parts :: [((String -> String), Maybe String)]
parts = [ (part1, Just "1588178")
        , (part2, Just "3783758")
        ]


part1 input = show . sum $ paperNeeded <$> boxes
  where
    boxes = parse (some box) input
    paperNeeded (Box l w h) = surfaceArea + minimum [a, b, c]
      where
        surfaceArea = sum [2*a, 2*b, 2*c]
        a = l * w
        b = w * h
        c = h * l


part2 input = show . sum $ ribbonNeeded <$> boxes
  where
    boxes = parse (some box) input
    ribbonNeeded (Box l w h) = bow + wrap
      where
        bow = l * w * h
        wrap = minimum [2*(l+w), 2*(w+h), 2*(h+l)]



data Box = Box Int Int Int deriving Show


box :: Parser Box
box = do l <- spaces >> natural
         w <- char 'x' >> natural
         h <- char 'x' >> natural
         return $ Box l w h
