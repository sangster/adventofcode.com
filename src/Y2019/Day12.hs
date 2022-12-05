module Y2019.Day12 (parts) where

import Data.Bool
import Data.Function

import Parser


parts = ( (part1, Just "7471")
        , (part2, Just "376243355967784")
        , parse (some moon)
        )


part1 :: [Moon] -> String
part1 moons = show $ systemEnergy (steps !! 1000)
  where steps           = iterate step moons
        systemEnergy    = sum . fmap totalEnergy
        totalEnergy m   = potentialEnergy m * kineticEnergy m
        potentialEnergy = sum . fmap abs . pos
        kineticEnergy   = sum . fmap abs . vel


part2 :: [Moon] -> String
part2 moons = show $ foldr1 lcm repeats
  where repeats = findRepeatStep moons <$> dims (head moons)


type Moon      = [(Position, Velocity)]
type Dimension = Int
type Position  = Int
type Velocity  = Int


-- | The list of positions for each of a moon's dimensions.
pos :: Moon -> [Position]
pos = fmap fst


-- | The list of velocities for each of a moon's dimensions.
vel :: Moon -> [Velocity]
vel = fmap snd


-- | The list of dimensions this moon exists in.
dims :: Moon -> [Dimension]
dims m = [0 .. length m - 1]


-- | Parse a single @Moon@ from the given text.
moon :: Parser Moon
moon = do x <- coord "<x="
          y <- coord ", y="
          z <- coord ", z="
          char '>' >> whitespace >> pure [(x,0), (y,0), (z,0)]
  where
    coord prefix = string prefix >> number


-- | Move the moons one step forward in time.
step :: [Moon] -> [Moon]
step = applyVelocity . applyGravity


-- | Change the position (move) of each moon, based on its own velocity.
-- This does not change their velocities.
applyVelocity :: [Moon] -> [Moon]
applyVelocity moons = fmap (\(p, v) -> (p+v, v)) <$> moons


-- | Change the velocity of each moon, based on their position to other moons.
-- This does not change their positions.
applyGravity :: [Moon] -> [Moon]
applyGravity moons = applyGrav <$> moons
  where applyGrav m = [applyGravitySingle moons m d | d <- dims m]


-- | Change the velocity of a single moon in a single dimension.
applyGravitySingle :: [Moon] -> Moon -> Dimension -> (Position,Velocity)
applyGravitySingle moons moon' d = (p, v + deltaV)
  where (p,v)     = moon' !! d
        deltaV    = sum $ offset <$> moons
        offset m' = unit p (fst $ m' !! d)


-- | Compare the two positions and return the positive or negative unit that
--   will move them together.
unit :: Position -> Position -> Velocity
unit a b = case compare a b of
               LT -> 1
               EQ -> 0
               GT -> (-1)


-- | For a single dimension, find the step number in which every moon is in its
--   original position.
--
-- TODO: This can be made more efficient by only "stepping" the dimension under
-- test, and not every dimension.
findRepeatStep :: [Moon] -> Dimension -> Int
findRepeatStep origin d = findRepeat (step origin) 1
  where findRepeat moons i = bool (findRepeat (step moons) $ i+1) i $ match moons
        match moons         = ((==) `on` fmap (!! d)) moons origin
