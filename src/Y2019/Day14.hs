{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Y2019.Day14 (parts) where

import           Data.Bool
import           Data.Char
import           Data.Function
import qualified Data.Map.Strict as M
import           Data.Maybe

import Parser


parts :: [((String -> IO String), Maybe String)]
parts = [ (part1, Just "1046184")
        , (part2, Just "1639374")
        ]


part1 input = return . show . fromJust
            $ evalState (runBuild request) factory
  where
    request = oreNeeded "FUEL" 1
    factory = Factory { cargo     = mkCargo [("ORE", maxBound)]
                      , reactions = parse recipes input
                      }


part2 input = return . show
            $ evalState (runBuild request) factory
  where
    request = fuelCapacity
    factory = Factory { cargo     = mkCargo [("ORE", 1000000000000)]
                      , reactions = parse recipes input
                      }


mkCargo :: [(Chemical, Int)] -> Cargo
mkCargo = M.fromList


-- | The amount of Ore needed to create a number of chemicals, or @Nothing@, if
--   there isn't enough ore available.
oreNeeded :: Chemical -> Int -> Build (Maybe Int)
oreNeeded "ORE" n = extract "ORE" n >>= return . (bool Nothing $ Just n)
oreNeeded chem  n = do
    have <- amount chem
    if have >= n
        then do extract chem n >> return (Just 0)
        else do ore <- acquire chem $ n - have
                oreNeeded chem n
                return ore


-- | The amount of fuel that can be greated with the given cargo.
fuelCapacity :: Build Int
fuelCapacity = do fact <- get
                  ore  <- amount "ORE"
                  return . fromJust $ binSearchInts (search' fact) 0 ore
  where
    search' fact n = isJust $ evalState (runBuild $ oreNeeded "FUEL" n) fact


-- | Find the largest integer that matches the given predicate.
binSearchInts :: Integral a => (a -> Bool) -> a -> a -> Maybe a
binSearchInts f min max
    | max - min == 1 = bool Nothing (Just min) $ f min && not (f max)
    | f midway       = binSearchInts f midway max
    | otherwise      = binSearchInts f min midway
  where
    midway = (min + max) `div` 2


-- | Create a number of chemicals by using existing chemicals in the cargo.
acquire :: Chemical -> Int -> Build (Maybe Int)
acquire chem i = do
    (i', forms) <- multiplex chem i
    insert chem i'
    ores <- mapM (uncurry oreNeeded) forms
    return $ sum <$> sequence ores


-- | Return the formula necessary to create at least the number of given
--   chemicals.
-- The return will be a turn, where the first entry is the actual number of
-- created chemicals.
multiplex :: Chemical -> Int -> Build (Int, Formula)
multiplex chem i = do
    (out, forms) <- flip (M.!) chem . reactions <$> get
    let fact = ceiling $ ((/) `on` fromIntegral) i out
    return $ (out * fact, [(c,i' * fact) | (c, i') <- forms])


-- | The amount of the named chemical in the cargo.
amount :: Chemical -> Build Int
amount chem = cargo <$> get >>= return . fromMaybe 0 . M.lookup chem


-- | Insert a number of the given chemical into the cargo.
insert :: Chemical -> Int -> Build ()
insert chem i = updateCargo chem (+i) >> return ()


-- | Remove a number of the given chemical from the cargo. Return @True@ if
--   there were enough available to do so.
extract :: Chemical -> Int -> Build Bool
extract chem i = do
    have <- amount chem
    if have < i
        then return False
        else (updateCargo chem . flip (-)) i >> return True


updateCargo :: Chemical -> (Int -> Int) -> Build Bool
updateCargo chem f = do
    car <- cargo <$> get
    new <- f <$> amount chem
    if new >= 0
        then do modify $ \fact -> fact{ cargo = M.insert chem new car }
                return True
        else return False


newtype Build a = Build { runBuild :: State Factory a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState Factory
             )


type Reactions = M.Map Chemical (Int, Formula)
type Cargo     = M.Map Chemical Int
type Formula   = [(Chemical, Int)]
type Chemical  = String


data Factory = Factory
    { cargo     :: !Cargo
    , reactions :: !Reactions
    }


data Recipe = Recipe
    { input :: !Formula
    , chem  :: !Chemical
    , count :: !Int
    }


recipes :: Parser Reactions
recipes = do recipes <- some recipe
             return $ M.fromList [(c, (n,i)) | Recipe i c n <- recipes]


recipe :: Parser Recipe
recipe = do input <- some quantity
            string "=>"
            (chem, i) <- quantity
            return $ Recipe input chem i


quantity :: Parser (Chemical, Int)
quantity = do spaces
              n <- natural
              spaces
              c <- some $ satisfy isAsciiUpper
              many $ oneOf ", "
              return $ (c, n)
