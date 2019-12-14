{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Day14 (parts) where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import           Data.Bool
import           Data.Char
import           Data.Function
import           Data.Maybe

import           Util.Parser


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


fuelCapacity :: Build Int
fuelCapacity = do
    ore <- oreNeeded "FUEL" 1
    maybe (return 0) (\_ -> fuelCapacity >>= return . succ) ore


mkCargo :: [(Chemical, Int)] -> Cargo
mkCargo = M.fromList


oreNeeded :: Chemical -> Int -> Build (Maybe Int)
oreNeeded "ORE" n = extract "ORE" n >>= return . (bool Nothing $ Just n)
oreNeeded chem  n = do
    have <- amount chem
    if have >= n
        then do extract chem n >> return (Just 0)
        else do ore <- acquire chem $ n - have
                oreNeeded chem n
                return ore


acquire :: Chemical -> Int -> Build (Maybe Int)
acquire chem i = do
    (i', forms) <- multiplex chem i
    insert chem i'
    ores <- mapM (uncurry oreNeeded) forms
    return $ sum <$> sequence ores


multiplex :: Chemical -> Int -> Build (Int, Formula)
multiplex chem i = do
    (out, forms) <- flip (M.!) chem . reactions <$> get
    let fact = ceiling $ ((/) `on` fromIntegral) i out
    return $ (out * fact, [(c,i' * fact) | (c, i') <- forms])


amount :: Chemical -> Build Int
amount chem = cargo <$> get >>= return . fromMaybe 0 . M.lookup chem


insert :: Chemical -> Int -> Build Bool
insert chem = updateCargo chem . (+)


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
