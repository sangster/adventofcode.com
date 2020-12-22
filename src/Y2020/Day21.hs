module Y2020.Day21 (parts) where

import           Data.Char
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import           Data.List
import           Parser


parts = ( (part1, Just "1885")
        , (part2, Just "fllssz,kgbzf,zcdcdf,pzmg,kpsdtv,fvvrc,dqbjj,qpxhfp")
        , parse $ splitSome (char '\n') food
        )


part1 :: [Food] -> String
part1 foods = show $ countOccurances noAllergens foods
  where
    allIngredients = S.unions $ ingredients <$> foods
    noAllergens = S.difference allIngredients ingredientsWithAllergens
    ingredientsWithAllergens = S.unions $ M.elems possible
    possible = allergenPossibilities foods


part2 :: [Food] -> String
part2 foods = intercalate "," ordered
  where
    ordered = ingredientsOrderedByAllergen allergyMap
    allergyMap = ingredientAllergens possible
    possible = allergenPossibilities foods


data Food = Food{ ingredients :: S.HashSet Ingredient
                , allergens   :: S.HashSet Allergen
                } deriving Show

type Ingredient = String
type Allergen   = Ingredient


countOccurances :: S.HashSet Ingredient -> [Food] -> Int
countOccurances is foods = foldr count 0 foods
  where
    count f acc = acc + (S.size $ S.intersection is (ingredients f))


ingredientsOrderedByAllergen :: M.HashMap Ingredient Allergen -> [Ingredient]
ingredientsOrderedByAllergen map' = fst <$> sortBy compareSnd (M.toList map')
  where
    compareSnd (_,a) (_,b) = compare a b


ingredientAllergens :: M.HashMap Allergen (S.HashSet Ingredient)
                    -> M.HashMap Ingredient Allergen
ingredientAllergens = reduce M.empty
  where
    reduce known possible
      | M.null possible = known
      | otherwise       = reduce known' possible'
      where
        singles   = M.filter ((== 1) . S.size) possible
        singles'  = M.foldrWithKey (\a is m -> M.insert (head $ S.toList is) a m) M.empty singles
        possible' = M.map (flip S.difference $ S.unions (M.elems singles))
                  $ M.difference possible singles
        known'    = M.union known singles'


allergenPossibilities :: [Food] -> M.HashMap Allergen (S.HashSet Ingredient)
allergenPossibilities foods = foldr intersect M.empty foods
  where
    intersect f m = S.foldr intersectAllergen m (allergens f)
      where
        intersectAllergen a m' = M.insertWith (S.intersection) a (ingredients f) m'


food :: Parser Food
food = do is <- some (token ingredient)
          string "(contains "
          as <- splitSome (string ", ") allergen
          string ")"
          pure $ Food { ingredients = S.fromList is
                      , allergens   = S.fromList as
                      }


ingredient :: Parser Ingredient
ingredient = some $ satisfy isAsciiLower
allergen   = ingredient
