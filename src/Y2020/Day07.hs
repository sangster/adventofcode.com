module Y2020.Day07 (parts) where

import           Data.Bool (bool)
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
import           Data.List (intercalate, foldr)
import           Parser


parts = ( (part1, Just "378")
        , (part2, Just "27526")
        , M.fromList . parse (some rule)
        )


part1 rules = show . length $ canEventuallyFit rules "shiny gold"


part2 rules = show $ countBags rules "shiny gold"


type Bag = String
type Requirement = (Bag, Int)
type Rule = (Bag, S.HashSet Requirement)
type RulesMap = M.HashMap Bag (S.HashSet Requirement)


rule :: Parser Rule
rule = do b <- bag
          rs <- token requiredSet
          char '.' >> (char '\n' <|> eof)
          pure (b, rs)


bag :: Parser Bag
bag = do words <- token $ some colorWord
         string "bags" <|> string "bag"
         pure $ intercalate " " words
  where
    colorWord = do whitespace
                   w <- some (oneOf ['a'..'z'])
                   bool abort (pure w) (not $ elem w ["bag", "bags"])


requiredSet :: Parser (S.HashSet Requirement)
requiredSet = do whitespace >> string "contain" >> whitespace
                 req <- emptyReq <|> someReq
                 pure req
  where
    emptyReq = whitespace >> symbol S.empty (string "no other bags")
    someReq = S.fromList <$> splitSome (char ',') requirement
    requirement = do { whitespace; n <- natural; b <- bag; pure (b, n) }


canEventuallyFit :: RulesMap -> Bag -> S.HashSet Bag
canEventuallyFit rules bag' = eventually S.empty [bag']
  where
    canFit = canFitMap rules
    eventually seen [] = seen
    eventually seen (q:qs) =
        case M.lookup q canFit of
          Just fit -> let seen' = S.union fit seen
                          diff  = S.difference fit seen
                       in eventually seen' (qs ++ S.toList diff)
          Nothing  -> eventually seen qs


-- Map of bags to the set of bags they can fit inside.
canFitMap :: RulesMap -> M.HashMap Bag (S.HashSet Bag)
canFitMap rulesMap = M.foldrWithKey flipKeyValues M.empty rulesMap
  where
    flipKeyValues outerBag reqs cMap =
      foldr (insert outerBag) cMap reqs
    insert outer (inner, _) map' =
      M.insertWith (S.union) inner (S.singleton outer) map'


countBags :: RulesMap -> Bag -> Int
countBags rules bag' = S.foldr sumRequirements 0 (rules M.! bag')
  where
    sumRequirements (b, n) sum' = sum' + n + n * countBags rules b
