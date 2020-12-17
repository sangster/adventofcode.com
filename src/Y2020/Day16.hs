module Y2020.Day16 (parts) where

import           Data.Hashable (Hashable, hashWithSalt)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import           Data.List (isPrefixOf, sortBy)
import           Parser


parts = ( (part1, Just "22057")
        , (part2, Just "1093427331937")
        , parse puzzleInput
        )


part1 :: (S.HashSet Rule, Ticket, [Ticket]) -> String
part1 (rules, your, nearby) = show $ sum invalids
 where
   invalids = concat $ invalidFields rules <$> nearby


part2 :: (S.HashSet Rule, Ticket, [Ticket]) -> String
part2 (rules, your, nearby) = show
                            $ S.foldr (\r prod -> prod * (yourTicket M.! r)) 1
                            $ departureRules rules
 where
   yourTicket = M.fromList $ orderedRules `zip` your
   orderedRules = reorderRules (filter isValid nearby) rules
   isValid = null . invalidFields rules


data Rule = Rule String (Int -> Bool)
type Ticket = [Field]
type Field = Int


instance Show     Rule where show (Rule n _) = n
instance Eq       Rule where (Rule a _) == (Rule b _) = a == b
instance Hashable Rule where hashWithSalt s (Rule n _) = hashWithSalt s n


-- | Return the Rules which the given field is valid for.
matchingRules :: S.HashSet Rule
              -> Field
              -> S.HashSet Rule
matchingRules rs n = S.filter (\(Rule _ f) -> f n) rs


-- | Return the fields in the given ticket which don't match any rule.
invalidFields :: S.HashSet Rule
              -> Ticket
              -> [Field]
invalidFields rs t = filter (null . matchingRules rs) t


departureRules :: S.HashSet Rule
               -> S.HashSet Rule
departureRules = S.filter $ \(Rule n _) -> "departure" `isPrefixOf` n


-- | Return the given set of rules in an ordered list.
reorderRules :: [Ticket]
             -> S.HashSet Rule
             -> [Rule]
reorderRules tickets rules = toList $ reorder initial M.empty
  where
    initial = M.fromList $ mkEntry <$> [0 .. length rules - 1]
    mkEntry i = (i, (rules, fields)) where fields = (flip (!!) i) <$> tickets


-- | Takes a map of Field Indicies to the set of possible rules for that field
-- and the values for that field from every ticket. Returns a map of Field
-- Indicies to the Rule that defines that field.
reorder :: M.HashMap Int (S.HashSet Rule, [Field])
        -> M.HashMap Int Rule
        -> M.HashMap Int Rule
reorder unsure sure
  | M.null unsure = sure
  | otherwise = reorder unsure' (M.union sure sure')
  where
    filtered = filterValidRules unsure
    unsure' = M.filter (\(rs, _) -> not $ S.null rs)
            $ M.map (\(rs, fs) -> (S.difference rs sureRules, fs)) filtered
    sureRules = M.foldr S.insert S.empty sure'
    sure' = M.foldrWithKey findSure M.empty filtered :: M.HashMap Int Rule
    findSure i (rules, fields) map' = if S.size rules == 1
                                      then M.insert i (head $ S.toList rules) map'
                                      else map'


toList :: M.HashMap Int Rule -> [Rule]
toList map' = snd <$> sortBy (\(a,_) (b,_) -> compare a b) (M.toList map')


-- | For each element, remove each rule that one of the fields is invalid for.
filterValidRules :: M.HashMap Int (S.HashSet Rule, [Field])
                 -> M.HashMap Int (S.HashSet Rule, [Field])
filterValidRules = M.map filt
  where
    filt (rules, fields) = (matches, fields)
      where matches = foldr (\n rs -> matchingRules rs n) rules fields


puzzleInput :: Parser (S.HashSet Rule, Ticket, [Ticket])
puzzleInput = do rules <- S.fromList <$> splitSome (char '\n') rule
                 spaces >> string "your ticket:" >> spaces
                 your <- ticket
                 spaces >> string "nearby tickets:" >> spaces
                 nearby <- splitSome (char '\n') ticket
                 pure (rules, your, nearby)
  where
    rule = do name <- some $ satisfy (/= ':')
              string ": "
              (aMin, aMax) <- range
              string " or "
              (bMin, bMax) <- range
              pure $ Rule name (\n -> (n>=aMin && n<=aMax) || (n>=bMin && n<=bMax))
    range = do min' <- natural
               char '-'
               max' <- natural
               pure (min', max')
    ticket = splitSome (char ',') natural
