module Y2020.Day19 (parts) where

import qualified Data.HashMap.Strict as M
import           Parser


parts = ( (part1, Just "136")
        , (part2, Just "256")
        , parse puzzleInput
        )


part1 :: (RuleMap, [String]) -> String
part1 (rules, inputs) = show . length
                      $ filter (isMatch rules 0) inputs


part2 :: (RuleMap, [String]) -> String
part2 (rules, inputs) = show . length
                      $ filter (isMatch rules' 0) inputs
  where
    rules' = (parse ruleMap part2ExtraInput) `M.union` rules


part2ExtraInput = unlines [ "8: 42 | 42 8"
                          , "11: 42 31 | 42 11 31"
                          ]


type RuleMap = M.HashMap RuleId Rule
type RuleId = Int
data Rule = Lit Char
          | Ref RuleId
          | All [Rule]
          | Or Rule Rule
          deriving Show


puzzleInput :: Parser (RuleMap, [String])
puzzleInput = do rules <- ruleMap
                 char '\n'
                 msgs <- splitSome (char '\n') word
                 pure (rules, msgs)


ruleMap :: Parser RuleMap
ruleMap = M.fromList <$> splitSome (char '\n') rule


rule :: Parser (RuleId, Rule)
rule = do ruleId <- natural
          string ": "
          body <- literal <|> ruleChain
          pure (ruleId, body)
  where
    ruleChain = ruleRefs `chainl1` symbol Or (reserved "|")
    ruleRefs  = All <$> map Ref <$> splitSome (char ' ') natural
    literal   = do { char '"'; ch <- item; char '"'; pure $ Lit ch }


isMatch :: RuleMap -> RuleId -> String -> Bool
isMatch map' ruleId input = any null remainings
  where
    remainings = match (map' M.! ruleId) input

    match _ [] = []
    match (Lit lit) (c:cs) | lit == c  = [cs]
                           | otherwise = []
    match (Ref id) cs = match (map' M.! id) cs
    match (All rs) cs = foldM (flip match) cs rs
    match (Or a b) cs = match a cs ++ match b cs
