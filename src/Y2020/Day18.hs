module Y2020.Day18 (parts) where

import Parser


parts = ( (part1, Just "29839238838303")
        , (part2, Just "201376568795521")
        , lines
        )


part1 :: [String] -> String
part1 lines' = show . sum $ eval <$> expressions
 where
   expressions = map (parse equalPrecedenceExpr) lines'


part2 :: [String] -> String
part2 lines' = show . sum $ eval <$> expressions
 where
   expressions = map (parse addPrecedenceExpr) lines'


data Expr = Lit Int
          | Mul Expr Expr
          | Add Expr Expr
          deriving Show


eval :: Expr -> Int
eval (Lit   a) = a
eval (Mul a b) = eval a * eval b
eval (Add a b) = eval a + eval b


equalPrecedenceExpr :: Parser Expr
equalPrecedenceExpr = block `chainl1` (add <|> mul)
  where
    block = lit <|> parens equalPrecedenceExpr


addPrecedenceExpr :: Parser Expr
addPrecedenceExpr = addBlock `chainl1` mul
  where
    addBlock = block `chainl1` add
    block = lit <|> parens addPrecedenceExpr


parens :: Parser Expr -> Parser Expr
parens expr = do reserved "("
                 body <- expr
                 reserved ")"
                 pure body


add = symbol Add (reserved "+")
mul = symbol Mul (reserved "*")
lit = Lit <$> token number
