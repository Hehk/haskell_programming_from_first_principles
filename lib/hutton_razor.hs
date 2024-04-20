module HuttonRazor () where

data Expr
  = Lit Integer
  | Add Expr Expr

expr1 = Add (Lit 1) (Lit 9001)
expr2 = Add (Lit 9001) (Lit 1)
expr3 = Add expr1 (Lit 20001)

eval :: Expr -> Integer
eval (Add a b) = eval a + eval b
eval (Lit x) = x

printExpr :: Expr -> String
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
printExpr (Lit x) = show x
