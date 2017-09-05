module Deeper
where


data Expr
  = Lit Integer
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr


eval :: Expr -> Integer
eval (Lit n) = n
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)
eval (Sub x y) = (eval x) - (eval y)


instance Num Expr where
  fromInteger x = Lit x
  x + y = Add x y
  x * y = Mul x y
  x - y = Sub x y
  abs x = error "Not implemented"
  signum x = error "Not implemented"


test :: [Expr]
test = [3, 4 * 3, 2 - 7]

main :: IO ()
main = print $ eval $ sum test
