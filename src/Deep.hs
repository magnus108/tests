module Deep
where


data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr


eval :: Expr -> Int
eval (Lit n) = n
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)
eval (Sub x y) = (eval x) - (eval y)


main :: IO ()
main = print (eval(Mul(Lit 3)(Lit 2)))
