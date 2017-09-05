module Shallow
where


type Expr = Int


lit :: Int -> Int
lit n = n


add :: Expr -> Expr -> Expr
add p q = p + q


mul :: Expr -> Expr -> Expr
mul p q = p * q


sub :: Expr -> Expr -> Expr
(sub) p q = p - q


main :: IO ()
main = print (10 + 10 `sub` (mul(lit 3)(lit 2)))
