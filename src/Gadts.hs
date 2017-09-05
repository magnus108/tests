{-# LANGUAGE GADTs #-}
module Gadts
where


data Expr a where
  I :: Int  -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a


eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2
eval (If p e1 e2)  = if eval p then eval e1 else eval e2


main :: IO ()
main = print $ eval (If (Eq (I 3) (I 2)) (Mul (I 4)(I 5)) (I 4))
