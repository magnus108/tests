{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module EvenDeeper
where


data Expr a where
  I :: Int  -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a


class Boolean f r | f -> r where
  bool :: r -> r -> f -> r
  false :: f
  true :: f


instance Boolean (Expr Bool) (Expr Int) where
  false = B False
  true = B True
  bool f t p = If p f t


class Equal j a where
  (==) :: (Boolean j a) => a -> a -> j


instance Equal (Expr Bool) (Expr Int) where
  (I x) == (I y) = B (x Prelude.== y)


eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2) = eval e1 Prelude.== eval e2
eval (If p e1 e2)  = if eval p then eval e1 else eval e2


main :: IO ()
main = print $ eval (If ((I 5) EvenDeeper.== (I 6)) (Mul (I 4)(I 5)) (I 4))
