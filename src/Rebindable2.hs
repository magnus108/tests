{-# LANGUAGE GADTs #-}
module Rebindable2
where

import Rebindable3


eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2) = eval e1 == eval e2
eval (If p e1 e2) = if eval p then eval e1 else eval e2
