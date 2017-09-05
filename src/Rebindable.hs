{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
module Rebindable
where

import Prelude

import Rebindable2
import Rebindable3


ifThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a
ifThenElse b e1 e2 = If b e1 e2


main :: IO ()
main = print $ eval (if (Eq (I 3) (I 3)) then (Mul (I 4)(I 5)) else (I 4))
