{-# LANGUAGE FlexibleInstances #-}
module Contracts
where

import Data.Maybe

import Prelude hiding (max)


newtype Date = Date { unDate :: Int } deriving (Eq)

data Asset = Asset [(Date, Double)]


data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Log Expr
  | Exp Expr
  | Less Expr Expr
  | Cond Expr Expr Expr
  | Observe Expr Expr
  | EDouble Double
  | EBool Bool
  | EAsset Asset
  | EDate Date


data E a = E Expr

type EDouble = E Double
type EBool = E Bool


instance Num EDouble where
  (+) = binOp Add
  (-) = binOp Sub
  (*) = binOp Mul
  abs x = cond (x .<. 0) (-x) x
  fromInteger = E . EDouble . fromInteger
  signum x = cond (x .<. 0) (-1) (cond (0 .<. x) 1 0)


binOp :: (Expr -> Expr -> Expr) -> E a -> E b -> E c
binOp op (E x) (E y) = E (op x y)


unOp :: (Expr -> Expr) -> E a -> E a
unOp op (E x) = E (op x)


instance Fractional EDouble where
  (/) = binOp Div
  fromRational = E . EDouble . fromRational


instance Floating EDouble where
  exp = unOp Exp
  log = unOp Log


observe :: E Asset -> E Date -> EDouble
observe = binOp Observe


infix 4 .<.
(.<.) :: E Double -> E Double -> E Bool
(.<.) = binOp Less


max :: EDouble -> EDouble -> EDouble
max a b = cond (a .<. b) b a


cond :: E Bool -> E a -> E a -> E a
cond (E c) (E t) (E e) = E (Cond c t e)


class Value a where
  lift :: a -> E a
  down :: E a -> a


instance Value Double where
  lift = E . EDouble
  down (E (EDouble x)) = x


instance Value Date where
  lift = E . EDate
  down (E (EDate x)) = x


instance Value Bool where
  lift = E . EBool
  down (E (EBool x)) = x


instance Value Asset where
  lift = E . EAsset
  down (E (EAsset x)) = x


-- GO Shallow -- for somereason this makes the list static
-- type List a = [a]
-- Go Deep -- Now the list is dynamic
-- type List a = E [a]
-- now i need to add more methods to expr like fold and stuff.

d1, d2 :: Date
d1 = Date 100
d2 = Date 200

--linear
a1, a2 :: Asset
a1 = Asset [(d1, 5), (d2, 5)]
a2 = Asset [(d1, 4), (d2, 6)]
a3 = Asset [(d1, 4), (d2, 8)]


t :: Double
t = evalE $ bestOf [lift a1, lift a2, lift a3] (lift d1) (lift d2)


bestOf :: [E Asset] -> E Date -> E Date -> EDouble
bestOf assets startDate endDate =
  foldl1 max $
  map (perf startDate endDate) assets


perf :: E Date -> E Date -> E Asset -> EDouble
perf t1 t2 asset =
  observe asset t2 / observe asset t1 - 1


lookupObs :: Date -> [(Date, Double)] -> Double
lookupObs d dvs = fromMaybe 0 $ lookup d dvs


eval :: Expr -> Expr
eval (Add e1 e2) = case (eval e1, eval e2) of
                    (EDouble x1, EDouble x2) -> EDouble (x1 + x2)

eval (Sub e1 e2) = case (eval e1, eval e2) of
                    (EDouble x1, EDouble x2) -> EDouble (x1 - x2)

eval (Mul e1 e2) = case (eval e1, eval e2) of
                    (EDouble x1, EDouble x2) -> EDouble (x1 * x2)

eval (Div e1 e2) = case (eval e1, eval e2) of
                    (EDouble x1, EDouble x2) -> EDouble (x1 / x2)

eval (Less e1 e2) = case (eval e1, eval e2) of
                    (EDouble x1, EDouble x2) -> EBool (x1 < x2)
eval (Log e) = case eval e of EDouble x -> EDouble (log x)
eval (Exp e) = case eval e of EDouble x -> EDouble (exp x)
eval (Cond c t e) = case eval c of
                    EBool b -> if b then eval t else eval e

eval (Observe e1 e2) = case (eval e1, eval e2) of
                          (EAsset (Asset dvs), EDate d) -> EDouble (lookupObs d dvs)

eval e = e


-- for type safty
evalE :: (Value a) => E a -> a
evalE (E e) = down $ E $ eval e


main :: IO ()
main = print t
