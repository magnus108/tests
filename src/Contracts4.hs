module Contracts4
where

import Control.Monad.Reader




data Currency = DKK




data Contract
  = Zero
  | One Currency
  | Give Contract
  | And Contract Contract
  | Or Contract Contract
  | Scale (Obs Double) Contract
  | When (Obs Bool) Contract


zero :: Contract
zero = Zero


one :: Currency -> Contract
one = One


give :: Contract -> Contract
give = Give


--this could be better. netrium
cAnd :: Contract -> Contract -> Contract
cAnd = And

--this could be better. netrium
cOr :: Contract -> Contract -> Contract
cOr = Or

scale :: Obs Double -> Contract -> Contract
scale = Scale

--this could be better. netrium
cWhen :: Obs Bool -> Contract -> Contract
cWhen = When





newtype Date = Date Int

mkDate :: Int -> Date
mkDate = Date


instance Eq Date where
  (==) (Date t1) (Date t2) = (t1 == t2)




newtype Obs a = Obs (Date -> a)


konst :: a -> Obs a
konst k = Obs (\t -> k)


at :: Date -> Obs Bool
at t = Obs (\time -> (time == t))


date :: Obs Date
date = Obs (\t -> t)


zcb :: Date -> Double -> Currency -> Contract
zcb t x k = cWhen (at t) (scale (konst x) (one k))


european :: Date -> Contract -> Contract
european t u = cWhen (at t) (u `cOr` zero)

c11 :: Contract
c11 = european (mkDate 2)
  ( zcb (mkDate 2) 0.4 DKK `cAnd`  -- replace 'cAnd'
    zcb (mkDate 2) 9.3 DKK `cAnd`
    zcb (mkDate 3) 109.3 DKK `cAnd`
    give (zcb (mkDate 2) 100 DKK))



--date hører til model
evalC :: Date -> Contract -> Double --Contract
evalC d = eval
  where eval Zero = 0 --Zero
        eval (One k) = 1 --(One k) -- Optimize
        eval (Give c) = -(eval c)
        eval (Scale (Obs o) c) = (o d) * (eval c)
        eval (And c1 c2)  = (eval c1) + (eval c2)
        eval (Or c1 c2) = max (eval c1) (eval c2)
        eval (When (Obs o) c) = if (o d) then eval c else eval zero








---tests
--

c1 :: Contract
c1 = zcb t1 10 DKK

t1 :: Date
t1 = mkDate t1Horizon

t1Horizon :: Int
t1Horizon = 3







--npv :: Contract -> Reader Date Contract
--npv c = do
 -- t <- ask
 -- return (evalC t c)



main :: IO ()
main = print $ evalC (mkDate 3) c11 --runReader (npv c1) (mkDate 1)





--newtype Obs a = Obs (Date -> a)

--konst :: a -> Obs a
--konst k = Obs (\t -> k)







{-
type ExchangeRate = Double

data Model =
  Model
    { exch :: Currency -> Currency -> ExchangeRate }


exampleModel :: Model
exampleModel =
  Model
    { exch = exch }
      where
        exch :: Currency -> Currency -> ExchangeRate
        exch k1 k2 = 1

-}
--time0 :: Date
--time0 = mkDate 0

---evalO :: Obs a -> a
---evalO (Obs o) = o time0
