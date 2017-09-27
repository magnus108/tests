-- brug af <$> og <|> kunne være nice
-- JEG skal finde udaf hvordan renter virker... altså disc
-- jeg skal finde ud af hvordan european giver mening????
-- side 50 i werkahnfelt
-- SKAL ALT VÆRE KONTRAKTER!?!??!?!??!?!?
module Contracts3
where


import Numeric
import Data.List


data Currency = USD | GBP | EUR | ZAR | KYD | CHF  deriving (Eq, Show)

-- exessive use of type
type Date = Double

mkDate :: Double -> Date
mkDate s = s

time0 :: Date
time0 = mkDate 0


data Contract
  = Zero
  | One Currency -- Tradeable
  | Give Contract
  | And Contract Contract
  | Or Contract Contract
  | Cond (Obs Bool) Contract Contract
  | Scale (Obs Double) Contract
  | When (Obs Bool) Contract
  | Anytime (Obs Bool) Contract
  | Until (Obs Bool) Contract
  deriving Show


newtype Obs a = Obs (Date -> PR a)


instance Show a => Show (Obs a) where
  show (Obs o) = let (PR (rv:_)) = o time0 in "(Obs " ++ show rv ++ ")"


zero :: Contract
zero = Zero

one :: Currency -> Contract
one = One

give :: Contract -> Contract
give = Give

cAnd :: Contract -> Contract -> Contract
cAnd = And

cOr :: Contract -> Contract -> Contract
cOr = Or

cond :: Obs Bool -> Contract -> Contract -> Contract
cond = Cond

scale :: Obs Double -> Contract -> Contract
scale = Scale

cWhen :: Obs Bool -> Contract -> Contract
cWhen = When

anytime :: Obs Bool -> Contract -> Contract
anytime = Anytime

cUntil :: Obs Bool -> Contract -> Contract
cUntil = Until

andGive :: Contract -> Contract -> Contract
andGive c d = c `cAnd` give d


konst :: a -> Obs a
konst k = Obs (\t -> bigK k)

lift :: (a -> b) -> Obs a -> Obs b
lift f (Obs o) = Obs (\t -> PR $ map (map f) (unPr $ o t))

lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
lift2 f (Obs o1) (Obs o2) = Obs (\t -> PR $ zipWith (zipWith f) (unPr $ o1 t) (unPr $ o2 t))

date :: Obs Date
date = Obs (\t -> PR $ timeSlices [t])


instance Num a => Num (Obs a) where
  fromInteger i = konst (fromInteger i)
  (+) = lift2 (+)
  (-) = lift2 (-)
  (*) = lift2 (*)
  abs = lift abs
  signum = lift signum


instance Eq a => Eq (Obs a) where
  (==) = undefined

(==*) :: Ord a => Obs a -> Obs a -> Obs Bool
(==*) = lift2 (==)

at :: Date -> Obs Bool
at t = date ==* konst t

(%<), (%<=), (%=), (%>=), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
(%<)  = lift2 (<)
(%<=) = lift2 (<=)
(%=)  = lift2 (==)
(%>=) = lift2 (>=)
(%>)  = lift2 (>)


european :: Date -> Contract -> Contract
european t u = cWhen (at t) (u `cOr` zero)

american :: (Date, Date) -> Contract -> Contract
american (t1, t2) = anytime (between t1 t2)

between :: Date -> Date -> Obs Bool
between t1 t2 = lift2 (&&) (date %>= (konst t1)) (date %<= (konst t2))

newtype PR a = PR { unPr :: [RV a] } deriving Show


type RV a = [a]


takePr :: Int -> PR a -> PR a
takePr n (PR rvs) = PR $ take n rvs


horizonPr :: PR a -> Int
horizonPr (PR rvs) = length rvs


andPr :: PR Bool -> Bool
andPr (PR rvs) = and (map and rvs)


data Model = Model {
  modelStart :: Date,
  disc :: Currency -> (PR Bool, PR Double) -> PR Double,
  exch :: Currency -> Currency -> PR Double,
  absorb :: Currency -> (PR Bool, PR Double) -> PR Double,
  rateModel :: Currency -> PR Double
  }

exampleModel :: Model
exampleModel = Model {
  modelStart = 0,
  disc = disc,
  exch = exch,
  absorb = absorb,
  rateModel = rateModel
  } where
    rates :: Double -> Double -> PR Double
    rates rateNow delta = PR $ makeRateSlices rateNow 1
      where
        makeRateSlices rateNow n = (rateSlice rateNow n) : (makeRateSlices (rateNow-delta) (n+1))
        rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]

    rateModels = [(CHF, rates 7   0.8)
                 ,(EUR, rates 6.5 0.25)
                 ,(GBP, rates 8   0.5)
                 ,(KYD, rates 11  1.2)
                 ,(USD, rates 5   1)
                 ,(ZAR, rates 15  1.5)
                 ]

    rateModel k =
      case lookup k rateModels of
        Just x -> x
        Nothing -> error $ "rateModel: currency not found " ++ (show k)

    disc :: Currency -> (PR Bool, PR Double) -> PR Double
    disc k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel k)
      where
       discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
       discCalc (bRv:bs) (pRv:ps) (rateRv:rs) =
         if and bRv -- test for horizon
           then [pRv]
           else let rest@(nextSlice:_) = discCalc bs ps rs
                    --discSlice = zipWith (\x r -> x) (prevSlice nextSlice) rateRv
                    discSlice = zipWith (\x r -> x / (1 + r/100)) (prevSlice nextSlice) rateRv
                    thisSlice = zipWith3 (\b p q -> if b then p else q) -- allow for partially discounted slices
                                  bRv pRv discSlice
                in thisSlice : rest

       prevSlice :: RV Double -> RV Double
       prevSlice [] = []
       prevSlice (_:[]) = []
       prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest

    absorb :: Currency -> (PR Bool, PR Double) -> PR Double
    absorb k (PR bSlices, PR rvs) =
      PR $ zipWith (zipWith $ \o p -> if o then 0 else p)
                  bSlices rvs

    exch :: Currency -> Currency -> PR Double
    exch k1 k2 = PR (konstSlices 1)


expectedValue :: RV Double -> RV Double -> Double
expectedValue outcomes probabilities = sum $ zipWith (*) outcomes probabilities

expectedValuePr :: PR Double -> [Double]
expectedValuePr (PR rvs) = zipWith expectedValue rvs probabilityLattice

probabilityLattice :: [RV Double]
probabilityLattice = probabilities pathCounts
  where

    probabilities :: [RV Integer] -> [RV Double]
    probabilities (sl:sls) = map (\n -> fromInteger n / fromInteger (sum sl)) sl : probabilities sls

    pathCounts :: [RV Integer]
    pathCounts = paths [1] where paths sl = sl : paths (zipWith (+) (sl++[0]) (0:sl))


evalC :: Model -> Currency -> Contract -> PR Double
evalC (Model modelDate disc exch absorb rateModel) k = eval    -- punning on record fieldnames for conciseness
  where eval Zero           = bigK 0
        eval (One k2)       = exch k k2
        eval (Give c)       = -(eval c) --bigK (-1) * (eval c)-- eval (scale (Obs (\t -> (-1))) c)-- -(eval c)
        eval (o `Scale` c)  = evalO o * eval c
        eval (c1 `And` c2)  = eval c1 + eval c2
        eval (c1 `Or` c2)   = max (eval c1) (eval c2)
        eval (Cond o c1 c2) = condPr (evalO o) (eval c1) (eval c2)
        eval (When o c)     = disc   k (evalO o, eval c)
--      eval (Anytime o c)  = snell  k (evalO o, eval c)
        eval (Until o c)    = absorb k (evalO o, eval c)

evalO :: Obs a -> PR a
evalO (Obs o) = o time0

bigK :: a -> PR a
bigK x = PR (konstSlices x)

konstSlices :: a -> [[a]]
konstSlices x = nextSlice [x]
  where nextSlice sl = sl : nextSlice (x:sl)


datePr :: PR Date
datePr = PR $ timeSlices [time0]

timeSlices :: (Num t1, Enum t1) => [t1] -> [[t1]]
timeSlices sl@(t:_) = sl : timeSlices [t+1 | _ <- [0..t+1]]

condPr :: PR Bool -> PR a -> PR a -> PR a
condPr = lift3Pr (\b tru fal -> if b then tru else fal)

liftPr :: (a -> b) -> PR a -> PR b
liftPr f (PR a) = PR $ map (map f) a

lift2Pr :: (a -> b -> c) -> PR a -> PR b -> PR c
lift2Pr f (PR a) (PR b) = PR $ zipWith (zipWith f) a b


lift2PrAll :: (a -> a -> a) -> PR a -> PR a -> PR a
lift2PrAll f (PR a) (PR b) = PR $ zipWithAll (zipWith f) a b

lift3Pr :: (a -> b -> c -> d) -> PR a -> PR b -> PR c -> PR d
lift3Pr f (PR a) (PR b) (PR c) = PR $ zipWith3 (zipWith3 f) a b c

zipWithAll :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithAll f (a:as) (b:bs)     = f a b : zipWithAll f as bs
zipWithAll f as@(_:_) []       = as
zipWithAll f []       bs@(_:_) = bs
zipWithAll _ _        _        = []


instance Num a => Num (PR a) where
  fromInteger i = bigK (fromInteger i)
  (+) = lift2PrAll (+)
  (-) = lift2PrAll (-)
  (*) = lift2PrAll (*)
  abs = liftPr  abs
  signum = liftPr signum


instance Ord a => Ord (PR a) where
  max = lift2Pr max



instance Eq a => Eq (PR a) where
  (PR a) == (PR b) = a == b


xm :: Model
xm = exampleModel

evalX :: Contract -> PR Double
evalX = evalC xm USD

zcb :: Date -> Double -> Currency -> Contract
zcb t x k = cWhen (at t) (scale (konst x) (one k))



--zcb :: Date -> Double -> Currency -> Contract
--zcb t x k = cUntil (at t) (scale (konst x) (one k))
--zcb = absorbEx 

{-
--- Denne kan jo laves til en pension
minKaptialPensionsOprettelse =  17.09.19.1997
minKaptialPensionsSlut = 17.09.2007 
minPensionsAlder = 17.09.2017 
myMonthlyPayCheck = 30.000 DKK
myWorkplace = DTU

samletKapital = standartKapital minKaptialPensionsOprettelse minKaptialPensionsSlut minPensionsAlder myMonthlyPayCheck myWorkplace


////////////////////
DTU x = 
myMonthlyPayCheck12p = x * 0.12 
(myMonthlyPayCheck12p * (⅓), myMonthlyPayCheck12p  * (⅔))

standartKapital a b c d e = 
(x, y) =  e d 
minInklAmbKaptialPensionsBetaling  = inklAmbKapitalPensionPaymentMonthly  a b x
employeeInklAmbKaptialPensionsBetaling  = inklAmbKapitalPensionPaymentMonthly  a b y

minInklAmbKaptialPensionsUdBetaling = receive c minInklAmbKaptialPensionsBetaling and receive c employeeInklAmbKaptialPensionsBetaling

samletKapital = (minInklAmbKaptialPensionsUdBetaling * 1.1) and minInklAmbKaptialPensionsBetaling
-}


--- DETTE KAN VI LAVE TIL DTU 20 ÅRS PAKKEN


mt0 :: Double
mt0 = 0

mt1 :: Double
mt1 = 10

mt2 :: Double
mt2 = 20

curr :: Currency
curr = USD

mct :: Double
mct = 30000

mwp = dtu

-- måske regn med nemmmere %%
-- måske regn med nemmmere %%
-- måske regn med nemmmere %%
-- måske regn med nemmmere %%
-- måske regn med nemmmere %%
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
-- JEG TROR IKKE AT PROCENTER SKAL LÆGGES TI L HER MEN DET SKAL SKE I RENTEMODELLEN
dtu x =
  let
    p12 = x * 0.12
  in
    ( 0.33 * p12, 0.66 * p12) --hvormeget du betaler og hvor meget arbejdsgiver betaler


inklAmbKapitalPensionPaymentAnnual a b x curr =
  -- der her er sku nok forkert
  foldl (\acc t -> acc `cAnd` give ( zcb (mkDate t) x curr)) zero [a..b]
  -- make a list of a to b payments


standartKapital a b c e curr d =
  let
    (x, y) = e d

    x1 = (b - a) * x --betaling over 10 år fra mig inlk amb

    y1 = (b - a) * y -- betaling over 10 år fra employee inkl amb

    minInklAmbKapitalPensionsBetaling =
      inklAmbKapitalPensionPaymentAnnual a b x curr

    minInklAmbKapitalPensionsUdBetaling =
      y1 + x1


    samletKapital =
      zcb (mkDate c) (1.1 * minInklAmbKapitalPensionsUdBetaling) curr
      `cAnd` minInklAmbKapitalPensionsBetaling

      --zcb from a to b

  in
    samletKapital


standartKapital1020 = standartKapital mt0 mt1 mt2

standartKapital1020DTU = standartKapital1020 dtu curr

samlet = standartKapital1020DTU mct

---`cAnd` give (zcb (mkDate 15) 20000 USD)
   --             `cAnd` zcb (mkDate 21) 40000 USD



cg a b c d =
     zcb (mkDate a) 100 USD `cAnd`
     zcb (mkDate b) 100 USD `cAnd`
     zcb (mkDate c) 100 USD `cAnd`
     zcb (mkDate d) 100 USD



c1 :: Contract
c1 = samlet

 {-
  give (zcb (mkDate 0) 100 USD) `cAnd`
  give (zcb (mkDate 1) 100 USD) `cAnd`
  give (zcb (mkDate 2) 100 USD) `cAnd`
  give (zcb (mkDate 3) 100 USD) `cAnd`
  give (zcb (mkDate 4) 100 USD) `cAnd`
  give (zcb (mkDate 5) 100 USD) `cAnd`
  give (zcb (mkDate 6) 100 USD) `cAnd`
  give (zcb (mkDate 7) 100 USD) `cAnd`
  give (zcb (mkDate 8) 100 USD) `cAnd`
  give (zcb (mkDate 9) 100 USD) `cAnd`
  (zcb (mkDate 10) 210 USD) `cAnd`
  (zcb (mkDate 11) 210 USD) `cAnd`
  (zcb (mkDate 12) 210 USD) `cAnd`
  (zcb (mkDate 13) 210 USD) `cAnd`
  (zcb (mkDate 14) 210 USD) `cAnd`
  (zcb (mkDate 15) 210 USD) `cAnd`
  (zcb (mkDate 16) 210 USD) `cAnd`
  (zcb (mkDate 17) 210 USD) `cAnd`
  (zcb (mkDate 18) 210 USD) `cAnd`
  (zcb (mkDate 19) 210 USD)
-}

--give (cg 0 1 2 3) `cAnd` (scale (konst 1.5) (cg 4 5 6 7))


--`andGive`
--    zcb (mkDate 2) 10 USD

--     zcb (mkDate 10) 221.5 USD `cAnd`
--     zcb (mkDate 20) 332.3 USD `cAnd`
--     zcb (mkDate 30) 444.4 USD `cAnd`
--     zcb (mkDate 40) 555.3 USD `cAnd`
--     zcb (mkDate 50) 666.3 USD `cAnd`
--     zcb (mkDate 3) 777.3 USD


--c11 `cAnd` c22 `cAnd` (zcb (mkDate 50) 3 USD)

t1 :: Date
t1 = mkDate t1Horizon

t1Horizon :: Date
t1Horizon = 30

c11 :: Contract
c11 = european (mkDate 2)
        (zcb (mkDate 20) 0.4 USD `cAnd`
        zcb (mkDate 30) 9.3 USD `cAnd`
        zcb (mkDate 40) 109.3 USD `andGive`
        (zcb (mkDate 12) 100 USD))


c22 :: Contract
c22 = european (mkDate 14)
        (zcb (mkDate 20) 0.4 USD `cAnd`
        zcb (mkDate 30) 9.3 USD `cAnd`
        zcb (mkDate 40) 109.3 USD `andGive`
        (zcb (mkDate 24) 100 USD))


pr1 :: PR Double
pr1 = evalX c1

tr1 :: [RV Double]
tr1 = unPr pr1

absorbEx :: Date -> Double -> Currency -> Contract
absorbEx t x k = cUntil (konst t %> date) (scale (konst x) (one k))


tolerance :: Double
tolerance = 0.001

testK :: Bool
testK = andPr $ liftPr (== 100) $ takePr 10 (bigK 100)

testProb :: Bool
testProb = (sum $ probabilityLattice !! 100) - 1 < tolerance

testPr1 :: Bool
testPr1 = andPr $ lift2Pr (\a b -> (abs (a - b)) < tolerance)
                           pr1
                           (PR [[8.641], [9.246,8.901], [9.709,9.524,9.346], [10,10,10,10]])

tests :: Bool
tests = and [testK
            ,testProb
            ,testPr1]



chartUrl :: [Double] -> String
chartUrl vs = "http://chart.apis.google.com/chart?chs=400x300&cht=lc&chxt=x,y&chg=20,25,2,5&chxr=0,0,"
              ++ (show $ length vs - 1)
              ++ "|1," ++ (showFFloat (Just 1) ymin ",")
                       ++ (showFFloat (Just 1) ymax "&chd=t:")
              ++ (concat $ intersperse "," $ map (\y -> showFFloat (Just 1) y "") ys)
  where (ymin, ymax, ys) = chartScale vs 100


chartScale :: [Double] -> Double -> (Double, Double, [Double])
chartScale ys upper =
  let ymin = minimum ys
      ymax = maximum ys
      yrange = ymax - ymin
      yscale = upper/yrange
  in (ymin, ymax, map (\y -> (y - ymin) * yscale ) ys)


c1ExpectedValueUrl :: String
c1ExpectedValueUrl = chartUrl $ expectedValuePr $ takePr 23 pr1


main :: IO ()
main = do
  print "notice the probability now"
  print $ c1ExpectedValueUrl
--  print $ expectedValuePr $ evalX c1
  print $ expectedValuePr $ takePr 23  $ evalX c1
  --this is all good
