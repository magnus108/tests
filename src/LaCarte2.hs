{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module LaCarte2
where


class ExpLit repr where
  lit :: Int -> repr


class ExpSYM repr where
  add :: repr -> repr -> repr


class ExpSYM2 repr where
  addM :: repr -> repr -> repr


instance ExpLit Int where
  lit n = n


instance ExpLit String where
  lit n = show n


instance ExpSYM Int where
  add e1 e2 = e1 + e2


instance ExpSYM String where
  add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"


instance ExpSYM2 Int where
  addM e1 e2 = e1 * e2


instance ExpSYM2 String where
  addM e1 e2 = "(" ++ e1 ++ " * " ++ e2 ++ ")"


eval :: Int -> Int
eval = id


view :: String -> String
view = id


type Rings v = (ExpSYM v, ExpSYM2 v, ExpLit v)


ringsExp :: Rings v => v
ringsExp = addM (add (lit 99) (lit 2)) (lit 3)


main :: IO ()
main = print $ eval ringsExp
