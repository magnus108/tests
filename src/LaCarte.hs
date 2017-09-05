{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
module LaCarte
where


--data Expr
--  = Val Int
--  | Add Expr Expr


--render :: Expr -> String
--render (Val x) = show x
--render (Add x y) = "(" ++ render x ++ " + " ++ render y ++ ")"


class ExpSYM repr where
  lit :: Int -> repr
  add :: repr -> repr -> repr


instance ExpSYM Int where
  lit n = n
  add e1 e2 = e1 + e2


instance ExpSYM String where
  lit n = show n
  add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"


test = add (add (lit 99) (lit 11)) (lit 3)


eval :: Int -> Int
eval = id


view :: String -> String
view = id


main :: IO ()
main = print $ view test
