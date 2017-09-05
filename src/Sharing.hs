module Sharing
where


data Expr
    = Pl Expr Expr
    | One


eval :: Expr -> Int
eval (Pl x y) = (eval x) + (eval y)
eval One = 1


tree :: Int -> Expr
tree 0 = One
tree n = let shared = tree (n - 1) in Pl shared shared


disp :: Expr -> String
disp (Pl x y) =  "(" ++ disp x ++ " + " ++ disp y ++ ")"
disp One = "1"


main :: IO ()
main = print $ disp (tree 3)
