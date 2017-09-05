module SharingSolved
where


data Expr a
    = Pl (Expr a) (Expr a)
    | One
    | Var a
    | Let (Expr a) (a -> Expr a)


eval :: Expr Int -> Int
eval (Pl x y) = (eval x) + (eval y)
eval One = 1
eval (Var x) = x
eval (Let e f) = eval(f(eval e))


tree :: Int -> Expr a
tree 0 = One
tree n = Let (tree (n - 1)) (\ shared -> Pl (Var shared) (Var shared ) )


-- c increments let
disp :: Expr String -> Int -> String
disp (Pl x y) c =  "(" ++ disp x c ++ " + " ++ disp y c ++ ")"
disp One c = "1"
disp (Var x) c = x
disp (Let e f) c = let v = "x" ++ show c
                   in "let " ++ v ++ " = " ++ disp e (c + 1) ++
                     " in " ++ disp (f v)(c + 1)


main :: IO ()
main = print (disp (tree 30) 1)
