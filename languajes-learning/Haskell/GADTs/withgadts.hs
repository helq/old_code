{-# LANGUAGE GADTs #-}

data Expr a where
    I   :: Int -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

eval :: Expr a -> a
eval (I i) = i
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2

main = print . eval $ ((I 5 `Add` I 23) `Eq` I 2) `Eq` B False
-- shouldn't work
--main = print . eval $ ((I 5 `Add` I 23) `Eq` B True) `Eq` B False
