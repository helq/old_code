data Expr = I Int         -- integer constants
          | B Bool        -- boolean constants
          | Add Expr Expr -- add two expressions
          | Mul Expr Expr -- multiply two expressions
          | Eq  Expr Expr -- equality test

eval :: Expr -> Maybe (Either Int Bool)
eval (I n) = Just . Left $ n
eval (B b) = Just . Right $ b

eval (Add e1 e2) = do
    Left v1 <- eval e1
    Left v2 <- eval e2
    return . Left $ v1 + v2

eval (Mul e1 e2) = do
    Left v1 <- eval e1
    Left v2 <- eval e2
    return . Left $ v1 * v2

eval (Eq e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    case (e1', e2') of
      ( Left v1,  Left v2) -> Just . Right $ v1 == v2
      (Right v1, Right v2) -> Just . Right $ v1 == v2
      _                    -> Nothing

main = print . eval $ ((I 5 `Add` I 23) `Eq` I 2) `Eq` B False
--main = print . eval $ ((I 5 `Add` I 23) `Eq` B True) `Eq` B False
