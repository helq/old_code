
cons :: a -> b -> (a -> b -> c) -> c
cons a b = \f -> f a b

car :: ((a -> b -> a) -> c) -> c
car z = z (\p q -> p)

cdr :: ((a -> b -> b) -> c) -> c
cdr z = z (\p q -> q)

-- example
main = do
    print $ car $ car $ cons (cons 2 3) undefined
    print $ car $ cdr $ cons (cons 2 3) (cons [1..2] undefined)


