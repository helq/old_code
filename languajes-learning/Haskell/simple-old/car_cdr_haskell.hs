cons :: a -> b -> (a -> b -> c) -> c
cons a b = \f -> f a b

car :: ((a -> b -> a) -> a) -> a
car e = e const

cdr :: ((a -> b -> b) -> b) -> b
cdr e = e (\a b->b)
