--module Main where

data Color = Red | Black deriving (Show, Eq)
data Direction = Left | Right deriving (Show, Eq)

data RBTree a = Tree a Color (RBTree a) (RBTree a)
              | Nil
              deriving (Show)

color :: RBTree t -> Color
color Nil            = Black
color (Tree _ c _ _) = c

insert :: Ord a => a -> RBTree a -> RBTree a
insert a Nil = Tree a Black Nil Nil
insert a (Tree a' c l r)
    | a < a'    = Tree a' c (insert a l) r
    | otherwise = Tree a' c l (insert a r)

-- Direction of child and Color of parent
help :: t -> RBTree t -> (Direction, Color, RBTree)
help a Nil = 


main :: IO()
main = print . insert 2 . insert 8 $ Tree 5 Red Nil Nil
