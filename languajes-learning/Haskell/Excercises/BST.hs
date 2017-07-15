import Data.Function (on)

data BST a = Tree a (BST a) (BST a) Int | NIL deriving (Show, Read, Eq)

-- instance Show a => Show (BST a) where
--     show NIL = ""
--     show (Tree a l r _) = "[" ++ show l ++ show a ++ show r ++ "]"

instance Eq a => Ord (BST a) where
    compare = compare `on` size

size NIL = 0
size (Tree _ _ _ s) = s

-- Is not the best implementation
insert NIL b = Tree b NIL NIL 1
insert (Tree a l r s) b
    | a < b     = leftRotateIf (l<r)  $ Tree a l (insert r b) (s+1)
    | otherwise = rightRotateIf (l>r) $ Tree a (insert l b) r (s+1)

leftRotateIf :: Ord a => Bool -> BST a -> BST a
leftRotateIf False t = t
leftRotateIf True  t = leftRotate t

leftRotate :: Ord a => BST a -> BST a
leftRotate NIL = NIL
leftRotate (Tree a l r s) =
        case r of
            NIL -> error "son right NIL"
            Tree a' l' r' s' -> Tree a' newL r' s
                            where newL = Tree a l l' (1 + size l + size l')

rightRotateIf :: Ord a => Bool -> BST a -> BST a
rightRotateIf False t = t
rightRotateIf True  t = rightRotate t

rightRotate :: Ord a => BST a -> BST a
rightRotate NIL = NIL
rightRotate (Tree a l r s) =
        case l of
            NIL -> error "son left NIL"
            Tree a' l' r' s' -> Tree a' l' newR s
                            where newR = Tree a r r' (1 + size r + size r')

extractMax NIL = error "extractMax from NIL"
extractMax (Tree a l r s)
    | r == NIL  = (a, NIL)
    | otherwise = (e, Tree a l r' (s-1))
        where (e, r') = extractMax r

extractMin NIL = error "extractMax from NIL"
extractMin (Tree a l r s)
    | l == NIL  = (a, NIL)
    | otherwise = (e, Tree a l' r (s-1))
        where (e, l') = extractMin l


fromList :: Ord a => [a] -> BST a
fromList = foldl insert NIL

height NIL = 0
height (Tree _ l r _) = 1 + max (height l) (height r)
