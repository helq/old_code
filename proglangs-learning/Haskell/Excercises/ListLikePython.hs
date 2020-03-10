-- by helq

data List a = List [List a] | Elem a
    deriving Read

instance Show a => Show (List a) where
    show (Elem a)  = show a
    show (List as) = show as

