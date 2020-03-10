-- by helq
{-# LANGUAJES FlexibleInstances, TypeSynonymInstances #-}

class MyClass a where
    jaja :: a -> String

instance MyClass String where
    jaja a = a
