import Data.Typeable

instance (Typeable a, Typeable b) => Show (a -> b) where
    show f = "Function: " ++ show (typeOf f)
