{-# LANGUAGE MagicHash,UnboxedTuples,FlexibleInstances #-}
module Main
       where

import GHC.Base
import Text.Printf

instance Show (a -> a) where
  show f = case unpackClosure# f of
    (# a, _, _ #) -> let addr = (I# (addr2Int# a))
                     in printf "<function ??? at %x>" addr

main :: IO ()
main = print (\a -> a)
