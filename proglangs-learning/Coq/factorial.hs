module Factorial where

import qualified Prelude

data Nat =
   O
 | S Nat

add :: Nat -> Nat -> Nat
add n m =
  case n of {
   O -> m;
   S p -> S (add p m)}

mul :: Nat -> Nat -> Nat
mul n m =
  case n of {
   O -> O;
   S p -> add m (mul p m)}

factorial :: Nat -> Nat
factorial n =
  case n of {
   O -> S O;
   S n' -> mul n (factorial n')}

