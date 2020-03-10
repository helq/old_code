module Test where

-- To compile code: agda Test.agda -c --ghc-flag=-dynamic
open import IO

-- All of these are not necessary but they help to reduce clutter when looking
-- at the types of the different things in the code
import Agda.Builtin.List as List
open import Agda.Builtin.Unit using (⊤)
open List using (_∷_)
import Agda.Builtin.String as String

main = run (putStrLn "Hello, World!")

data Bool : Set where
  true  : Bool
  false : Bool

if_then_else_ : { A : Set } → Bool → A → A → A
if true then t else e = t
if false then t else e = e
