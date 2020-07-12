open import Data.Nat --using (ℕ; suc; zero; _≤?_; _≤_; s≤s; z≤n)
open import Data.Bool using (true; false)
--open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Relation.Nullary -- using (Dec; yes; no; _because_)
open import Relation.Nullary.Decidable --using (True; fromWitness)
--open import Data.Unit using (tt)

-- stack exec -- agda -c --ghc-dont-call-ghc --no-main nats.agda

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : (x : A) → Maybe A
{-# COMPILE GHC Maybe = data Maybe (Nothing | Just) #-}

n-3 : (n : ℕ) → .{3≤n : True (3 ≤? n)} → ℕ
n-3 (suc (suc (suc n))) = n

exportFunc : ℕ → Maybe ℕ
exportFunc n with 3 ≤? n
... | yes 3≤n = just (n-3 n {fromWitness 3≤n})
... | no ¬3≤n = nothing
{-# COMPILE GHC exportFunc as exportFunc #-}
