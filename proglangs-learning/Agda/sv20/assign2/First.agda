-- For the first task, we are asked to prove that max is idempotent and
-- associative. The proves presented in this file are as succint as they
-- possibly can. The proves come with added comments to aid understanding as
-- the proves are meant to be short and simple but not very legible. The
-- solution to task 1 starts in lines 44 and 51.

module sv20.assign2.First where

open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong)
open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Nat using (_≤_; z≤n; s≤s; _<_)
open import Relation.Nullary using (¬_)
open import Data.Empty using (⊥-elim)
open import Function using (_∘_)

-- First, we define the max function. Notice, how the function differs from the
-- definition offered for the assignment
max : ℕ → ℕ → ℕ
max zero    n       = n
max (suc m) zero    = suc m
max (suc m) (suc n) = suc (max m n)

-- Nonetheless, we can prove that the definition of max presented in the
-- assignment follows from ours.
-- In athena, the first part of the definition corresponds to the sentence
-- "y < x ==> x max y = x", which translated into Agda corresponds to:
less : ∀ {x y} → y < x → max x y ≡ x
-- and it is easily proven by:
less {zero} ()                       -- Nonesense case
less {suc m} {zero} (s≤s z≤n) = refl -- Simplest case, y ≡ 0
less {suc m} {suc n} (s≤s n<m) = cong suc (less n<m)  -- if "n < m → max m n ≡ m" then "suc n < suc m → max (suc m) (suc n) ≡ (suc m)"

-- The second part of the definition asserts (in Athena) that
-- "~ y < x ==> x max y = y"
not-less : ∀ {x y} → ¬ (y < x) → max x y ≡ y
-- which is easily proven by induction with:
not-less {zero} {n} ¬y<x = refl -- Base case 1
not-less {suc m} {zero} ¬z<sm = ⊥-elim (¬z<sm (s≤s z≤n)) -- Base case 2
not-less {suc m} {suc n} ¬sn<sm = cong suc (not-less (¬sn<sm ∘ s≤s)) -- Inductive case
--not-less {suc m} {suc n} ¬sn<sm = cong suc (not-less {m} {n} (λ n<m → ¬sn<sm (s≤s n<m)))
-- The pattern ¬sn<sm ∘ s≤s means that you are creating a function ¬n<m from ¬sn<sm

-- SOLUTIONS
-- max is idempotent
-- By induction:
max-idem : ∀ x → max x x ≡ x
max-idem zero    = refl                  -- Base case: max 0 0 ≡ 0
max-idem (suc n) = cong suc (max-idem n) -- Inductive case: if "max n n ≡ n" then "max (suc n) (suc n) ≡ suc n" because "max (suc n) (suc n) ≡ suc (max n n)"

-- max is associative
-- By induction:
max-assoc : ∀ x y z → max x (max y z) ≡ max (max x y) z
max-assoc zero    _       _       = refl -- Base cases: when one of the numbers is 0, then "max 0 n ≡ n" by definition of max
max-assoc (suc m) zero    _       = refl --
max-assoc (suc m) (suc n) zero    = refl --
max-assoc (suc m) (suc n) (suc o) = cong suc (max-assoc m n o) -- Inductive case: if "max m (max n o) ≡ max (max m n) o" then "max (suc m) (max (suc n) (suc o)) ≡ suc (max m (max n o)) ≡ suc (max (max m n) o) ≡ max (max (suc m) (suc n)) (suc o)"
