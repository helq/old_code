module sv20.class-exercise where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; s≤s; z≤n)

-- Subset definition taken from: https://agda.readthedocs.io/en/v2.6.1/language/irrelevance.html
-- Notice that irrelevance is not the same as function dot-pattern: https://agda.readthedocs.io/en/v2.6.1/language/function-definitions.html#dot-patterns

postulate
  .irrAx : ∀ {ℓ} {A : Set ℓ} -> .A -> A

record Subset (A : Set) (P : A -> Set) : Set where
  constructor _#_
  field
    elem         : A
    .certificate : P elem

.certificate : {A : Set}{P : A -> Set} -> (x : Subset A P) -> P (Subset.elem x)
certificate (a # p) = irrAx p

_ : Subset ℕ (_≤ 3)
_ = 3 # s≤s (s≤s (s≤s z≤n))
--_ = 0 # z≤n

--_∈_ : ?
--(a # p) ∈ s = ?
