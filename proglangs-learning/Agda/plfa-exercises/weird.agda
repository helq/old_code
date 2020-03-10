module plfa-exercises.weird where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Bool.Base using (Bool; true; false; _∧_; _∨_; not)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; sym; trans; cong; subst)

data ℕ∨Bool : Set where
  N B : ℕ∨Bool

-- Idea taken from paper: Fractional Types by Chao-Hong Chen et al.
⟦_⟧ : ℕ∨Bool → Set
⟦ N ⟧ = ℕ
⟦ B ⟧ = Bool

manyinputs : {a : ℕ∨Bool} → ⟦ a ⟧ → ⟦ a ⟧
manyinputs {N} n = suc n
manyinputs {B} b = not b

_ : manyinputs 6 ≡ 7
_ = refl

_ : manyinputs true ≡ false  -- WOW!!
_ = refl
