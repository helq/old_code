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


open import Relation.Nullary using (¬_)
open import Data.Unit using (⊤; tt)
open import Data.Empty using (⊥; ⊥-elim)
-- Actually this should use "isomorphisms" not mere 'iff' between propositions.
-- https://agda.github.io/agda-stdlib/v1.1/Function.Inverse.html#2229
open import plfa.part1.Isomorphism using (_⇔_)

lemma₁ : ¬ ⊤ → ⊥
lemma₁ ¬⊤ = ¬⊤ tt

lemma₂ : ⊥ → ¬ ⊤
lemma₂ ()

-- I can easily prove this!! Which is not the same as ¬ ⊤ same as ⊥,
-- but ¬ ⊤ iff ⊥, where ⊤ and ⊥ are predicates
prf₀ : (¬ ⊤) ⇔ ⊥
prf₀ = record {
    to = lemma₁
  ; from = lemma₂
  }

-- But how to prove the following?
--prf : ¬ ⊤ ≡ ⊥
--prf = ?  -- seems to require extensionality and iso-is-≡, or cubical, see https://stackoverflow.com/a/58912530

open import Level using (0ℓ)
open import Agda.Primitive using (lsuc)
open import Relation.Binary.Definitions using (Substitutive; _Respects_; _⟶_Respects_)

prf₂ : ¬ (⊤ ≡ ⊥)  -- same as:  ⊤ ≢ ⊥
prf₂ ⊤≡⊥ = subst (λ x → x) ⊤≡⊥ tt
-- We don't need to give the implicit parameters but doing so lets us look
-- inside of the definitions and how they get reduced to other terms
--prf₂ ⊤≡⊥ = subst {A = Set} {ℓ = 0ℓ} (λ x → x) ⊤≡⊥ tt

-- `subst` has type: {a : Level} {A : Set a} {ℓ : Level} → Substitutive _≡_ ℓ
-- `subst {A = Set}` has type: {ℓ : Level} → Substitutive _≡_ ℓ
-- `subst {A = Set} {ℓ = 0ℓ}` has type: Substitutive _≡_ 0ℓ
--    actually it has type: Substitutive {A = Set} _≡_ 0ℓ
-- `Substitutive {A = Set} _≡_ 0ℓ` which reduces to  (P : Set → Set) {x y : Set} → x ≡ y → P x → P y
-- which means that the type of
--  `subst {A = Set} {ℓ = 0ℓ}` 
-- is
--  (P : Set → Set) {x y : Set} → x ≡ y → P x → P y
-- thus, the type of 
--  `subst {A = Set} {ℓ = 0ℓ} (λ x → x)`   (*)
-- is
--   {x y : Set} → x ≡ y → P x → P y
-- Note: careful, Agda tells us that the type of (*) is:
--   (λ x → x) Respects _≡_
-- but it is actually:
--   _Respects_ {A = Set} (λ x → x) _≡_
--
-- From reading the code:
--   _Respects_ {A = Set} (λ x → x) _≡_   (**)
-- reduces first to:
--   _⟶_Respects_ {A = Set} (λ x → x) (λ x → x) _≡_
--
-- There is even more. (**) actually has some implicit parameters, here they are
--   _Respects_ {A = Set} {ℓ₁ = 0ℓ} {ℓ₂ = lsuc 0ℓ} (λ x → x) _≡_
--
-- LESSON LEARNT: Implicit arguments are cool because you don't need to give
-- them, Agda can generally infer them from context which makes code to look
-- prettier but excesively harder to read
--
-- Other stuff:
-- `Substitutive {A = Set} _≡_ 0ℓ` reduces to  (P : Set → Set) {x y : Set} → x ≡ y → P x → P y
-- `Substitutive {A = Set} _≡_ (lsuc 0ℓ)` reduces to  (P : Set → Set₁) {x y : Set} → x ≡ y → P x → P y
