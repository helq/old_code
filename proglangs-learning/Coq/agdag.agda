open import Data.Bool using (Bool; true; false)
open import Data.Empty using (⊥)
open import Data.Unit using (⊤; tt)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)
open import Relation.Nullary using (¬_)

--data _≡_ {A : Set} (x : A) : A → Set where
--  refl : x ≡ x

trans : ∀ {a} {A : Set a} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

-- This can be done in Coq but not in Agda?
--disc'' : ∀ {x : Bool} → x ≡ x → x ≡ (¬ x) → ⊥
--disc'' = ?

-- How to prove this???
disc' : ∀ {a} {x : Set a} → x ≡ x → x ≡ (¬ x) → ⊥
disc' refl = ?

disc : ∀ {a} {x : Set a} → ¬ (x ≡ (¬ x))
disc {x} x≡¬x = ? -- disc' ? ? -- (refl {x}) x≡¬x


open import Data.Nat using (ℕ; suc; zero; _+_)

right-zero : ∀ n → n + 0 ≡ n
right-zero 0 = refl
right-zero (suc n) = cong suc (right-zero n)

comm : ∀ m n p → m + (n + p) ≡ (m + n) + p
comm 0 _ _ = refl
comm (suc m) n p = cong suc (comm m n p)
