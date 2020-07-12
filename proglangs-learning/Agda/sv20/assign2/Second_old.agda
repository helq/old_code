{-# OPTIONS --cubical #-}

--open import Data.Nat using (ℕ; zero; suc; _+_)
open import Cubical.Data.Nat using (ℕ; zero; suc; _+_)
open import Cubical.Foundations.Prelude using (refl; _≡_; subst; cong; transport; transp; ℓ-zero; ℓ-suc) -- hiding (_≡⟨_⟩_)
open import Cubical.Foundations.Function using (_∘_)

data Captured {a} (A : Set a) : Set a where
  cap : A → Captured A

Id : ∀ {a} {A} → Captured A → Set a
Id {_} {A} (cap _) = A

id : ∀ {a} {A} → (n : Captured A) → Id {a} n
id (cap n) = n

data ⟨Set⟩ {a} (A : Set a) : Set a where
  empty : ⟨Set⟩ A
  insert : A → ⟨Set⟩ A → ⟨Set⟩ A
  dup : ∀ a sa → insert a (insert a sa) ≡ insert a sa
  com : ∀ a b sa → insert a (insert b sa) ≡ insert b (insert a sa)

rem-dup ()

--_in_ : A → ⟨Set⟩ A → 

--insert 3 (insert 3 empty)
--transport (dup 3 empty))

--transport (dup {ℓ-zero} 3 empty)
--insert 3 (insert 3 empty)
--subst {A = insert 3 (insert 3 empty)} (dup 3 empty)
--transport {ℓ-zero} {ℕ} refl
--cong cap (dup 3 empty)
--transport (cong (Id ∘ cap) (dup 3 empty)) (id (cap (insert 3 (insert 3 empty))))
--id (cap (insert 3 (insert 3 empty)))
