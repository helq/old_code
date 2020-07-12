module sv20.assign2.Second_old2 where
-- ------------------------------------------------------------------------

open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong)
open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Nat.Properties using (≡-decSetoid)
open import Data.List using (List; []; _∷_; _++_)
open import Relation.Binary using (Setoid; DecSetoid)
open import Data.Product using (_×_) renaming (_,_ to ⟨_,_⟩)
open import Relation.Nullary using (¬_; Dec; yes; no)

-- ℕsetoid : Setoid lzero lzero
-- ℕsetoid = record {
--     Carrier = ℕ
--   ; _≈_ = _≡_
--   ; isEquivalence = record {
--       refl = refl
--     ; sym = sym
--     ; trans = trans
--     }
-- }

-- open import Data.Product.Relation.Binary.Pointwise.NonDependent as Pointwise
--   using (Pointwise)
-- 
-- ℕ×ℕdecsetoid : DecSetoid lzero lzero
-- ℕ×ℕdecsetoid = record {
--     Carrier = ℕ × ℕ
--   ; _≈_ = _≡_
--   ; isDecEquivalence = record
--     { isEquivalence = record { refl = refl ; sym = sym ; trans = trans }
--     ; _≟_           = ? -- _≟_
--     }
--   }

import Data.List.Relation.Binary.Subset.Setoid as SubSetoid
open SubSetoid (DecSetoid.setoid ≡-decSetoid) using (_⊆_)

--import Data.List.Membership.Setoid
--module ListSetoid = Data.List.Membership.Setoid (ℕsetoid)
--open ListSetoid using (_∈_)

open import Data.List.Membership.DecSetoid (≡-decSetoid) using () renaming (_∈?_ to _∈ℕ?_)
--open import Data.List.Membership.DecSetoid (ℕ×ℕdecsetoid) using () renaming (_∈?_ to _∈ℕ×ℕ?_)
open import Data.List.Relation.Unary.Any using (Any; index; map; here; there)

--simple : [] ⊆ (2 ∷ [])
--simple ()
--
--simple₂ : (1 ∷ []) ⊆ (2 ∷ 1 ∷ [])
--simple₂ (here px) = there (here px)

--Look at:
-- https://agda.github.io/agda-stdlib/Data.List.Relation.Binary.Subset.Propositional.Properties.html
-- https://agda.github.io/agda-stdlib/Data.List.Relation.Binary.Subset.Setoid.html#692
-- https://agda.github.io/agda-stdlib/Relation.Binary.Structures.html#1522

--simple₃ : 1 ∈ (2 ∷ 1 ∷ [])
--simple₃ = ?

-- ⟨Set⟩ are basically lists in Athena. Which is not what a set should be but
-- the professor doesn't care about this, so I can implement this using lists
-- data relation {a b : Set} (A : ⟨Set⟩ a) (B : ⟨Set⟩ b) : ⟨Set⟩ (a × b) where
--
--relation = _×_
--    
--range-theorem-2 : ∀ {a b} {A : ⟨Set⟩ a} {B : ⟨Set⟩ b}
--                  (F G : ⟨Set⟩ (a × b))
--                → Range (F intersect G) ⊆ (Range F) intersect (Range G)
--
--range-theorem-2 : ∀ {a b} {A : ⟨Set⟩ a} {B : ⟨Set⟩ b}
--                  (F G : ⟨Set⟩ (a × b))
--                → Range (F ∩ G) ⊆ (Range F) ∩ (Range G)
--
--dom-theorem-3 : ∀ {A B : ⟨Set⟩}
--                (F G : A × B)
--              → (Dom F) diff (Dom G) ⊆ Dom (F diff G)
--    
--range-theorem-3 : ∀ {a b} {A : ⟨Set⟩ a} {B : ⟨Set⟩ b}
--                  (F G : A × B)
--                → (Range F) diff (Range G) ⊆ Range (F diff G)

dom : ∀ {A B : Set} → List (A × B) → List A
dom [] = []
dom (⟨ x , _ ⟩ ∷ ls) = x ∷ dom ls

range : ∀ {A B : Set} → List (A × B) → List B
range [] = []
range (⟨ _ , y ⟩ ∷ ls) = y ∷ range ls

_∩ℕ_ : List ℕ → List ℕ → List ℕ
--_∩ : List A → List A → List A
[] ∩ℕ _ = []
--xs ∩ ys = ?
(x ∷ xs) ∩ℕ ys with x ∈ℕ? ys
...               | yes _ = x ∷ (xs ∩ℕ ys)
...               | no  _ = xs ∩ℕ ys

--_intersectℕ×ℕ_ : List (ℕ × ℕ) → List (ℕ × ℕ) → List (ℕ × ℕ)
----_intersect_ : List A → List A → List A
--[] intersectℕ×ℕ _ = []
----xs intersect ys = ?
--(x ∷ xs) intersectℕ×ℕ ys with x ∈ℕ×ℕ? ys
--...               | yes _ = x ∷ (xs intersectℕ×ℕ ys)
--...               | no  _ = xs intersectℕ×ℕ ys

simple-theorem : (F G : List ℕ)
               → (F ∩ℕ G) ⊆ F
simple-theorem []       _  p = p
simple-theorem (x ∷ xs) ys = ? -- NO IDEA HOW TO PROCEDE EVEN WITH THE "SIMPLEST" OF CASES
--simple-theorem (x ∷ xs) ys = ?
-- λ x xs ys x₁ → x₁ ListSetoid.∈ ((x ∷ xs) intersectℕ ys) → x₁ ListSetoid.∈ x ∷ xs}
--simple-theorem [] _ p      = p
--simple-theorem (x ∷ xs) [] = ?
--simple-theorem (x ∷ xs) (y ∷ ys) = ?

-- (2 ∷ 3 ∷ 60 ∷ []) intersectℕ (1 ∷ 3 ∷ 2 ∷ [])

-- (⟨ 2 , 0 ⟩ ∷ ⟨ 3 , 2 ⟩ ∷ ⟨ 60 , 1 ⟩ ∷ []) intersectℕ (1 ∷ 3 ∷ 2 ∷ [])

--module Intersect {a ℓ} (DS : DecSetoid a ℓ) where
--  open import Data.List.Membership.DecSetoid (DS) using (_∈?_)
--  --open DecSetoid DS using (_≟_)
--
--  A = DecSetoid.Carrier DS
--
--  --_intersect_ : List ℕ → List ℕ → List ℕ
--  _intersect_ : List A → List A → List A
--  [] intersect ys = ys
--  --xs intersect ys = ?
--  (x ∷ xs) intersect ys with x ∈? ys
--  ...               | yes _ = x ∷ (xs intersect ys)
--  ...               | no  _ = xs intersect ys
--
--
--import Data.List.Relation.Binary.Subset.Setoid as SubSetoid
--
--module Range-Theorem-2 {a ℓ} (DS₁ : DecSetoid a ℓ) (DS₂ : DecSetoid a ℓ) where
--  A = DecSetoid.Carrier DS₁
--  B = DecSetoid.Carrier DS₂
--
--  DSboth : DecSetoid a ℓ
--  DSboth = record {
--      Carrier = A × B
--    ; _≈_ = ?
--    ; isDecEquivalence = ?
--    }
--
--  open Intersect (DSboth) using (_intersect_)
--  open Intersect (DS₂) using () renaming (_intersect_ to _intersect₂_)
--
--  S₂setoid : Setoid a ℓ
--  S₂setoid = record {
--      Carrier = B
--    ; _≈_ = DecSetoid._≈_ DS₂
--    ; isEquivalence = Setoid.isEquivalence (DecSetoid.setoid DS₂)
--    }
--  open SubSetoid (S₂setoid) using (_⊆_)
--  
--  range-theorem-2 : --∀ {A B : Set}
--                    (F G : List (A × B))
--                    --(F G : List (ℕ × ℕ))
--                  → range (F intersect G) ⊆ (range F) intersect₂ (range G)
--  range-theorem-2 [] _ p      = p
--  range-theorem-2 (x ∷ xs) [] = ?
--  range-theorem-2 (x ∷ xs) (y ∷ ys) = ?

--open import Data.AVL.Sets using (⟨Set⟩; empty; insert; delete)
