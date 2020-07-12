module plfa-exercises.Practice4 where

open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; cong)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Product using (_×_ ; ∃-syntax; ∃; _,_) -- renaming (_,_ to ⟨_,_⟩)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_; _≤_; _≤?_; _≟_; s≤s; z≤n)
open import Data.Nat.Binary.Properties using (≰⇒>)
open import Data.Nat.Properties using (≤-refl; ≤-trans; ≤-antisym; ≤-total)
open import Data.Maybe using (Maybe; just; nothing; maybe; _>>=_; from-just; From-just)
open import Data.Bool using (Bool; true; false; T; _∧_; _∨_; not)
open import Relation.Nullary.Decidable using (⌊_⌋; toWitness; fromWitness; from-yes; From-yes)
open import Relation.Nullary using (¬_; Dec; yes; no)
open import Data.Unit using (⊤; tt)

--data OrderedTree (A : Set) : (smallest middle biggest : ℕ) → Set where
--  leaf : ∀ (n : ℕ) → A → OrderedTree A n n
--  node : ∀ {s₁ m₁ b₁ s₂ m₂ b₂ : ℕ}
--       → (m : ℕ)
--       → b₁ ≤ m
--       → m ≤ s₂
--       → OrderedTree A s₁ m₁ b₁
--       → OrderedTree A s₂ m₂ b₂
--       → OrderedTree A s₁ m b₂
--
---- node 7 _ _ (leaf 5 300) (leaf 10 200)
----middle_values_in_node_are_smaller : 
--
--insert_left : ∀ {s m b : ℕ} → (n : ℕ) → A → OrderedTree A s m b

data BST : ℕ → ℕ → Set where
  leaf : ∀ {l u : ℕ} → l ≤ u → BST l u
  node : ∀ {l l₀ u₁ u : ℕ} (d : ℕ)
       → BST l₀ d
       → BST d  u₁
       → l  ≤ l₀ -- This overcomplicates some stuff in later code. Better get rid off it
       → u₁ ≤ u
       → BST l u

-- node {l = 0} {u = 30} 5 (leaf {2} {5} _) (leaf {5} {6} _)

-- If we use the long definition of BST this function gets short, otherwise it's quite complicated
widen : ∀ {l l' u u' : ℕ}
      → BST l' u'
      → l ≤ l'
      → u' ≤ u
      → BST l u
widen (leaf l'≤u') l≤l' u'≤u = leaf (≤-trans l≤l' (≤-trans l'≤u' u'≤u))
widen (node d left right l'≤l₀ u₁≤u') l≤l' u'≤u = node d left right (≤-trans l≤l' l'≤l₀) (≤-trans u₁≤u' u'≤u)

-- node {l = 0} {u = 30} 5 (leaf {2} {5} _) (widen (leaf {23} {29} _) _ _) z≤n _

a-tree : BST 0 30 -- A tree with a single node!!
--a-tree = node 5 (leaf 2≤5) (leaf 5≤30) z≤n 30≤30
a-tree = node 5 (leaf 2≤5) (widen (leaf 23≤29) 5≤23 29≤30) z≤n 30≤30
--a-tree = node {l = 0} {u = 30} 5 (leaf {2} {5} 2≤5) (widen (leaf {23} {29} 23≤29) 5≤23 29≤30) z≤n 30≤30
  where
    --2≤5   = toWitness {Q =  2 ≤?  5} tt
    2≤5   = from-yes ( 2 ≤?  5)
    23≤29 = from-yes (23 ≤? 29)
    5≤23  = from-yes ( 5 ≤? 23)
    29≤30 = from-yes (29 ≤? 30)
    30≤30 = from-yes (30 ≤? 30)

binary-search : ∀ {l u : ℕ} → (n : ℕ) → l ≤ n → n ≤ u → BST l u → Maybe (∃[ l' ] (∃[ u' ] (BST l' u')))
binary-search _ _ _ (leaf _) = nothing
binary-search n l≤n n≤u (node {l'} {l₀} {u₁} {u'} d left right l'≤l₀ u₁≤u') with d ≟ n
--binary-search n _ _ (node {_} {l} {_} {u} d left right l≤l₀ u₁≤u) with d ≟ n
...             | yes d=n = just (l' , u' , node d left right l'≤l₀ u₁≤u')
...             | no  d≠n  with ≤-total n d | l₀ ≤? n | n ≤? u₁
...                           | inj₁ n≤d | yes l₀≤n | _         = binary-search n l₀≤n n≤d left
...                           | inj₁ n≤d | no ¬l₀≤n | _         = nothing -- This shouldn't exist if the definition of BST wasn't so overcomcomplicated
...                           | inj₂ d≤n | _        | yes n≤u₁  = binary-search n d≤n n≤u₁ right
...                           | inj₂ d≤n | _        | no ¬n≤u₁  = nothing

smaller : ℕ → ℕ → Bool
smaller m n with m ≤? n
...            | yes m≤n = true
...            | _       = false

_≤ℕ_ : ∀ (m n : ℕ) → From-yes (m ≤? n)
m ≤ℕ n = from-yes (m ≤? n)

--_ : Maybe (∃ (λ l' → ∃ (BST l')))
--_ = binary-search 5 _ _ a-tree ≡ nothing
  --where
    --5≤30 = from-yes (5 ≤? 30)
_ : binary-search 2 z≤n (2 ≤ℕ 30) a-tree ≡ nothing
_ = refl

_ : binary-search 5 z≤n (5 ≤ℕ 30) a-tree ≡ just (0 , 30 , node 5 _ _ _ _)
_ = refl

insert : ∀ {l u} (d : ℕ) → l ≤ d → d ≤ u → BST l u → BST l u
insert d l≤d d≤u (leaf _) = node d (leaf l≤d) (leaf d≤u) ≤-refl ≤-refl
insert d l≤d d≤u (node {_} {l₀} m left right l≤m m≤u) with ≤-total m d
...                | inj₁ m≤d = node m (insert d ? ? left) right l≤m m≤u -- This shouldn't exist if the definition of BST wasn't so overcomcomplicated
...                | inj₂ d≤m = ?

-- ≰⇒> ¬3≤4
