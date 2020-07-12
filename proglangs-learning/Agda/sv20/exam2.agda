module sv20.exam2 where

-------------------------- FIRST TASK PREPARATION CODE --------------------------
open import Data.Unit using (⊤; tt)
open import Data.Product using (_×_ ; ∃) renaming (_,_ to ⟨_,_⟩)
open import Relation.Nullary using (¬_)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Function using (_∘_)
import Relation.Binary.PropositionalEquality as Eq
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_; _≡˘⟨_⟩_; _∎)
open Eq using (_≡_; refl; cong; sym; trans)

Subset : (A : Set) -> Set _
Subset A = A → Set

_∈_ : ∀ {A} → A → Subset A → Set
a ∈ P = P a

Relation : ∀ A B → Set₁
Relation A B = Subset (A × B)

Range : ∀ {A B} → Relation A B → Subset B
Range R b = ∃ (R ∘ ⟨_, b ⟩)  -- equivalent to ∃ \a → R ⟨ a , b ⟩

Dom : ∀ {A B} → Relation A B → Subset A
Dom R a = ∃ (R ∘ ⟨ a ,_⟩)  -- equivalent to ∃ \b → R ⟨ a , b ⟩

_⊆_ : ∀ {A} → Subset A → Subset A → Set
A ⊆ B = ∀ x → x ∈ A → x ∈ B

_∩_ : ∀ {A} → Subset A → Subset A → Subset A
A ∩ B = λ x → x ∈ A × x ∈ B

_∪_ : ∀ {A} → Subset A → Subset A → Subset A
A ∪ B = λ x → x ∈ A ⊎ x ∈ B

_/_ : ∀ {A} → Subset A → Subset A → Subset A
A / B = λ x → x ∈ A × ¬ (x ∈ B)

postulate
  ⊆→≡ : ∀ {a} {A B : Subset a} → A ⊆ B → B ⊆ A → A ≡ B

-------------------------- Solution to TASK 1 --------------------------

diff-theorem-3 : ∀ {a} {A B : Subset a}
               → (A / (A ∩ B)) ≡ (A / B)
-- The proof is divided in two parts, forwards (right contained in left) and
-- backwards (left contained in right)
diff-theorem-3 = ⊆→≡ diff-theorem-3→ diff-theorem-3←
  where
    diff-theorem-3→ : ∀ {a} {A B : Subset a}
                    → (A / (A ∩ B)) ⊆ (A / B)
    diff-theorem-3→ x x∈A/A∩B =           -- Given an x such that `x ∈ (A / (A ∩ B))`
      let
        ⟨ x∈A , ¬x∈A∩B ⟩ = x∈A/A∩B        -- (1) By def. of diff, we have that `x ∈ A` and `¬ x ∈ A ∩ B`
        ¬x∈B  =                           -- (2) Proof by contradiction that ¬ x ∈ B
               λ{ x∈B →                    -- (a) Suppose `x ∈ B`
               let x∈A∩B = ⟨ x∈A , x∈B ⟩   -- (b) then by def. of ∩ and (1) (a), we have that `x ∈ A ∩ B`
               in ¬x∈A∩B x∈A∩B }           -- Contradiction `x ∈ A ∩ B` (b) and `¬ x ∈ A ∩ B` (1).
        x∈A/B = ⟨ x∈A , ¬x∈B ⟩            -- From (1), (2) and by def. of /, we have that `x ∈ A / B`
      in x∈A/B                            -- QED
    --Proof without comments
    --diff-theorem-3→ x ⟨ x∈A , ¬x∈A∩B ⟩ = ⟨ x∈A , (λ{x∈B → ¬x∈A∩B ⟨ x∈A , x∈B ⟩}) ⟩
    
    diff-theorem-3← : ∀ {a} {A B : Subset a}
                    → (A / B) ⊆ (A / (A ∩ B))
    diff-theorem-3← x x∈A/B =             -- Given an x such that `x ∈ A / B`
      let
        ⟨ x∈A , ¬x∈B ⟩ = x∈A/B            -- (1) By def. of diff, we have that `x ∈ A` and `¬ x ∈ B`
        ¬x∈A∩B =                          -- (2) Proof by contradiction that `¬ x ∈ A ∩ B`
                λ{ x∈A∩B →                 -- (a) Suppose `x ∈ A ∩ B`
                let ⟨ x∈A , x∈B ⟩ = x∈A∩B  -- (b) then by def. of ∩, we have that `x ∈ A` and `x ∈ B`
                in ¬x∈B x∈B }              -- Contraduction `x ∈ B` (b) and `¬ x ∈ B` (1)
        x∈A/A∩B = ⟨ x∈A , ¬x∈A∩B ⟩        -- From (1), (2) and by the def. of /, `x ∈ (A / (A ∩ B))`
      in x∈A/A∩B                          -- QED
    --Proof without comments
    --diff-theorem-3← x ⟨ x∈A , ¬x∈B ⟩ = ⟨ x∈A , (λ{⟨ x∈A , x∈B ⟩ → ¬x∈B x∈B}) ⟩

-- This might be another way to prove it, it still seems to require some postulate to work
--diff-theorem-3` : ∀ {a} {A B : Subset a}
--                (y : a)
--                → y ∈ (A / (A ∩ B)) ≡ y ∈ (A / B)
---- The proof is divided in two parts, forwards (right contained in left) and
---- backwards (left contained in right)
--diff-theorem-3` {_} {A} {B} y =
--  begin
--    y ∈ (A / (A ∩ B))
--  ≡⟨⟩
--    (A / (A ∩ B)) y
--  ≡⟨⟩
--    (A y × ¬ (A y × B y))
--  ≡⟨ cong (A y ×_) ? ⟩
--    (A y × ¬ (B y))
--  ≡⟨⟩
--    (A / B) y
--  ≡⟨⟩
--    y ∈ (A / B)
--  ∎

-------------------------- END FIRST TASK --------------------------

-------------------------- SECOND TASK PREPARATION CODE --------------------------

open import Data.List using (List; []; _∷_; _++_; length; reverse; map; foldr; downFrom; foldl)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Nat.Properties using (*-identityʳ; +-identityʳ; *-assoc)

pattern [_] z = z ∷ []

product : List ℕ → ℕ
product [] = 1
product (n ∷ ns) = n * product ns

-------------------------- Solution to TASK 2 --------------------------

of-singleton : ∀ (n : ℕ) → product [ n ] ≡ n
of-singleton n =
  begin
    product [ n ]     ≡⟨⟩
    product (n ∷ [])  ≡⟨⟩
    n * product []    ≡⟨⟩
    n * 1             ≡⟨ *-identityʳ n ⟩
    n
  ∎

of-join : ∀ (ms ns : List ℕ)
        → product (ms ++ ns) ≡ (product ms) * (product ns)
of-join [] ns = 
  begin
    product ([] ++ ns)       ≡⟨⟩
    product ns               ≡˘⟨ +-identityʳ (product ns) ⟩  -- product ns ≡ 1 * product ns
    1 * product ns           ≡⟨⟩
    product [] * product ns
  ∎
of-join (m ∷ ms) ns =
  begin
    product ((m ∷ ms) ++ ns)        ≡⟨⟩
    product (m ∷ (ms ++ ns))        ≡⟨⟩
    m * product (ms ++ ns)          ≡⟨ cong (m *_) (of-join ms ns) ⟩ -- product (ms ++ ns) ≡ product ms * product ns
    m * (product ms * product ns)   ≡˘⟨ *-assoc m (product ms) (product ns) ⟩ -- * associativity
    (m * product ms) * product ns   ≡⟨⟩
    product (m ∷ ms) * product ns
  ∎

-------------------------- END SECOND TASK --------------------------
