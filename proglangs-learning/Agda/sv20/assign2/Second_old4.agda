module sv20.assign2.Second_old4 where

open import Data.Bool as Bool using (Bool; true; false; T; _∨_; _∧_)
--open import Relation.Nullary using (¬_)
open import Data.Unit using (⊤; tt)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Level using (Level; _⊔_; 0ℓ) renaming (suc to lsuc)
--open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Product using (_×_; ∃-syntax) renaming (_,_ to ⟨_,_⟩)
open import Function using (_∘_)
--import Relation.Binary.PropositionalEquality as Eq
--open Eq using (_≡_; refl; subst)

Subset : ∀ {α} (A : Set α) -> Set _
Subset A = A → Bool 

_∈_ : ∀ {α} {A : Set α} → A → Subset A → Set
a ∈ p = T (p a)

--odd : Subset ℕ
--odd 0 = false
--odd 1 = true
--odd (suc (suc n)) = odd n
--
--_ : 3 ∈ odd
--_ = tt
--
--_ : Subset Set  -- I'm not actually sure, what this is
--_ = λ _ → true

-- _ : true ∈ odd -- Doesn't make sense. Type inference fails
--_ = ?

--∈or∉ : ∀ {A : Set} (SubA : Subset A) (a : A) → (a ∈ SubA) ⊎ ¬ (a ∈ SubA)
--∈or∉ subset a with subset a
--...              | true  = inj₁ tt
--...              | false = inj₂ ⊥-elim
--
--empty-⊥ : ∀ (sub : Subset ⊥) (a : ⊥) → ¬ (a ∈ sub)
--empty-⊥ _ ()

--¬⊥ : ∀ (A : Set) → ¬ (⊥ ∈ A)  -- doesn't make any sense
--¬⊥ = ?

Relation : ∀ {α β} (A : Set α) (B : Set β) → Set (α ⊔ β)
Relation A B = Subset (A × B)

--equalℕ : Relation ℕ ℕ
--equalℕ ⟨ zero    , zero    ⟩ = true
--equalℕ ⟨ zero    , (suc n) ⟩ = false
--equalℕ ⟨ (suc m) , zero    ⟩ = false
--equalℕ ⟨ (suc m) , (suc n) ⟩ = equalℕ ⟨ m , n ⟩

--_ : equalℕ ⟨ 3 , 4 ⟩ ≡ false
--_ = refl

_∪_ : ∀ {A : Set} → Subset A → Subset A → Subset A
A ∪ B = λ x → (A x) ∨ (B x)

_∩_ : ∀ {A : Set} → Subset A → Subset A → Subset A
A ∩ B = λ x → (A x) ∧ (B x)

_⊆_ : ∀ {A : Set} → Subset A → Subset A → Set
A ⊆ B = ∀ x → x ∈ A → x ∈ B

wholeSet : ∀ (A : Set) → Subset A
wholeSet _ = λ _ → true

--odd⊆ℕ : odd ⊆ wholeSet ℕ
----odd⊆ℕ zero ()
----odd⊆ℕ 1 tt = tt
----odd⊆ℕ (suc (suc n)) _ = tt
---- or, simply
--odd⊆ℕ _ _ = tt

-- In general
∀subset⊆set : ∀ {A : Set} {sub : Subset A} → sub ⊆ wholeSet A
∀subset⊆set = λ _ _ → tt

record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A

record Range (A B : Set) : Set where
  field
    range : Relation A B → Subset B
    def : ∀ (rel : Relation A B) (b : B)
        → (∃[ a ] (⟨ a , b ⟩ ∈ rel))
          ⇔ (b ∈ range rel)

--lemma : ∀ {A B : Set}
--        (F G : Relation A B)
--        (b : B)
--      → ∃[ a ] (⟨ a , b ⟩ ∈ (F ∩ G))
--      → ∃[ a ] ((⟨ a , b ⟩ ∈ F) × (⟨ a , b ⟩ ∈ G))
--lemma f g b ⟨ a , f∩ga ⟩ with f ⟨ a , b ⟩ | g ⟨ a , b ⟩
--...                         | true        | true         = ⟨ ? , ⟨ ? , ? ⟩ ⟩
--...                         | true        | false        = ⊥-elim f∩ga
--...                         | false       | true         = ⊥-elim f∩ga
--...                         | false       | false        = ⊥-elim f∩ga
--  
--module range-props {A B : Set} (rng : Range A B) where
--  range = Range.range rng
--  def   = Range.def rng
--
--  range-∩-⊆ : (F G : Relation A B)
--            → range (F ∩ G) ⊆ (range F ∩ range G)
--  range-∩-⊆ f g b = ?
--    where
--      --def-rngf-→ = _⇔_.to (def f b)
--
--      -- b ∈ range (f ∩ g) → ∃[ a ] (⟨ a , b ⟩ ∈ (f ∩ g))
--      def-rngf-← = _⇔_.from (def (f ∩ g) b)
--      -- b ∈ range (f ∩ g) → ∃[ a ] ((⟨ a , b ⟩ ∈ F) × (⟨ a , b ⟩ ∈ G))
--      lemma' = (lemma f g b) ∘ def-rngf-←

-- Class exercise
-- Dammit! All definitions of subset I have found for agda make this problem not a problem
exercise103a : ∀ {a} {A B : Subset a}
             → (∀ x → x ∈ A → x ∈ B)
             → A ⊆ B
exercise103a ∀x→x∈A→x∈B = ∀x→x∈A→x∈B
