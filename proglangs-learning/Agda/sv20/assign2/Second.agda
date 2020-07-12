module sv20.assign2.Second where
-- The solution for the second task starts in line 53

open import Data.Unit using (⊤; tt)
open import Data.Product using (_×_ ; ∃) renaming (_,_ to ⟨_,_⟩)
open import Relation.Nullary using (¬_)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Function using (_∘_)

-- For the second task. We took one common definition of Subset mentioned in
-- https://stackoverflow.com/q/34183349

-- Notice that this definition is equivalent to the definition presented in the
-- Standard Library where Subset is a "record".

Subset : (A : Set) -> Set _
Subset A = A → Set

-- Because, we are using a different definition from the standard library, we
-- need to define what it means to be included in a subset, what is a relation,
-- what is the range and domain of a relation, and so forth.

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

-- Practice theorem
∪-range-⊆ : ∀ {A B} {F G : Relation A B}
          → (Range F ∪ Range G) ⊆ Range (F ∪ G)
∪-range-⊆ _ (inj₁ ⟨ a , fab ⟩) = ⟨ a , inj₁ fab ⟩
∪-range-⊆ _ (inj₂ ⟨ a , gab ⟩) = ⟨ a , inj₂ gab ⟩

-- =========== SOLUTION TO TASK 2 STARTS HERE ===========

-- range-theorem-2
range-∩-⊆ : ∀ {A B} {F G : Relation A B}
          → Range (F ∩ G) ⊆ (Range F ∩ Range G)
range-∩-⊆ b b∈RangeF∩G =  -- Given "b ∈ Range (F ∩ G)", we need to prove that "b ∈ (Range F ∩ Range G)"
  let
    ⟨ a , a,b∈F∩G ⟩ = b∈RangeF∩G  -- (1) if "b ∈ Range (F ∩ G)" then there exists 'a' such that "(a,b) ∈ (F ∩ G)"
    ⟨ a,b∈F , a,b∈G ⟩ = a,b∈F∩G   -- (2) per definition of intersection, if "(a,b) ∈ (F ∩ G)" then "(a,b) ∈ F" and "(a,b) ∈ G"
    b∈RangeF = ⟨ a , a,b∈F ⟩      -- (3) from (1) we know there is an 'a' such that "(a,b) ∈ F" (2), so "b ∈ Range F", per definition of Range
    b∈RangeG = ⟨ a , a,b∈G ⟩      -- (4) as above, "b ∈ Range G" because of 'a' (1) and "(a,b) ∈ F" (2)
    b∈RangeF∩RangeG = ⟨ b∈RangeF , b∈RangeG ⟩ -- (5) given "b ∈ Range F" (3) and "b ∈ Range G" (4), and definition of ∩ "b ∈ Range F ∩ Range G". QED
  in b∈RangeF∩RangeG

-- dom-theorem-3
dom-/-⊆ : ∀ {A B} {F G : Relation A B}
        → (Dom F / Dom G) ⊆ Dom (F / G)
dom-/-⊆ a a∈DomF/DomG =  -- Given "a ∈ (Dom F / Dom G)", we need to prove that "a ∈ Dom (F / G)"
  let
    ⟨ a∈DomF , ¬a∈DomG ⟩ = a∈DomF/DomG  -- (1) per definition of '/', "a ∈ (Dom F / Dom G)" is the same as "a ∈ Dom F" and "¬ a ∈ Dom G"
    ⟨ b , a,b∈F ⟩ = a∈DomF              -- (2) per definition of 'Dom', if "a ∈ Dom F" then there exists 'b' such that "(a,b) ∈ F"
    ¬a,b∈G =                            -- (3) From "¬ a ∈ Dom G" we can prove that "¬ (a, b₁) ∈ G" for any 'b₁'
            λ{ a,b∈G →                     -- (a) By contradiction, suposse that "(a, b₁) ∈ G" for some 'b₁'
            let a∈DomG = ⟨ _ , a,b∈G ⟩     -- (b) per definition of 'Dom', "a ∈ Dom G"
            in ¬a∈DomG a∈DomG }            -- We know that "¬ a ∈ Dom G" (3) and "a ∈ Dom G" (b). Nonesense! So, "¬ (a, b₁) ∈ G" for all 'b₁'
    a,b∈F/G = ⟨ a,b∈F , ¬a,b∈G ⟩        -- (4) per definition of '/' and given (2) and (3), "(a, b) ∈ F/G"
    a∈DomF/G = ⟨ b , a,b∈F/G ⟩          -- (5) per definition of 'Dom' and given (4) and 'b', we have that "a ∈ Dom (F / G)". QED
  in a∈DomF/G

-- range-theorem-3
/-range-⊆ : ∀ {A B} {F G : Relation A B}
          → (Range F / Range G) ⊆ Range (F / G)
/-range-⊆ b b∈RangeF/RangeG = -- Given "b ∈ (Range F / Range G)", we need to prove that "b ∈ Range (F / G)"
  let
    ⟨ b∈RangeF , ¬b∈RangeG ⟩ = b∈RangeF/RangeG -- (1) per definition of '/', "b ∈ Range F" and "¬ b ∈ Range G"
    ⟨ a , a,b∈F ⟩ = b∈RangeF                   -- (2) per definition of 'Range', there exists 'a' such that "(a,b) ∈ F"
    ¬a,b∈G =                                   -- (3) by contradiction, we will prove that "¬ (a, b) ∈ G" given (1)
            λ{ a,b∈G →                           -- (a) suppose that there exists 'a₁' such that "(a₁, b) ∈ G"
            let b∈RangeG = ⟨ _ , a,b∈G ⟩         -- (b) per definition of Range and given (a), "b ∈ Range G"
            in ¬b∈RangeG b∈RangeG }              -- (c) we know from (1) that "¬ b ∈ Range G" but (b) says "b ∈ Range G". Nonesense!
    a,b∈F/G = ⟨ a,b∈F , ¬a,b∈G ⟩               -- (4) per definition of '/' and given (2) and (3), then "(a, b) ∈ F / G"
    b∈RangeF/G = ⟨ a , a,b∈F/G ⟩               -- (5) per definition of 'Range' and given 'a' and (4), we know that "b ∈ Range (F / G)". QED
  in b∈RangeF/G

-- Class exercise
-- This is not really an exercise at all because this is the definition of `⊆`!
exercise103a : ∀ {a} {A B : Subset a}
             → (∀ x → x ∈ A → x ∈ B)
             → A ⊆ B
exercise103a x→x∈A→x∈B = x→x∈A→x∈B

open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; cong)

exercise103id→ : ∀ {a} {A B : Subset a}
              → A ≡ B → A ⊆ B × B ⊆ A
exercise103id→ refl = ⟨ (λ{_ p → p}) , (λ{_ p → p}) ⟩

--exercise103id← : ∀ {a} {A B : Subset a}
--              → A ⊆ B × B ⊆ A → A ≡ B
--exercise103id← ⟨ A⊆B , B⊆A ⟩ = ? -- Probably impossible to prove without unification. Possibly, impossible to prove. Needs `postulate`

exercise108a : ∀ {a} {A B : Subset a} (z : a)
             → (z ∈ A) × (z ∈ B) → z ∈ (A ∩ B)
exercise108a _ ⟨ z∈A , z∈B ⟩ = ⟨ z∈A , z∈B ⟩ -- How to explain this?
