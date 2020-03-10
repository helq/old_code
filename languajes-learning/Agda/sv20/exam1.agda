module sv20.exam1 where

open import Data.Product using (∃-syntax) renaming (_,_ to ⟨_,_⟩)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Function.Base using (_∘_)

record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A

-- An idiom that makes things slightly more readable (sometimes)
case_of_ : ∀ {a b} {A : Set a} {B : Set b} → A → (A → B) → B
case x of f = f x

-- Exam starts from here:

-- 1.

first : ∀ {Object : Set} {P Q : Object → Set}
      → (∃[ x ] (P x ⊎ Q x)) ⇔ (∃[ x ] (P x) ⊎ ∃[ x ] (Q x))
-- We prove each side separately of the bijection separately. If we are able to
-- prove both ways, we will have proved the bijection
first =
    record { to = to ; from = from }
  where
  -- First, we prove that (∃[ x ] (P x ⊎ Q x)) → (∃[ x ] (P x) ⊎ ∃[ x ] (Q x))
  to : ∀ {Object : Set} {P Q : Object → Set}
     → (∃[ x ] (P x ⊎ Q x)) → (∃[ x ] (P x) ⊎ ∃[ x ] (Q x))
  -- Proof
  --to ⟨ x , inj₁ px ⟩ = inj₁ ⟨ x , px ⟩
  --to ⟨ x , inj₂ qx ⟩ = inj₂ ⟨ x , qx ⟩
  --
  -- Proof explained:
  to ∃x-px⊎qx =
    let
      ⟨ x , px⊎qx ⟩ = ∃x-px⊎qx  -- From ∃[ x ] (P x ⊎ Q x) we find an element x that fullfills (P x ⊎ Q x)
    in case px⊎qx of            -- P x ⊎ Q x  indicates two possible cases, either
      λ { (inj₁ px) → inj₁ ⟨ x , px ⟩  -- P x  is true, and so ∃[ x ] (P x)
        ; (inj₂ qx) → inj₂ ⟨ x , qx ⟩  -- or, Q x is true, and so ∃[ x ] (Q x)
      }

  -- Now, we prove the inverse (∃[ x ] (P x) ⊎ ∃[ x ] (Q x)) → (∃[ x ] (P x ⊎ Q x))
  from : ∀ {Object : Set} {P Q : Object → Set}
       → (∃[ x ] (P x) ⊎ ∃[ x ] (Q x)) → (∃[ x ] (P x ⊎ Q x))
  -- Proof
  --from (inj₁ ⟨ x , px ⟩) = ⟨ x , inj₁ px ⟩
  --from (inj₂ ⟨ x , qx ⟩) = ⟨ x , inj₂ qx ⟩
  --
  -- Proof explained:
  -- There are two possible properties that get fulfilled, either
  from (inj₁ ⟨ x , px ⟩) = ⟨ x , inj₁ px ⟩  -- ∃[ x ] (P x) gets fulfilled
  from (inj₂ ⟨ x , qx ⟩) = ⟨ x , inj₂ qx ⟩  -- or, ∃[ x ] (Q x) gets fulfilled

-- 2.

second : ∀ {Object : Set} {P Q : Object → Set} {_R_ : Object → Object → Set}
       → ∃[ x ] (P x)
       → ∃[ x ] (Q x)
       → (∀ x → P x → ∀ y → Q y → x R y)
       ---------------------------------
       → ∃[ x ] ∃[ y ] (x R y)
--Proof:
--second ⟨ x₁ , px₁ ⟩ ⟨ x₂ , qx₂ ⟩ ∀x→px→∀y→qy→xry = ⟨ x₁ , ⟨ x₂ , ∀x→px→∀y→qy→xry x₁ px₁ x₂ qx₂ ⟩ ⟩
--
--Proof explained:
second ∃x-px ∃x-qx ∀x→px→∀y→qy→xry =
  let
    ⟨ x₁ , px₁ ⟩ = ∃x-px  -- If we have a value from ∃[ x ] (P x), we can get an x₁ that fulfills P
    ⟨ x₂ , qx₂ ⟩ = ∃x-qx  -- and there is a value x₂ that fulfills Q
    px→∀y→qy→xry = ∀x→px→∀y→qy→xry x₁   -- For all x it is true that (P x → ∀ y → Q y → x R y), including x₁
    ∀y→qy→xry    = px→∀y→qy→xry px₁     -- P is fulfilled by x₁
    xry          = ∀y→qy→xry x₂ qx₂     -- In a similar way, x₂ is a value that fulfills Qy, so we can instanciate (∀ y → Q y → x₁ R y) into (x₁ R x₂)
  in ⟨ x₁ , ⟨ x₂ , xry ⟩ ⟩ -- So, we know there is an element x₁ and an element x₂ for which x₁ R x₂ is true
