module plfa-exercises.Practice2 where

-- Trying exercises:
-- 5.2 pp 340
-- 5.7 pp 386
-- 6.1 pp 423

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; subst)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Relation.Nullary using (¬_)
open import Data.Product using (_×_; proj₁; ∃-syntax) renaming (_,_ to ⟨_,_⟩)
open import Data.Sum using (_⊎_; inj₁; inj₂)
--open import plfa.part1.Isomorphism using (_≃_; extensionality)

∀-elim : ∀ {A : Set} {B : A → Set} → (L : ∀ (x : A) → B x) → (M : A) → B M
∀-elim l m = l m

--totype : ℕ → Set
--totype 0 = ℕ
--totype 1 = 4 ≡ 2 + 2
--totype _ = ℕ

-- λ noidea → ∀-elim {ℕ} {totype} noidea 4

------- Preliminary proofs -------
modus-tollens : ∀ {A B : Set}
  → (A → B)
  -------------
  → (¬ B → ¬ A)
modus-tollens a→b = λ{¬b → λ{a → ¬b (a→b a)}}
----------------------------------

postulate
  dne : ∀ {A : Set} → ¬ ¬ A → A

--data Σ (A : Set) (B : A → Set) : Set where
--  ⟨_,_⟩ : (x : A) → B x → Σ A B
--
--Σ-syntax = Σ
--infix 2 Σ-syntax
--syntax Σ-syntax A (λ x → B) = Σ[ x ∈ A ] B
--
--∃ : ∀ {A : Set} (B : A → Set) → Set
--∃ {A} B = Σ A B
--
--∃-syntax = ∃
--syntax ∃-syntax (λ x → B) = ∃[ x ] B

--⟨,⟩-syntax : ∀ {A : Set} {B : A → Set} (x : A) → B x → Σ A B
--⟨,⟩-syntax = ⟨_,_⟩
----⟨,⟩-syntax = Σ.⟨_,_⟩
--syntax ⟨,⟩-syntax x p = the-proof-for x is p

--∃-elim : ∀ {A : Set} {B : A → Set} {C : Set}
--  → (∀ x → B x → C)
--  → ∃[ x ] B x
--    ---------------
--  → C
--∃-elim f ⟨ x , y ⟩ = f x y

record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A


---------------------- Athena book exercises ----------------------

exercise531 : ∀ {A : Set} {R : A → A → Set}
            → (∀ (x y : A) → R x y)
            → (∀ (x : A) → R x x)
exercise531 R x = R x x

exercise532 : ∀ {A : Set} (x : A) → ∃[ y ] (x ≡ y)
exercise532 e = ⟨ e , refl ⟩

exercise533 : ∀ {A : Set} {P Q S : A → Set}
            → (∀ {x} → (P x ⊎ Q x) → S x)
            → (∃[ y ] (Q y))
            -------------------------
            → (∃[ y ] (S y))
exercise533 Px⊎Qx→Sx ⟨ y , qy ⟩ = 
  let
    -- P x ⊎ Q x
    py⊎qy = inj₂ qy
  in ⟨ y , Px⊎Qx→Sx py⊎qy ⟩

exercise534 : ∀ {A : Set} {P Q S : A → Set}
            → (∃[ y ] (P y × Q y))
            → (∀ {y} → P y → S y)
            -------------------------
            → (∃[ y ] (S y × Q y))
exercise534 ⟨ y , ⟨ py , qy ⟩ ⟩ py→sy = ⟨ y , ⟨ py→sy py , qy ⟩ ⟩

exercise535 : ∀ {A : Set} {P Q : A → Set}
            → (¬ ∃[ x ] (Q x))
            → (∀ {x} → P x → Q x)
            -------------------------
            → (¬ ∃[ x ] (P x))
exercise535 ¬∃x-qx ∀x→px→qx = λ{ ∃x-px@(⟨ x , px ⟩) → ¬∃x-qx ⟨ x , ∀x→px→qx px ⟩ }

exercise536 : ∀ {A : Set} {P Q S : A → Set}
            → (∀ {y} → P y → Q y)
            → (∃[ y ] (S y × ¬ Q y))
            ------------------------
            → (∃[ y ] (S y × ¬ P y))
exercise536 ∀y→py→qy ⟨ y , ⟨ sy , ¬qy ⟩ ⟩ = ⟨ y , ⟨ sy , modus-tollens ∀y→py→qy ¬qy ⟩ ⟩

exercise537 : ∀ {A : Set} {P Q : A → Set} {R : A → A → Set}
            → (∀ {x} → R x x → P x)
            → (∃[ x ] (P x) → ¬ ∃[ x ] (Q x))
            -------------------------------
            → ((∀ {x} → Q x) → ¬ ∃[ x ] (R x x))
exercise537 ∀x→rxx→px ∃x-px→¬∃x-qx ∀x→qx = λ{∃x-rxx@(⟨ x , rxx ⟩) → ∃x-px→¬∃x-qx ⟨ x , ∀x→rxx→px rxx ⟩ ⟨ x , ∀x→qx {x} ⟩ }

exercise538 : ∀ {A : Set} {P Q : A → Set}
            → (∃[ x ] (P x ⊎ Q x)) ⇔ (∃[ x ] (P x) ⊎ ∃[ x ] (Q x))
exercise538 = record { to = to ; from = from }
  where
  to : ∀ {A : Set} {P Q : A → Set}
     → ∃[ x ] (P x ⊎ Q x)
     -----------------------------
     → ∃[ x ] (P x) ⊎ ∃[ x ] (Q x)
  to ⟨ x , (inj₁ px) ⟩ = inj₁ ⟨ x , px ⟩
  to ⟨ x , (inj₂ qx) ⟩ = inj₂ ⟨ x , qx ⟩

  from : ∀ {A : Set} {P Q : A → Set}
       → ∃[ x ] (P x) ⊎ ∃[ x ] (Q x)
       -----------------------------
       → ∃[ x ] (P x ⊎ Q x)
  from (inj₁ ⟨ x , px ⟩) = ⟨ x , inj₁ px ⟩
  from (inj₂ ⟨ x , qx ⟩) = ⟨ x , inj₂ qx ⟩

------------------

exercise571 : ∀ {A : Set} {P Q : A → Set}
            → (∀ {x} → P x ⇔ Q x)
            → (∀ {x} → P x) ⇔ (∀ {x} → Q x)
exercise571 px⇔qx =
  record
  { to   = λ{px → (_⇔_.to   px⇔qx) px}
  ; from = λ{qx → (_⇔_.from px⇔qx) qx}
  }
--exercise571 ∀x→px⇔qx =
--  record
--  { to   = λ{∀x→px →  (_⇔_.to   (∀x→px⇔qx {x})) (∀x→px {x})}
--  ; from = λ{∀x→qx →  (_⇔_.from (∀x→px⇔qx {x})) (∀x→qx {x})}
--  }

exercise572 : ∀ {A : Set} {B : Set} {Q S : B → Set} {R T : B → B → Set}
            → (∃[ y ] (R y y × A))
            → (∃[ y ] (Q y × T y y))
            → (∀ y → A × Q y → ¬ S y)
            → (∃[ y ] (¬ S y × T y y))
exercise572 ⟨ _ , ⟨ _ , a ⟩ ⟩ ⟨ y , ⟨ qy , tyy ⟩ ⟩ ∀y→a×qy→¬sy = ⟨ y , ⟨ ∀y→a×qy→¬sy y ⟨ a , qy ⟩ , tyy ⟩ ⟩

-- This is fucking false!!!
--postulate
--  existence : {A : Set} {P : A → Set}
--            → (∀ x → P x)
--            ----------------
--            → (∃[ x ] (P x))

-- This cannot be proved! Take the empty set as an example. For any function
-- and relation the ∀'s are trivially true, but there is no element that
-- actually fulfills the function or the relation
--exercise574 : ∀ {A : Set} {F : A → A} {R : A → A → Set}
--            → (∀ x → R x x)
--            → (∀ x → F x ≡ F (F x))
--            -----------------------
--            → (∃[ y ] (R y (F y)))
--exercise574 ∀x→rxx ∀x→fx≡ffx = ⟨ ? , ? ⟩

exercise574 : ∀ {F : ℕ → ℕ} {R : ℕ → ℕ → Set}
            → (∀ x → R x x)
            → (∀ x → F x ≡ F (F x))
            -----------------------
            → (∃[ y ] (R y (F y)))
exercise574 {f} {r} ∀x→rxx ∀x→fx≡ffx =
  let y    = f 0
      y≡fy = ∀x→fx≡ffx 0          -- F 0 ≡ F (F 0)     =>  y ≡ F y
      ryy  = ∀x→rxx y             -- R (F 0) (F 0)     =>  R y y
      ryfy = subst (r y) y≡fy ryy -- R (F 0) (F (F 0)) =>  R y (F y)
  in ⟨ y , ryfy ⟩

------------------

exercise61a : ∀ {A B : Set}
            → (B ⊎ (A → B))
            → A
            ---------------
            → B
exercise61a (inj₁ b)   _ = b
exercise61a (inj₂ a→b) a = a→b a

exercise61b : ∀ {A B C : Set}
            → (¬ B → ¬ C)
            → ((A × B) ⊎ ¬ ¬ C)
            -------------------
            → B
exercise61b ¬b→¬c (inj₁ ⟨ a , b ⟩) = b
exercise61b ¬b→¬c (inj₂ ¬¬c) = dne ((modus-tollens ¬b→¬c) ¬¬c)

¬¬ : ∀ {A : Set}
   → A
   → ¬ ¬ A
¬¬ a = λ{¬a → ¬a a}

-- What is the difference between: ∀ x → P x → ∀ y → R y → L  and  (∀ x → P x) → (∀ y → R y) → L

---- Can the following be proved without Double Negation Elimination (dne)?
--lemma₁ : ∀ {A B : Set}
--       → (¬ B → ¬ A)
--       → A
--       -------------------
--       → B
--lemma₁ = ?
--
--lemma₂ : ∀ {A B : Set}
--       → (¬ B → ¬ A)
--       → ¬ ¬ A
--       -------------------
--       → B
--lemma₂ = ?
--
---- `lemma₁` seems not to imply `dne` but it can't be proven without it. 
----
---- The answer is NO (for the second one)! Because from it we can prove `dne`!
---- `dne` cannot be proved in vanila Agda.
--dne₁ : ∀ {A : Set} → ¬ ¬ A → A
--dne₁ ¬¬a = lemma₁ ? ?
--
--dne₂ : ∀ {A : Set} → ¬ ¬ A → A
--dne₂ ¬¬a = lemma₂ (λ{x → x}) ¬¬a
