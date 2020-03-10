{-# OPTIONS --without-K --safe #-}

module sv20.assign1.Second where

{- Code partially taken from the book "Programming Language Foundations in
 - Agda" by Philip Wadler, Wen Kokke, Jeremy Siek and many others. The book
 - can be found at https://plfa.github.io/
 -
 - Based on chapters 6 and 7 - Connectives and Negation
 -  https://plfa.github.io/Connectives/
 -  https://plfa.github.io/Negation/
 -}

open import Data.Product using (_×_; proj₁; proj₂) renaming (_,_ to ⟨_,_⟩)
open import Data.Sum using (_⊎_; inj₁; inj₂; [_,_])
open import Relation.Nullary using (¬_)

-- Logic can be formalised as types [1], so the current homework solution uses
-- types and type operations to proof logic operations.
--
-- [1] “Propositions as Types”, Philip Wadler, Communications of the ACM, December 2015.

------------------------------------------------------------------------------------------
--                                   HOMEWORK SOLUTION                                  --
------------------------------------------------------------------------------------------

-- Conjuction is formalised in type systems as product

-- Prove that:   A ⇒ B ⇒ C   from    A ∧ B ⇒ C
proof₁ : ∀ {A B C : Set}
  → (A × B → C)
  -------------
  → (A → B → C)
proof₁ f a b = f ⟨ a , b ⟩

-- Prove that:   (A ⇒ ¬ E ⇒ ¬ C)    from    ((A ∨ B) ⇒ (C ∨ D) ⇒ E)

-- Preliminary proofs
modus-tollens : ∀ {A B : Set}
  → (A → B)
  -------------
  → (¬ B → ¬ A)
modus-tollens a→b = λ{¬b → λ{a → ¬b (a→b a)}}

-- Disjunction is formalised as Union
lemma₁ : ∀ {A B E : Set}
  → (A ⊎ B → E)
  -------------
  → (A → E)
lemma₁ a⊎b→e a = a⊎b→e (inj₁ a)

lemma₂ : ∀ {C D : Set}
  → ¬ (C ⊎ D)
  -------------
  → ¬ C
lemma₂ c⊎d = λ{c → c⊎d (inj₁ c)}


---- And finally the proof
proof₂ : ∀ {A B C D E : Set}
  → (A ⊎ B → C ⊎ D → E)
  ---------------------
  → (A → ¬ E → ¬ C)

proof₂ a⊎b→c⊎d→e a ¬e =
  let
     --   (A ⊎ B → C ⊎ D → E)
     -- → A
     -- ---------------------
     -- → (C ⊎ D → E)
    c⊎d→e = lemma₁ a⊎b→c⊎d→e a

     --   (C ⊎ D → E)
     -- → ¬E
     -- -------------
     -- → ¬ (C ⊎ D)
    ¬c⊎d  = modus-tollens c⊎d→e ¬e

  in
     --   ¬ (C ⊎ D)
     -- -------------
     -- → ¬ C
    lemma₂ ¬c⊎d

-------- Other stuff
stuff₁ : ∀ {C D : Set}
  → ¬ (C ⊎ D)
  -------------
  → (¬ C) × (¬ D)
stuff₁ c⊎d = ⟨ (λ c → c⊎d (inj₁ c)) , (λ d → c⊎d (inj₂ d)) ⟩
