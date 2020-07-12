module sv20.assign2.Second where

open import sv20.assign2.SetTheory.Logic   using (_⇔_; ¬_; _∧_; _∨_) renaming (_,_ to ⟨_,_⟩)
open import sv20.assign2.SetTheory.ZAxioms using (𝓢; _⊆_; _∈_; _∉_; _≡_; _⊂_; sym; cong; subs; trans;
                                                  proj₁; proj₂;
                                                  empt; ext; union; pair; pow; sub; pem; sum)
open import sv20.assign2.SetTheory.Algebra using (_∪_; _-_; _∩_)

∪-dist : (A B C : 𝓢) → A ∪ (B ∪ C) ≡ (A ∪ B) ∪ C
∪-dist = ?

-- I could try going this route. This would prove certain things using
-- ZFC and would be really neat but requires too much work.
