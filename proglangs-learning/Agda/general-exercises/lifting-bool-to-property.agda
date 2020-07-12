open import Data.Bool using (Bool; true; false; _∧_; _∨_; T)
open import Data.Bool.Properties using (∨-identityˡ; ∨-zeroˡ)
open import Data.List using (List; []; _∷_; any; map; foldl)
open import Data.List.Relation.Unary.Any using (Any; here; there; lookup; satisfied)
open import Data.Product as Prod using (∃; _,_; proj₁; proj₂)
open import Data.Unit using (⊤; tt)
open import Relation.Binary.PropositionalEquality 
   using (_≡_; refl; cong; subst; sym; trans; module ≡-Reasoning)
open import Relation.Binary.PropositionalEquality as P
open ≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_; _∎)
open import Data.Nat using (ℕ; zero; suc)

≡→T : ∀ {b : Bool} → b ≡ true → T b
≡→T refl = tt

any-val : ∀ {a} {A : Set a} (f) (ns : List A)
        → any f ns ≡ true
        → Any (λ x → T (f x)) ns
any-val f (n ∷ ns) any-f-⟨n∷ns⟩≡true with f n | P.inspect f n
... | true  | P.[ fn≡t ] = here (≡→T fn≡t)
... | false | _          = there (any-val f ns any-f-⟨n∷ns⟩≡true)

get-val : ∀ {a} {A : Set a} {f} (ns : List A)
        → any f ns ≡ true
        → A
get-val {_} {_} {f} ns any≡true = lookup {xs = ns} (any-val f ns any≡true)

get-prop : ∀ {a} {A : Set a} {f} (ns : List A)
         → any f ns ≡ true
         → ∃ (λ x → T (f x))
get-prop {_} {_} {f} ns any≡true = satisfied {xs = ns} (any-val f ns any≡true)

is-not-zero : ℕ → Bool
is-not-zero zero = false
is-not-zero _ = true

eg1 : any is-not-zero (0 ∷ 0 ∷ 3 ∷ []) ≡ true
eg1 = refl
--proj₁ (get-prop {f = λ x → x} (true ∷ false ∷ []) _)
--proj₁ (get-prop {f = is-not-zero} (0 ∷ 0 ∷ 3 ∷ []) refl)


--data Singleton {a} {A : Set a} (x : A) : Set a where
--  _with≡_ : (y : A) → x ≡ y → Singleton x
--
--inspect : ∀ {a} {A : Set a} (x : A) → Singleton x
--inspect x = x with≡ refl


any-val' : ∀ {a} {A : Set a} (f : A → Bool) (ns : List A)
         → any f ns ≡ true
         → (∃ λ x → T (f x))
any-val' f [] ()
any-val' f (n ∷ ns) anyfnns≡true with f n | P.inspect f n
...   | true  | P.[ fn≡t ] = n , ≡→T fn≡t
...   | false | P.[ fn≡f ] = any-val' f ns anyfnns≡true
--  where
--    anyfns≡true =
--      begin
--        any f ns            ≡⟨ sym (∨-identityˡ (any f ns)) ⟩
--        false ∨ any f ns    ≡⟨ cong (_∨ any f ns) (sym ?) ⟩
--        f n ∨ any f ns      ≡⟨ anyfnns≡true ⟩
--        true
--      ∎
