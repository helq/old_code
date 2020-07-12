module plfa-exercises.part1.Decidable where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; sym; cong)
open Eq.≡-Reasoning
open import Data.Nat using (ℕ; zero; suc; pred)
open import Data.Product using (_×_) renaming (_,_ to ⟨_,_⟩)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Relation.Nullary using (¬_)
open import Relation.Nullary.Negation using ()
  renaming (contradiction to ¬¬-intro)
open import Data.Unit using (⊤; tt)
open import Data.Empty using (⊥; ⊥-elim)
open import plfa.part1.Relations using (_<_; z<s; s<s)
open import plfa.part1.Isomorphism using (_⇔_)
open import Function.Base using (_∘_)

infix 4 _≤_

data _≤_ : ℕ → ℕ → Set where

  z≤n : ∀ {n : ℕ}
      --------
    → zero ≤ n

  s≤s : ∀ {m n : ℕ}
    → m ≤ n
      -------------
    → suc m ≤ suc n
    

_ : 2 ≤ 4
_ = s≤s (s≤s z≤n)

¬4≤2 : ¬ (4 ≤ 2)
--¬4≤2 (s≤s (s≤s ())) = ? -- This causes Agda to die! TODO: Report
¬4≤2 (s≤s (s≤s ()))

data Bool : Set where
  true  : Bool
  false : Bool

infix 4 _≤ᵇ_

_≤ᵇ_ : ℕ → ℕ → Bool
zero ≤ᵇ n       =  true
suc m ≤ᵇ zero   =  false
suc m ≤ᵇ suc n  =  m ≤ᵇ n

_ : (2 ≤ᵇ 4) ≡ true
_ = refl
_ : (5 ≤ᵇ 4) ≡ false
_ = refl

T : Bool → Set
T true = ⊤
T false = ⊥

≤→≤ᵇ : ∀ {m n} → (m ≤ n) → T (m ≤ᵇ n)
≤→≤ᵇ z≤n = tt
≤→≤ᵇ (s≤s m≤n) = ≤→≤ᵇ m≤n

≤ᵇ→≤ : ∀ {m n} → T (m ≤ᵇ n) → (m ≤ n)
≤ᵇ→≤ {zero} {_} tt = z≤n
-- What is going on in here? Left `t` has type `T (suc m ≤ᵇ suc n)` and the right `t` has type `T (m ≤ᵇ n)`
-- λ m n → T (suc m ≤ᵇ suc n)   => reduces to:  λ m n → T (m ≤ᵇ n)
-- Ohh! Ok. It is because the third rule of `≤ᵇ` reduces `suc m ≤ᵇ suc n` to `m ≤ᵇ n`
≤ᵇ→≤ {suc m} {suc n} t = s≤s (≤ᵇ→≤ {m} {n} t)
-- λ m → T (suc m ≤ᵇ zero)   => reduces to:  λ m → ⊥
-- which means that there are no rules for `fromᵇ {suc m} {zero}`
≤ᵇ→≤ {suc m} {zero} ()

proof≡computation : ∀ {m n} → (m ≤ n) ⇔ T (m ≤ᵇ n)
proof≡computation {m} {n} =
  record
    { from = ≤ᵇ→≤
    ; to = ≤→≤ᵇ
    }

T→≡ : ∀ (b : Bool) → T b → b ≡ true
T→≡ true tt = refl
T→≡ false ()

--postulate
--  lie : false ≡ true

≡→T : ∀ {b : Bool} → b ≡ true → T b
--≡→T {false} refl = ? -- This is impossible because of refl's definition. Unification forces `b` to be `true`
--≡→T {false} rewrite lie = λ refl → ? -- Even postulating a lie, it is impossible to create a bottom value
≡→T refl = tt

_ : 2 ≤ 4
_ = ≤ᵇ→≤ tt

¬4≤2₂ : ¬ (4 ≤ 2)
--¬4≤2₂ 4≤2 = ≤→≤ᵇ 4≤2
--¬4≤2₂ = ≤→≤ᵇ {4} {2}
-- The type of `T (4 ≤ᵇ 2)` which reduces to `T false` and then `⊥`
¬4≤2₂ = ≤→≤ᵇ
-- Notice how defining ≤ᵇ lifts from us the demand of computing the correct
-- `evidence` (implementation) for the `proof` (function type)


data Dec (A : Set) : Set where
  yes :   A → Dec A
  no  : ¬ A → Dec A

¬s≤z : {m : ℕ} → ¬ (suc m) ≤ zero
--¬s≤z = ≤→≤ᵇ
¬s≤z ()

¬s≤s : {m n : ℕ} → ¬ m ≤ n → ¬ suc m ≤ suc n
¬s≤s ¬m≤n = λ { (s≤s m≤n) → ¬m≤n m≤n }

_≤?_ : (m n : ℕ) → Dec (m ≤ n)
zero    ≤? n       = yes z≤n
(suc m) ≤? zero    = no ¬s≤z
(suc m) ≤? (suc n) with m ≤? n
...                   | yes  m≤n = yes (s≤s m≤n)
...                   | no  ¬m≤n = no  (¬s≤s ¬m≤n)

-- `2 ≤? 4` reduces to `yes (s≤s (s≤s z≤n))`
_ : Dec (2 ≤ 4)
_ = 2 ≤? 4
_ = yes (s≤s (s≤s (z≤n {2})))

⌊_⌋ : ∀ {A : Set} → Dec A → Bool
⌊ yes x ⌋  =  true
⌊ no ¬x ⌋  =  false

_ : Bool
_ = true
_ = ⌊ 3 ≤? 4 ⌋

_ : ⌊ 3 ≤? 4 ⌋ ≡ true
_ = refl

_ : ⌊ 3 ≤? 2 ⌋ ≡ false
_ = refl

toWitness : ∀ {A : Set} {D : Dec A} → T ⌊ D ⌋ → A
toWitness {_} {yes v} tt = v
--toWitness {_} {no _} = ? -- `T ⌊ no x ⌋ → A` reduces to `⊥ → A`
toWitness {_} {no _} () -- Empty because there is no value for `⊥`

fromWitness : ∀ {A : Set} {D : Dec A} → A → T ⌊ D ⌋
fromWitness {_} {yes _} _ = tt
fromWitness {_} {no ¬a} a = ¬a a -- with type ⊥

_ : 2 ≤ 4
--_ = toWitness {D = 2 ≤? 4} tt
_ = toWitness {_} {2 ≤? 4} tt

¬4≤2₃ : ¬ (4 ≤ 2)
--¬4≤2₃ = fromWitness {D = 4 ≤? 2}
¬4≤2₃ = fromWitness {_} {4 ≤? 2}

¬z<z : ¬ (zero < zero)
¬z<z ()

¬s<z : ∀ {m : ℕ} → ¬ (suc m < zero)
¬s<z ()

_<?_ : ∀ (m n : ℕ) → Dec (m < n)
zero <? zero = no ¬z<z
zero <? suc n = yes z<s
suc m <? zero = no ¬s<z
suc m <? suc n with m <? n
...               | yes m<n = yes (s<s m<n)
...               | no ¬m<n = no λ{(s<s m<n) → ¬m<n m<n}

_ : ⌊ 2 <? 4 ⌋ ≡ true
_ = refl

¬z≡sn : ∀ {n : ℕ} → ¬ zero ≡ suc n
¬z≡sn ()

_≡ℕ?_ : ∀ (m n : ℕ) → Dec (m ≡ n)
zero    ≡ℕ? zero    = yes refl
zero    ≡ℕ? (suc n) = no ¬z≡sn
--(suc m) ≡ℕ? zero    = no (λ{sn≡z → ¬z≡sn (sym sn≡z)})
--(suc m) ≡ℕ? zero    = no (¬z≡sn ∘ sym)
(suc m) ≡ℕ? zero    = no ((λ()) ∘ sym)
--The following doesn't work though
--(suc m) ≡ℕ? zero    with zero ≡ℕ? (suc m)
--...                 | yes  z≡sm = yes (sym z≡sm)
--...                 | no  ¬z≡sm = no (¬z≡sm ∘ sym)
(suc m) ≡ℕ? (suc n) with m ≡ℕ? n
...               | yes m≡n = yes (cong suc m≡n)
...               | no ¬m≡n = no  (¬m≡n ∘ (cong pred))


infixr 6 _×-dec_

_×-dec_ : ∀ {A B : Set} → Dec A → Dec B → Dec (A × B)
yes x ×-dec yes y = yes ⟨ x , y ⟩
no ¬x ×-dec _     = no λ{ ⟨ x , y ⟩ → ¬x x }
_     ×-dec no ¬y = no λ{ ⟨ x , y ⟩ → ¬y y }

infixr 6 _⊎-dec_

_⊎-dec_ : ∀ {A B : Set} → Dec A → Dec B → Dec (A ⊎ B)
yes x ⊎-dec _     = yes (inj₁ x)
_     ⊎-dec yes y = yes (inj₂ y)
no ¬x ⊎-dec no ¬y = no λ{(inj₁ x) → ¬x x; (inj₂ y) → ¬y y}

¬? : ∀ {A : Set} → Dec A → Dec (¬ A)
¬? (yes x) = no (¬¬-intro x)
¬? (no ¬x) = yes ¬x

_→-dec_ : ∀ {A B : Set} → Dec A → Dec B → Dec (A → B)
_     →-dec yes y = yes (λ _ → y)
--no ¬x →-dec _     = yes ((λ()) ∘ ¬x)
no ¬x →-dec _     = yes (⊥-elim ∘ ¬x)
yes x →-dec no ¬y = no (λ{x→y → ¬y (x→y x)})


infixr 6 _∧_

_∧_ : Bool → Bool → Bool
true  ∧ true  = true
false ∧ _     = false
_     ∧ false = false

∧-× : ∀ {A B : Set} (x : Dec A) (y : Dec B) → ⌊ x ⌋ ∧ ⌊ y ⌋ ≡ ⌊ x ×-dec y ⌋
∧-× (yes x) (yes y) = refl
∧-× (no ¬x) _       = refl
∧-× (yes x) (no ¬y) = refl

_iff_ : Bool → Bool → Bool
true  iff true  = true
false iff false = true
_     iff _     = false

_⇔-dec_ : ∀ {A B : Set} → Dec A → Dec B → Dec (A ⇔ B)
yes x ⇔-dec yes y = yes (record { from = λ{_ → x} ; to = λ{_ → y} })
no ¬x ⇔-dec no ¬y = yes (record { from = (λ()) ∘ ¬y ; to = (λ()) ∘ ¬x })
yes x ⇔-dec no ¬y = no (λ x⇔y → ¬y (_⇔_.to   x⇔y x))
no ¬x ⇔-dec yes y = no (λ x⇔y → ¬x (_⇔_.from x⇔y y))

iff-⇔ : ∀ {A B : Set} (x : Dec A) (y : Dec B) → ⌊ x ⌋ iff ⌊ y ⌋ ≡ ⌊ x ⇔-dec y ⌋
iff-⇔ (yes x) (yes y) = refl
iff-⇔ (no ¬x) (no ¬y) = refl
iff-⇔ (no ¬x) (yes y) = refl
iff-⇔ (yes x) (no ¬y) = refl
