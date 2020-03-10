module plfa-exercises.Practice where

--------------------------------------- Naturals ---------------------------------------

-- Inductive definition of Numbers (new datatype)
data ℕ : Set where
-- Judgements (two in total for this case)
  zero : ℕ       -- No hypothesis and one conclusion
  suc  : ℕ → ℕ   -- One hypothesis and one conclusion

seven : ℕ
seven = suc (suc (suc (suc (suc (suc (suc zero))))))
--seven′ = --7

---

-- Gives us the power of writing 3 to signify suc (suc (suc zero)) :)
{-# BUILTIN NATURAL ℕ #-}

import Relation.Binary.PropositionalEquality as Eq
open import Function.Base using (flip)
open Eq using (_≡_; _≢_; refl; cong; sym; trans)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_; _∎)
open import Relation.Nullary using (¬_)

_+_ : ℕ → ℕ → ℕ
zero + n = n                -- +-def₀
(suc m) + n = suc (m + n)   -- +-def₁

--_ : (suc (suc zero)) + (suc (suc (suc zero))) ≡ (suc (suc (suc (suc (suc zero)))))
--_ =
--  begin
--    (suc (suc zero)) + (suc (suc (suc zero)))
--  ≡⟨⟩    -- inductive case
--    suc ((suc zero) + (suc (suc (suc zero))))
--  ≡⟨⟩    -- inductive case
--    suc (suc (zero + (suc (suc (suc zero)))))
--  ≡⟨⟩    -- base case
--    suc (suc (suc (suc (suc zero))))
--  ∎
--
--_ : 2 + 3 ≡ 5
--_ =
--  begin
--    2 + 3
--    (suc 1) + 3
--  ≡⟨⟩
--    suc (1 + 3)
--  ≡⟨⟩
--    suc (suc 0 + 3)
--  ≡⟨⟩
--    suc (suc (0 + 3))
--  ≡⟨⟩
--    suc (suc 3)
--  ≡⟨⟩
--    suc 4
--  ≡⟨⟩
--    5
--  ∎
--
--
--_ : (suc (suc zero)) + (suc (suc (suc zero))) ≡ (suc (suc (suc (suc (suc zero)))))
--_ = refl

_*_ : ℕ → ℕ → ℕ
zero    * n  =  zero
(suc m) * n  =  n + (m * n)

_^_ : ℕ → ℕ → ℕ
n ^ zero  =  suc zero
n ^ (suc m)  =  n * (n ^ m)

-- Monus
_∸_ : ℕ → ℕ → ℕ
m     ∸ zero   =  m
zero  ∸ suc n  =  zero
suc m ∸ suc n  =  m ∸ n

infixl 6  _+_  _∸_
infixl 7  _*_
infixr 8  _^_


-- Superfun binary numbers :D
data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

inc : Bin → Bin
inc ⟨⟩ = ⟨⟩ I
inc (b O) = b I
inc (b I) = (inc b) O

_ : inc (⟨⟩ I O I I) ≡ ⟨⟩ I I O O
_ = refl

toᵇ : ℕ → Bin
toᵇ zero = ⟨⟩ O
toᵇ (suc n) = inc (toᵇ n)

fromᵇ : Bin → ℕ
fromᵇ ⟨⟩ = zero
fromᵇ (b O) = let n = fromᵇ b in n + n
fromᵇ (b I) = let n = fromᵇ b in suc (n + n)

_ : toᵇ 11 ≡ (⟨⟩ I O I I)
_ = refl

_ : fromᵇ (inc (⟨⟩ I O I I)) ≡ 12
_ = refl
--_ = begin
--      (fromᵇ (⟨⟩ I I O O))
--    ≡⟨ 12 ≡⟨⟩ 12 ∎ ⟩ 
--      12
--    ∎

--_ : Set₉₁₁₁₁₁₁₁₁₁₁₁₁₁₁₁₁₁₁₁₁
--_ : Set₀
_ : Set
_ = suc 11 ≡ 12

_+ᵇ_ : Bin → Bin → Bin
⟨⟩    +ᵇ b     = b
b     +ᵇ ⟨⟩    = b
--(b O) +ᵇ ⟨⟩    = b O
(b O) +ᵇ (d O) = (b +ᵇ d) O
(b O) +ᵇ (d I) = (b +ᵇ d) I
--(b I) +ᵇ ⟨⟩    = b I
(b I) +ᵇ (d O) = (b +ᵇ d) I
(b I) +ᵇ (d I) = (inc (b +ᵇ d)) O

-- Proving the following is trivial
mod-left : ∀ {b : Bin} → ⟨⟩ +ᵇ b ≡ b
mod-left = refl
-- But not its complement. This is due to "case trees" (or how Agda implements
-- functions under the hood)
-- https://agda.readthedocs.io/en/v2.6.0.1/language/function-definitions.html#case-trees
mod-right : ∀ {b : Bin} → b +ᵇ ⟨⟩ ≡ b
mod-right {⟨⟩}  = refl
mod-right {b O} = refl
mod-right {b I} = refl
-- Also, I'm confused on the implications of improper "case trees". If the
-- second rule wasn't reachable, the following code would run even
-- if the rule was nonesense (eg, changing `b +ᵇ ⟨⟩ = b O O`) but it doesn't
-- work! The rule must make sense. So, Agda is applying the rule after all and
-- not ignoring it even thought it can't be reached directly in proofs
_ : (⟨⟩ I O I I I) +ᵇ (⟨⟩ O O I) ≡ ⟨⟩ I I O O O
_ = refl


---proppre : ∀ (n : ℕ) → zero + suc n ≡ suc (zero + n)
---proppre zero = refl
---proppre (suc n) =
---  begin
---    zero + suc (suc n)
---  ≡⟨⟩
---    zero + suc (zero + suc n)
---  ≡⟨⟩
---    suc (zero + suc n)
---  ∎
  --≡⟨ cong suc (proppre n) ⟩

-- Taken it from book
assoc-+ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
assoc-+ zero n p = refl
assoc-+ (suc m) n p rewrite assoc-+ m n p = refl

comm-+₀ : ∀ (m : ℕ) → m + zero ≡ m
comm-+₀ zero = refl
comm-+₀ (suc n) rewrite comm-+₀ n = refl
--comm-+₀ (suc n) =
--  begin
--    zero + suc n
--  ≡⟨⟩
--    zero + suc (zero + n)
--  ≡⟨⟩
--    suc (zero + n)
--  ≡⟨ cong suc (comm-+₀ n) ⟩
--    suc (n + zero)
--  ≡⟨⟩
--    suc n + zero
--  ∎

succ_right : ∀ (n m : ℕ) → suc (n + m) ≡ n + suc m
succ_right zero m = refl
succ_right (suc n) m rewrite succ_right n m = refl
--succ_right (suc n) m = cong suc (succ_right n m)
--succ_right (suc n) m =
--  begin
--    suc (suc n + m)
--  ≡⟨⟩
--    suc (suc (n + m))
--  ≡⟨ cong suc (succ_right n m) ⟩
--    suc (n + suc m)
--  ≡⟨⟩
--    suc n + suc m
--  ∎

comm-+ : ∀ (n m : ℕ) → n + m ≡ m + n
comm-+ zero n = sym (comm-+₀ n)
comm-+ (suc n) m rewrite comm-+ n m | succ_right m n = refl
--comm-+ (suc n) m = trans (cong suc (comm-+ n m)) (succ_right m n)
--comm-+ (suc n) m =
--  begin
--    suc n + m
--  ≡⟨⟩ -- +-def₁
--    suc (n + m)
--  ≡⟨ cong suc (comm-+ n m) ⟩
--    suc (m + n)
--  ≡⟨ succ_right m n ⟩
--    m + suc n
--  ∎

-- Try evaluating and type-checking the following expressions:
-- comm-+ zero
-- (flip comm-+) zero
-- λ n m → cong suc (comm-+ n m)
-- λ m n → succ_right m n
-- λ m n → sym (succ_right m n)
-- λ n m → trans (cong suc (comm-+ n m)) (succ_right m n)

monus : ∀ (n : ℕ) → zero ∸ n ≡ zero
monus zero = refl
monus (suc n) = refl
--monus : ∀ {n : ℕ} → zero ∸ n ≡ zero
--monus {zero} = refl
--monus {suc n} = refl

inc≡suc : ∀ (b : Bin) → fromᵇ (inc b) ≡ suc (fromᵇ b)
inc≡suc ⟨⟩ = refl
inc≡suc (b O) rewrite sym (comm-+₀ (fromᵇ b)) = refl
inc≡suc (b I)
  rewrite
    comm-+₀ (fromᵇ (inc b))
  | inc≡suc b
  | succ_right (fromᵇ b) (fromᵇ b)
  | comm-+₀ (fromᵇ b)
  | succ_right (fromᵇ b) (fromᵇ b) = refl

-- `toᵇ (fromᵇ b) ≡ b` doesn't hold for all values, just for some.
-- So the following is false
--tofromb≢b : ∀ (b : Bin) → ¬ (toᵇ (fromᵇ b) ≡ b)
--tofromb≢b ⟨⟩ = λ()
--tofromb≢b = ? -- impossible to prove

_ : ¬ (toᵇ (fromᵇ ⟨⟩) ≡ ⟨⟩)
_ = λ()

--from∘toᵇ₀ : ∀ (n : ℕ) → fromᵇ ((toᵇ n) O) ≡ fromᵇ (toᵇ n) + fromᵇ (toᵇ n)
--from∘toᵇ₀ zero = refl
--from∘toᵇ₀ (suc n) = refl
--
--from∘toᵇ₁ : ∀ (n : ℕ) → fromᵇ ((toᵇ n) I) ≡ suc (fromᵇ (toᵇ n) + fromᵇ (toᵇ n))
--from∘toᵇ₁ zero = refl
--from∘toᵇ₁ (suc n) = refl

monobin₀ : ∀ (b : Bin) → inc (inc (b +ᵇ b)) ≡ (inc b +ᵇ inc b)
monobin₀ ⟨⟩ = refl
monobin₀ (b O) = refl
monobin₀ (b I) rewrite monobin₀ b = refl

monobin : ∀ (n : ℕ) → toᵇ (n + n) ≡ (toᵇ n) +ᵇ (toᵇ n)
monobin zero = refl
monobin (suc n) rewrite
    sym (succ_right n n)
  | monobin n
  | monobin₀ (toᵇ n) = refl

--mononat₀ : ∀ (a : Bin) → a +ᵇ ⟨⟩ ≡ a
--mononat₀ ⟨⟩ = refl
--mononat₀ (b O) = refl
--mononat₀ (b I) = refl
--
--mononat₁ : ∀ (a b : Bin) → fromᵇ (inc (a +ᵇ b)) ≡ suc (fromᵇ a + fromᵇ b)
--mononat₁ ⟨⟩ b rewrite inc≡suc b = refl
--mononat₁ a ⟨⟩ rewrite mononat₀ a | comm-+₀ (fromᵇ a) | inc≡suc a = refl
--mononat₁ = ?
---- λ a → fromᵇ (inc (a +ᵇ ⟨⟩)) ≡ suc (fromᵇ a + fromᵇ ⟨⟩)
---- fromᵇ (inc b) ≡ suc (fromᵇ b)
--
--mononat : ∀ (a b : Bin) → fromᵇ (a +ᵇ b) ≡ fromᵇ a + fromᵇ b
--mononat ⟨⟩ _ = refl
--mononat a ⟨⟩ rewrite mononat₀ a | comm-+₀ (fromᵇ a) = refl
----mononat a ⟨⟩ = 
----  begin
----    fromᵇ (a +ᵇ ⟨⟩)
----    ≡⟨ cong fromᵇ (mononat₀ a) ⟩
----    fromᵇ (a)
----    ≡⟨ sym (comm-+₀ (fromᵇ a)) ⟩
----    fromᵇ a + zero
----    ≡⟨⟩
----    fromᵇ a + fromᵇ ⟨⟩
----  ∎
--mononat (a O) (b O) rewrite
--    mononat a b
--  | assoc-+ (fromᵇ a) (fromᵇ b) (fromᵇ a + fromᵇ b)
--  | comm-+ (fromᵇ b) (fromᵇ a + fromᵇ b)
--  | assoc-+ (fromᵇ a) (fromᵇ a) (fromᵇ b + fromᵇ b)
--  | assoc-+ (fromᵇ a) (fromᵇ b) (fromᵇ b)
--  = refl
--mononat (a I) (b O) rewrite
--    mononat a b
--  | assoc-+ (fromᵇ a) (fromᵇ b) (fromᵇ a + fromᵇ b)
--  | comm-+ (fromᵇ b) (fromᵇ a + fromᵇ b)
--  | assoc-+ (fromᵇ a) (fromᵇ a) (fromᵇ b + fromᵇ b)
--  | assoc-+ (fromᵇ a) (fromᵇ b) (fromᵇ b)
--  = refl
--mononat (a O) (b I) rewrite
--    mononat a b
--  | sym (succ_right (fromᵇ a + fromᵇ a) (fromᵇ b + fromᵇ b))
--  | assoc-+ (fromᵇ a) (fromᵇ b) (fromᵇ a + fromᵇ b)
--  | comm-+ (fromᵇ b) (fromᵇ a + fromᵇ b)
--  | assoc-+ (fromᵇ a) (fromᵇ a) (fromᵇ b + fromᵇ b)
--  | assoc-+ (fromᵇ a) (fromᵇ b) (fromᵇ b)
--  = refl
--mononat (a I) (b I) rewrite
--    mononat₁ a b
--  | sym (succ_right (fromᵇ a + fromᵇ a) (fromᵇ b + fromᵇ b))
--  | sym (succ_right (fromᵇ a + fromᵇ b) (fromᵇ a + fromᵇ b))
--  | assoc-+ (fromᵇ a) (fromᵇ b) (fromᵇ a + fromᵇ b)
--  | comm-+ (fromᵇ b) (fromᵇ a + fromᵇ b)
--  | assoc-+ (fromᵇ a) (fromᵇ a) (fromᵇ b + fromᵇ b)
--  | assoc-+ (fromᵇ a) (fromᵇ b) (fromᵇ b)
--  = refl

-- Really hard!! Keep working on it!
-- IT WASN'T HARD!!! I JUST COULDN'T SEE THE RIGHT REWRITE!!
from∘toᵇ : ∀ (n : ℕ) → fromᵇ (toᵇ n) ≡ n
from∘toᵇ zero = refl
from∘toᵇ (suc n) rewrite inc≡suc (toᵇ n) | from∘toᵇ n = refl
--from∘toᵇ (suc n) =
--  begin
--    fromᵇ (toᵇ (suc n))
--  ≡⟨⟩
--    fromᵇ (inc (toᵇ n))
--  ≡⟨ inc≡suc (toᵇ n) ⟩
--    suc (fromᵇ (toᵇ n))
--  ≡⟨ cong suc (from∘toᵇ n) ⟩
--    suc n
--  ∎

swap-m-n-+ : ∀ (m n p) → m + (n + p) ≡ n + (m + p)
swap-m-n-+ m n p rewrite
    sym (assoc-+ m n p)
  | sym (assoc-+ n m p)
  | comm-+ m n
  = refl

right-zero-* : ∀ (n : ℕ) → n * 0 ≡ 0
right-zero-* zero    = refl
right-zero-* (suc n) rewrite right-zero-* n = refl

suc-right-* : ∀ (m n) → m * suc n ≡ m + m * n
suc-right-* zero n = refl
suc-right-* (suc m) n
  rewrite
    suc-right-* m n
  | sym (assoc-+ n m (m * n))
  | comm-+ n m
  | assoc-+ m n (m * n)
  = refl

comm-* : ∀ (m n : ℕ) → m * n ≡ n * m
comm-* zero n rewrite right-zero-* n = refl
comm-* (suc m) n
  rewrite
    comm-* m n
  | suc-right-* n m
  = refl

distr-*-+ : ∀ (m n p) → (m + n) * p ≡ m * p + n * p
distr-*-+ zero _ _ = refl
distr-*-+ (suc m) n p rewrite distr-*-+ m n p | assoc-+ p (m * p) (n * p) = refl

distl-*-+ : ∀ (p m n) → p * (m + n) ≡ p * m + p * n
distl-*-+ zero _ _ = refl
distl-*-+ (suc p) m n
  rewrite
    distl-*-+ p m n
  | sym (assoc-+ (m + n) (p * m) (p * n))
  | sym (assoc-+ (m + p * m) n (p * n))
  | assoc-+ m n (p * m)
  | comm-+ n (p * m)
  | assoc-+ m (p * m) n
  = refl

assoc-* : ∀ (m n p : ℕ) → (m * n) * p ≡ m * (n * p)
assoc-* zero n p = refl
assoc-* (suc m) n p
  rewrite
    assoc-* m n p
  | distr-*-+ n (m * n) p
  | assoc-* m n p
  = refl

swap-m-n-* : ∀ (m n p) → m * (n * p) ≡ n * (m * p)
swap-m-n-* m n p rewrite
    sym (assoc-* m n p)
  | sym (assoc-* n m p)
  | comm-* m n
  = refl

distr-^-* : ∀ (m n p) → (m * n) ^ p ≡ (m ^ p) * (n ^ p)
--distr-^-* m n zero    =
--  begin
--    (m * n) ^ zero
--  ≡⟨⟩
--    (m * n) ^ 0
--  ≡⟨⟩
--    suc 0
--  ≡⟨⟩
--    1
--  ≡⟨⟩
--    1 + 0
--  ≡⟨⟩
--    1 + 0 * 1
--  ≡⟨⟩
--    (suc 0) * 1
--  ≡⟨⟩
--    (suc 0) * 1
--  ≡⟨⟩
--    1 * 1
--  ≡⟨⟩
--    1 * (n ^ 0)
--  ≡⟨⟩
--    (m ^ zero) * (n ^ zero)
--  ≡⟨⟩
--    (m ^ 0) * (n ^ 0)
--  ∎
distr-^-* _       _       zero    = refl
distr-^-* zero    zero    (suc p) = refl
distr-^-* zero    (suc n) (suc p) = refl
distr-^-* (suc m) zero    (suc p)
  rewrite
    right-zero-* (suc m ^ suc p)
  | right-zero-* m
  = refl
distr-^-* (suc m) (suc n) (suc p)
  rewrite
--  (suc m * suc n) ^ suc p ≡ suc m ^ suc p * suc n ^ suc p
--
--    suc (n + m * suc n) ^ p + (n + m * suc n) * suc (n + m * suc n) ^ p
--  ≡ (suc m ^ p + m * suc m ^ p) * (suc n ^ p + n * suc n ^ p)
    distr-*-+ (suc m ^ p) (m * suc m ^ p) (suc n ^ p + n * suc n ^ p)
--    suc (n + m * suc n) ^ p + (n + m * suc n) * suc (n + m * suc n) ^ p
--  ≡
--    suc m ^ p * (suc n ^ p + n * suc n ^ p)
--    + m * suc m ^ p * (suc n ^ p + n * suc n ^ p)
  | assoc-* m (suc m ^ p) (suc n ^ p + n * suc n ^ p)
--    suc (n + m * suc n) ^ p + (n + m * suc n) * suc (n + m * suc n) ^ p
--  ≡
--    suc m ^ p * (suc n ^ p + n * suc n ^ p)
--    + m * (suc m ^ p * (suc n ^ p + n * suc n ^ p))
  | distl-*-+ (suc m ^ p) (suc n ^ p) (n * suc n ^ p)
--    suc (n + m * suc n) ^ p + (n + m * suc n) * suc (n + m * suc n) ^ p
--  ≡
--    suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p)
--    + m * (suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p))
  | comm-* m (suc n)
--    suc (n + (m + n * m)) ^ p
--    + (n + (m + n * m)) * suc (n + (m + n * m)) ^ p
--  ≡
--    suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p)
--    + m * (suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p))
  | comm-* n m
--    suc (n + (m + m * n)) ^ p
--    + (n + (m + m * n)) * suc (n + (m + m * n)) ^ p
--  ≡
--    suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p)
--    + m * (suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p))
  | sym (suc-right-* m n)
--    suc (n + m * suc n) ^ p
--    + (n + m * suc n) * suc (n + m * suc n) ^ p
--  ≡
--    suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p)
--    + m * (suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p))
  | distr-^-* (suc m) (suc n) p
--    suc m ^ p * suc n ^ p + (n + m * suc n) * (suc m ^ p * suc n ^ p)
--  ≡
--    suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p)
--    + m * (suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p))
  | distr-*-+ n (m * suc n) (suc m ^ p * suc n ^ p)
--    suc m ^ p * suc n ^ p + (n * (suc m ^ p * suc n ^ p)
--    + m * suc n * (suc m ^ p * suc n ^ p))
--    ≡
--    suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p)
--    + m * (suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p))
  | distl-*-+ m (suc m ^ p * suc n ^ p) (suc m ^ p * (n * suc n ^ p))
--    suc m ^ p * suc n ^ p + (n * (suc m ^ p * suc n ^ p)
--    + m * suc n * (suc m ^ p * suc n ^ p))
--  ≡
--    suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p)
--    + (m * (suc m ^ p * suc n ^ p) + m * (suc m ^ p * (n * suc n ^ p)))
  | comm-* m (suc n)
--    suc m ^ p * suc n ^ p + (n * (suc m ^ p * suc n ^ p)
--    + (m + n * m) * (suc m ^ p * suc n ^ p))
--  ≡
--    suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p)
--    + (m * (suc m ^ p * suc n ^ p) + m * (suc m ^ p * (n * suc n ^ p)))
  | distr-*-+ m (n * m) (suc m ^ p * suc n ^ p)
--    suc m ^ p * suc n ^ p +
--    (n * (suc m ^ p * suc n ^ p) +
--     (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p)))
--  ≡
--    suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p) +
--    (m * (suc m ^ p * suc n ^ p) + m * (suc m ^ p * (n * suc n ^ p)))
  | swap-m-n-* n (suc m ^ p) (suc n ^ p)
--    suc m ^ p * suc n ^ p +
--    (suc m ^ p * (n * suc n ^ p) +
--     (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p)))
--  ≡
--    suc m ^ p * suc n ^ p + suc m ^ p * (n * suc n ^ p) +
--    (m * (suc m ^ p * suc n ^ p) + m * (suc m ^ p * (n * suc n ^ p)))
  | swap-m-n-* (suc m ^ p) n (suc n ^ p)
--    suc m ^ p * suc n ^ p +
--    (n * (suc m ^ p * suc n ^ p) +
--     (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p)))
--  ≡
--    suc m ^ p * suc n ^ p + n * (suc m ^ p * suc n ^ p) +
--    (m * (suc m ^ p * suc n ^ p) + m * (n * (suc m ^ p * suc n ^ p)))
  | sym (assoc-* m n (suc m ^ p * suc n ^ p))
--    suc m ^ p * suc n ^ p +
--    (n * (suc m ^ p * suc n ^ p) +
--     (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p)))
--  ≡
--    suc m ^ p * suc n ^ p + n * (suc m ^ p * suc n ^ p) +
--    (m * (suc m ^ p * suc n ^ p) + m * n * (suc m ^ p * suc n ^ p))
  | comm-* m n
--    suc m ^ p * suc n ^ p +
--    (n * (suc m ^ p * suc n ^ p) +
--     (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p)))
--    ≡
--    suc m ^ p * suc n ^ p + n * (suc m ^ p * suc n ^ p) +
--    (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p))
  | assoc-+ (suc m ^ p * suc n ^ p) (n * (suc m ^ p * suc n ^ p))
    (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p))
--    suc m ^ p * suc n ^ p +
--    (n * (suc m ^ p * suc n ^ p) +
--     (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p)))
--    ≡
--    suc m ^ p * suc n ^ p +
--    (n * (suc m ^ p * suc n ^ p) +
--     (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p)))
-- QED
  = refl

--distr-^-* (suc m) (suc n) (suc p)
--  rewrite
--    distr-*-+ (suc m ^ p) (m * suc m ^ p) (suc n ^ p + n * suc n ^ p)
--  | assoc-* m (suc m ^ p) (suc n ^ p + n * suc n ^ p)
--  | distl-*-+ (suc m ^ p) (suc n ^ p) (n * suc n ^ p)
--  | comm-* m (suc n)
--  | comm-* n m
--  | sym (suc-right-* m n)
--  | distr-^-* (suc m) (suc n) p
--  | distr-*-+ n (m * suc n) (suc m ^ p * suc n ^ p)
--  | distl-*-+ m (suc m ^ p * suc n ^ p) (suc m ^ p * (n * suc n ^ p))
--  | comm-* m (suc n)
--  | distr-*-+ m (n * m) (suc m ^ p * suc n ^ p)
--  | swap-m-n-* n (suc m ^ p) (suc n ^ p)
--  | swap-m-n-* (suc m ^ p) n (suc n ^ p)
--  | sym (assoc-* m n (suc m ^ p * suc n ^ p))
--  | comm-* m n
--  | assoc-+ (suc m ^ p * suc n ^ p) (n * (suc m ^ p * suc n ^ p))
--    (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p))
--  = refl

--------------------------------------- Relations ---------------------------------------

data _≤_ : ℕ → ℕ → Set where
  z≤n : ∀ {n : ℕ} → zero ≤ n
  s≤s : ∀ {m n : ℕ} → m ≤ n → suc m ≤ suc n

_ : 2 ≤ 4
_ = s≤s (s≤s z≤n)
--_ = s≤s {1} {3} (s≤s {0} {2} (z≤n {2}))

inv-s≤s : ∀ {m n : ℕ} → suc m ≤ suc n → m ≤ n
inv-s≤s (s≤s m≤n) = m≤n

inv-z≤n : ∀ {m : ℕ} → m ≤ zero → m ≡ zero
inv-z≤n z≤n = refl

refl-≤ : ∀ {n : ℕ} → n ≤ n
refl-≤ {zero}  = z≤n
refl-≤ {suc o} = s≤s refl-≤

trans-≤ : ∀ {m n p : ℕ} → m ≤ n → n ≤ p → m ≤ p
trans-≤ z≤n       _         = z≤n
trans-≤ (s≤s m≤n) (s≤s n≤p) = s≤s (trans-≤ m≤n n≤p)

antisym-≤ : ∀ {m n : ℕ} → m ≤ n → n ≤ m → m ≡ n
antisym-≤ z≤n       n≤m       = sym (inv-z≤n n≤m)
antisym-≤ (s≤s m≤n) (s≤s n≤m) = cong suc (antisym-≤ m≤n n≤m)

open import Data.Sum using (_⊎_; inj₁; inj₂) renaming ([_,_] to case-⊎)

--data Total (m n : ℕ) : Set where
--  forward : m ≤ n → Total m n
--  flipped : n ≤ m → Total m n
--
--total-≤ : ∀ (m n : ℕ) → Total m n
--total-≤ zero    _    = forward z≤n
--total-≤ (suc m) zero = flipped z≤n
--total-≤ (suc m) (suc n) with total-≤ m n
--...                     | forward m≤n = forward (s≤s m≤n)
--...                     | flipped n≤m = flipped (s≤s n≤m)

total-≤` : ∀ (m n : ℕ) → m ≤ n ⊎ n ≤ m
total-≤` zero    _       = inj₁ z≤n
total-≤` _       zero    = inj₂ z≤n
total-≤` (suc m) (suc n) with total-≤` m n
...                      | inj₁ m≤n = inj₁ (s≤s m≤n)
...                      | inj₂ n≤m = inj₂ (s≤s n≤m)

--+-monoʳ-≤ : ∀ (n p q : ℕ) → p ≤ q → (n + p) ≤ (n + q)
--+-monoʳ-≤ zero    _ _ p≤q = p≤q
--+-monoʳ-≤ (suc n) p q p≤q = s≤s (+-monoʳ-≤ n p q p≤q)

+-monoʳ-≤ : ∀ {n p q : ℕ} → p ≤ q → (n + p) ≤ (n + q)
+-monoʳ-≤ {zero}  p≤q = p≤q
+-monoʳ-≤ {suc n} p≤q = s≤s (+-monoʳ-≤ {n} p≤q)

+-monoˡ-≤ : ∀ {m n p : ℕ} → m ≤ n → (m + p) ≤ (n + p)
+-monoˡ-≤ {m} {n} {p} m≤n rewrite comm-+ m p | comm-+ n p = +-monoʳ-≤ m≤n

-- From book
≤-trans : ∀ {m n p : ℕ} → m ≤ n → n ≤ p → m ≤ p
≤-trans z≤n       _          =  z≤n
≤-trans (s≤s m≤n) (s≤s n≤p)  =  s≤s (≤-trans m≤n n≤p)

+-mono-≤ : ∀ {m n p q : ℕ} → m ≤ n → p ≤ q → (m + p) ≤ (n + q)
+-mono-≤ m≤n p≤q = ≤-trans (+-monoˡ-≤ m≤n) (+-monoʳ-≤ p≤q)

-- Exercises
*-monoʳ-≤ : ∀ {n p q : ℕ} → p ≤ q → (n * p) ≤ (n * q)
*-monoʳ-≤ {zero}  p≤q = z≤n
*-monoʳ-≤ {suc n} p≤q = +-mono-≤ p≤q (*-monoʳ-≤ {n} p≤q)

*-monoˡ-≤ : ∀ {m n p : ℕ} → m ≤ n → (m * p) ≤ (n * p)
*-monoˡ-≤ {m} {n} {p} m≤n rewrite comm-* m p | comm-* n p = *-monoʳ-≤ {p} {m} {n} m≤n

*-mono-≤ : ∀ {m n p q : ℕ} → m ≤ n → p ≤ q → (m * p) ≤ (n * q)
*-mono-≤ {_} {n} m≤n p≤q = ≤-trans (*-monoˡ-≤ m≤n) (*-monoʳ-≤ {n} p≤q)

infix 4 _<_

data _<_ : ℕ → ℕ → Set where
  z<s : ∀ {n : ℕ} → zero < suc n
  s<s : ∀ {m n : ℕ} → m < n → suc m < suc n

<-trans : ∀ {m n p} → m < n → n < p → m < p
--<-trans {m} {suc n} {suc p} z<s (s<s n<p) = z<s {p}
--<-trans {suc m} {suc n} {suc p} (s<s m<n) (s<s n<p) = s<s (<-trans m<n n<p)
<-trans z<s       (s<s n<p) = z<s
<-trans (s<s m<n) (s<s n<p) = s<s (<-trans m<n n<p)

tricotomy : ∀ (m n) → (m ≡ n) ⊎ (m < n) ⊎ (n < m)
tricotomy zero    zero    = inj₁ refl
tricotomy zero    (suc n) = inj₂ (inj₁ z<s)
tricotomy (suc m) zero    = inj₂ (inj₂ z<s)
tricotomy (suc m) (suc n) with tricotomy m n
...                       | inj₁ m≡n        = inj₁ (cong suc m≡n)
...                       | inj₂ (inj₁ m<n) = inj₂ (inj₁ (s<s m<n))
...                       | inj₂ (inj₂ n<m) = inj₂ (inj₂ (s<s n<m))

+-monoʳ-< : ∀ {n p q} → p < q → (n + p) < (n + q)
+-monoʳ-< {zero}  p<q = p<q
+-monoʳ-< {suc n} p<q = s<s (+-monoʳ-< p<q)

+-monoˡ-< : ∀ {m n p} → m < n → (m + p) < (n + p)
+-monoˡ-< {m} {n} {p} m<n rewrite comm-+ m p | comm-+ n p = +-monoʳ-< m<n

+-mono-< : ∀ {m n p q} → m < n → p < q → (m + p) < (n + q)
+-mono-< m<n p<q = <-trans (+-monoˡ-< m<n) (+-monoʳ-< p<q)

--open import Function.Equivalence using (_⇔_)
record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A

open _⇔_

<-if-≤ : ∀ {m n} → suc m ≤ n → m < n
<-if-≤ {zero}  {suc n} z≤s        = z<s
<-if-≤ {suc m} {suc n} (s≤s sm≤n) = s<s (<-if-≤ sm≤n)

≤-if-< : ∀ {m n} → m < n → suc m ≤ n
≤-if-< {zero} {suc n} z<s = s≤s z≤n
≤-if-< {suc m} {suc n} (s<s m<n) = s≤s (≤-if-< m<n)

≤-iff-< : ∀ {m n} → (suc m ≤ n) ⇔ (m < n)
≤-iff-< = record
  { to   = <-if-≤
  ; from = ≤-if-<
  }

pred-smaller : ∀ {m n} → suc m ≤ n → m ≤ n
pred-smaller {zero}          _          = z≤n
pred-smaller {suc m} {suc n} (s≤s sm≤n) = s≤s (pred-smaller sm≤n)

<-trans-revisited : ∀ {m n p} → m < n → n < p → m < p
<-trans-revisited {m} {n} {p} m<n n<p
  = <-if-≤ (≤-trans (≤-if-< m<n) (pred-smaller (≤-if-< n<p)))

---

data even : ℕ → Set
data odd  : ℕ → Set

data even where 
  zero-e : even zero
  suc-e  : ∀ {n : ℕ} → odd n → even (suc n)

data odd where 
  suc-o : ∀ {n : ℕ} → even n → odd (suc n)


--- 

data Can : Bin → Set
data One : Bin → Set

data Can where
  zero-C : Can (⟨⟩ O)
  one-C : ∀ {b : Bin} → One b → Can b

data One where
  oneO : One (⟨⟩ I)
  oneO-O : ∀ {b : Bin} → One b → One (b O)
  oneO-I : ∀ {b : Bin} → One b → One (b I)

_ : Can (⟨⟩ I)
_ = one-C oneO

inc-Bin : ∀ {b : Bin} → One b → One (inc b)
inc-Bin oneO        = oneO-O oneO
inc-Bin (oneO-O ob) = oneO-I ob
inc-Bin (oneO-I ob) = oneO-O (inc-Bin ob)

inc-Can : ∀ {b : Bin} → Can b → Can (inc b)
inc-Can zero-C     = one-C oneO
inc-Can (one-C ob) = one-C (inc-Bin ob)

to-Can : ∀ (n : ℕ) → Can (toᵇ n)
to-Can zero = zero-C
to-Can (suc n) = inc-Can (to-Can n)

twicebinisO : ∀ {b : Bin} → One b → b +ᵇ b ≡ b O
twicebinisO {⟨⟩ I} _ = refl
twicebinisO {b O} (oneO-O ob) rewrite twicebinisO ob = refl
twicebinisO {b I} (oneO-I ob) rewrite twicebinisO ob = refl

to∘from-Can : ∀ {b : Bin} → Can b → toᵇ (fromᵇ b) ≡ b
to∘from-Can zero-C = refl
to∘from-Can (one-C oneO) = refl
to∘from-Can {b O} (one-C (oneO-O ob))
  rewrite monobin (fromᵇ b)
        | to∘from-Can (one-C ob)
        | twicebinisO ob = refl
to∘from-Can {b I} (one-C (oneO-I ob))
  rewrite monobin (fromᵇ b)
        | to∘from-Can (one-C ob)
        | twicebinisO ob = refl

--------------------------------------- Equality ---------------------------------------

module ≤-Reasoning where
  infix  1 begin≤_
  infixr 2 _≤⟨⟩_ _≤⟨_⟩_
  infix  3 _∎≤
  
  begin≤_ : ∀ {x y : ℕ} → x ≤ y → x ≤ y
  begin≤ x≤y = x≤y
  
  _≤⟨⟩_ : ∀ (x : ℕ) {y : ℕ} → x ≤ y → x ≤ y
  x ≤⟨⟩ x≤y = x≤y

  _≤⟨_⟩_ : ∀ (x : ℕ) {y z : ℕ} → x ≤ y → y ≤ z → x ≤ z
  x ≤⟨ x≤y ⟩ y≤z = trans-≤ x≤y y≤z
  
  _∎≤ : ∀ (x : ℕ) → x ≤ x
  x ∎≤ = refl-≤

open ≤-Reasoning

+-monoʳ-≤` : ∀ {n p q : ℕ} → p ≤ q → (n + p) ≤ (n + q)
+-monoʳ-≤` {zero} p≤q = p≤q
+-monoʳ-≤` {suc n} {p} {q} p≤q =
  begin≤
    suc n + p
  ≤⟨⟩
    suc (n + p)
  ≤⟨ s≤s (+-monoʳ-≤` p≤q) ⟩
    suc (n + q)
  ≤⟨⟩
    suc n + q
  ∎≤

pred : ℕ → ℕ
pred zero = zero
pred (suc n) = n

pred≡ : ∀ {m n : ℕ} → suc m ≡ suc n → m ≡ n
pred≡ = cong pred

≡to≤ : ∀ {m n : ℕ} → m ≡ n → m ≤ n
≡to≤ {zero}  {zero}  _     = refl-≤
≡to≤ {suc m} {suc n} sm≡sn = s≤s (≡to≤ (cong pred sm≡sn))

+-monoˡ-≤` : ∀ {m n p : ℕ} → m ≤ n → (m + p) ≤ (n + p)
+-monoˡ-≤` {m} {n} {p} m≤n =
  begin≤
    m + p
  ≤⟨ ≡to≤ (comm-+ m p) ⟩
    p + m
  ≤⟨ +-monoʳ-≤` {p} {m} {n} m≤n ⟩
    p + n
  ≤⟨ ≡to≤ (comm-+ p n) ⟩
    n + p
  ∎≤

+-mono-≤` : ∀ {m n p q : ℕ} → m ≤ n → p ≤ q → (m + p) ≤ (n + q)
+-mono-≤` {m} {n} {p} {q} m≤n p≤q =
  begin≤
    m + p
  ≤⟨ +-monoˡ-≤` m≤n ⟩
    n + p
  ≤⟨ +-monoʳ-≤` p≤q ⟩
    n + q
  ∎≤

--even-comm′ : ∀ (m n : ℕ)
--  → even (m + n)
--    ------------
--  → even (n + m)
--even-comm′ m n ev with   m + n  | comm-+ m n
--...                  | .(n + m) | refl       = ev

--------------------------------------- Isomorphism ---------------------------------------

postulate
  extensionality : ∀ {A B : Set} {f g : A → B}
    → (∀ (x : A) → f x ≡ g x)
      -----------------------
    → f ≡ g

_+′_ : ℕ → ℕ → ℕ
m +′ zero  = m
m +′ suc n = suc (m +′ n)

same-app : ∀ (m n : ℕ) → m +′ n ≡ m + n
same-app m n rewrite comm-+ m n = helper m n
  where
    helper : ∀ (m n : ℕ) → m +′ n ≡ n + m
    helper _ zero    = refl
    helper m (suc n) = cong suc (helper m n)

same-+-+′ : _+′_ ≡ _+_
same-+-+′ = extensionality (λ m → extensionality (λ n → same-app m n))

--open import Level using (Level; _⊔_) renaming (zero to lzero; suc to lsuc)
--
--private
--  variable
--    ℓ ℓ₁ : Level

--infix 0 _≃_
--record _≃_ (A : Set ℓ) (B : Set ℓ₁) : Set (ℓ ⊔ ℓ₁) where
record _≃_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
open _≃_

≃-refl : ∀ {A : Set} → A ≃ A
≃-refl =
  record
    { to      = λ x → x
    ; from    = λ y → y
    ; from∘to = λ x → refl
    ; to∘from = λ y → refl
    }

≃-sym : ∀ {A B : Set} → A ≃ B → B ≃ A
≃-sym A≃B =
  record
    { to      = from A≃B
    ; from    = to A≃B
    ; from∘to = to∘from A≃B
    ; to∘from = from∘to A≃B
    }

open import Function.Base using (_∘_)

≃-trans : ∀ {A B C : Set} → A ≃ B → B ≃ C → A ≃ C
≃-trans A≃B B≃C =
  record
    { to      = to B≃C ∘ to A≃B
    ; from    = from A≃B ∘ from B≃C
    ; from∘to = λ{x →
        begin
          from A≃B (from B≃C (to B≃C (to A≃B x)))
        ≡⟨ cong (from A≃B) (from∘to B≃C (to A≃B x)) ⟩
          from A≃B (to A≃B x)
        ≡⟨ from∘to A≃B x ⟩
          x
        ∎
      }
    ; to∘from = λ{y →
        begin
          to B≃C (to A≃B (from A≃B (from B≃C y)))
        ≡⟨ cong (to B≃C) (to∘from A≃B (from B≃C y)) ⟩
          to B≃C (from B≃C y)
        ≡⟨ to∘from B≃C y ⟩
          y
        ∎
      }
    }

infix 0 _≲_
record _≲_ (A B : Set) : Set where
  field
    to      : A → B
    from    : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
open _≲_

≃-implies-≲ : ∀ {A B : Set} → A ≃ B → A ≲ B
≃-implies-≲ A≃B =
  record
  { to   = to A≃B
  ; from = from A≃B
  ; from∘to = from∘to A≃B
  }

-- Idea: Prove that (ℕ) is isomorph to (Can)
-- Idea: Prove that (+, ℕ) is isomorph to (+ᵇ, Can)
--record _⇔_ (A B : Set) : Set where
--  field
--    to   : A → B
--    from : B → A

-- Exercise: implement reflexive, symetric and transitive properties on _⇔_

ℕ≲Bin : ℕ ≲ Bin
ℕ≲Bin =
  record
  { to   = toᵇ
  ; from = fromᵇ
  ; from∘to = from∘toᵇ
  }

data Dec-Can : Set where
  num : ∀ (b : Bin) → Can b → Dec-Can

toᵈᶜ : ℕ → Dec-Can
toᵈᶜ n = num (toᵇ n) (to-Can n)
--toᵈᶜ n = num (to-Can n)

fromᵈᶜ : Dec-Can → ℕ
fromᵈᶜ (num b _) = fromᵇ b

--from∘toᵈᶜ (suc n) = ?
-- λ n → fromᵈᶜ (toᵈᶜ (suc n)) ≡ suc n
-- λ cb → toᵈᶜ (fromᵈᶜ (num cb)) ≡ num cb
-- λ b ob → toᵈᶜ (fromᵈᶜ (num (b O) (one-C (oneO-O ob))))

--to∘fromᵈᶜ : ∀ (y : Dec-Can) → toᵈᶜ (fromᵈᶜ y) ≡ y
--to∘fromᵈᶜ (num (⟨⟩ O) zero-C)       = refl
--to∘fromᵈᶜ (num (⟨⟩ I) (one-C oneO)) = refl
--to∘fromᵈᶜ (num (b O) (one-C (oneO-O {b} ob))) rewrite monobin (fromᵇ b) = ?
---- λ b ob → toᵈᶜ (fromᵈᶜ (num (b O) (one-C (oneO-O ob))))
---- λ b ob → λ b ob → num (toᵇ (fromᵇ b + fromᵇ b)) (to-Can (fromᵇ b + fromᵇ b))
--to∘fromᵈᶜ = ?
----to∘fromᵈᶜ (num b cb) =
----  begin
----    toᵈᶜ (fromᵈᶜ (num b cb))
----  ≡⟨⟩
----    toᵈᶜ (fromᵇ b)
----  ≡⟨⟩
----    num (toᵇ (fromᵇ b)) (to-Can (fromᵇ b))
----  ≡⟨ ? ⟩
----    num b cb
----  ∎

ℕ≃Can : ℕ ≃ Dec-Can
ℕ≃Can =
  record
  { to      = toᵈᶜ
  ; from    = fromᵈᶜ
  ; from∘to = from∘toᵇ
  ; to∘from = ? -- to∘fromᵈᶜ -- to∘from-Can
  }

--num (zero-C)

--------------------------------------- Decidable ---------------------------------------
-- Use `Dec` to create elements for `Can`. Look if this is enough to prove `ℕ ≃ Can`

--------------------------------------- Last ---------------------------------------
-- Prove that `sqrt 2` is irrational

-- Stuff to take into account and add to agda neovim:
-- IOTCM "plfa/part1/Naturals.agda" None Direct (Cmd_load "plfa/part1/Naturals.agda" [])
-- IOTCM "plfa/part1/Naturals.agda" None Direct (Cmd_compute_toplevel DefaultCompute "2 + suc 3") # Already done by plugin
-- IOTCM "plfa/part1/Naturals.agda" None Direct (Cmd_infer_toplevel AsIs "2 + suc 3")  # THIS SHOULD BE PART OF THE STUFF IN MAGDA
--
-- Other stuff:
-- IOTCM "plfa/part1/Naturals.agda" None Direct (Cmd_show_module_contents_toplevel AsIs "Eq")
-- IOTCM "plfa/part1/Naturals.agda" None Direct (Cmd_why_in_scope_toplevel "Eq")
-- IOTCM "plfa/part1/Naturals.agda" None Direct (Cmd_why_in_scope (InteractionId 1) (Range (Just "plfa/part1/Naturals.agda") (Interval (Pn () 1 76 1) (Pn () 1 76 19))) "Eq")
---- IOTCM "plfa/part1/Naturals.agda" None Direct (Cmd_autoOne (InteractionId 1) noRange "")
---- IOTCM "plfa/part1/Naturals.agda" NonInteractive Indirect (Cmd_infer Simplified 103992 (intervalsToRange (Just (mkAbsolute "plfa/part1/Naturals.agda")) [Interval (Pn () 50 75 1) (Pn () 52 79 1)]) "3 + suc 2")
