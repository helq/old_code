module sv20.assign1.First where

{- Code partially taken from the book "Programming Language Foundations in
 - Agda" by Philip Wadler, Wen Kokke, Jeremy Siek and many others. The book
 - can be found at https://plfa.github.io/
 -
 - Based on chapter 1 - Natural numbers - https://plfa.github.io/Naturals/
 -
 - The solution for the homework can be found from line 161
 -}

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

-- Gives us the power of writing `3` to mean `suc (suc (suc zero))` :)
{-# BUILTIN NATURAL ℕ #-}

import Relation.Binary.PropositionalEquality as Eq
open import Function.Base using (flip)
open Eq using (_≡_; _≢_; refl; cong; sym; trans)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_; _∎)

_+_ : ℕ → ℕ → ℕ
zero + n = n                -- +-def₀
(suc m) + n = suc (m + n)   -- +-def₁

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

assoc-+ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
assoc-+ zero n p = refl
assoc-+ (suc m) n p rewrite assoc-+ m n p = refl

comm-+₀ : ∀ (m : ℕ) → m + zero ≡ m
comm-+₀ zero = refl
-- Proofs on natural numbers can be written in a manner that ressembles a pen
-- and paper proof. For example:
comm-+₀ (suc n) =
  begin
    suc n + zero
  ≡⟨⟩ -- By addition definition
    suc (n + zero)
  ≡⟨ cong suc (comm-+₀ n) ⟩
    suc n
  ∎
-- But often, proofs are written for Agda to check them and not to humans to
-- read them. The above proof can be written succintly as:
--comm-+₀ (suc n) rewrite comm-+₀ n = refl

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
distr-*-+ (suc m) n p
  rewrite
    distr-*-+ m n p
  | assoc-+ p (m * p) (n * p)
  = refl

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

------------------------------------------------------------------------------------------
--                                   HOMEWORK SOLUTION                                  --
------------------------------------------------------------------------------------------

-- We want to proof that:
distr-^-* : ∀ (m n p) → (m * n) ^ p ≡ (m ^ p) * (n ^ p)

-- We will prove the `distr-^-*` by induction. To prove something by induction
-- in Agda, we use recursion.

-- The first basic case is when `p = 0`
distr-^-* m n zero =
  begin
    (m * n) ^ zero
  ≡⟨⟩ -- The builtin option "NATURAL" allows us to write `0` instead of `zero`
    (m * n) ^ 0
  ≡⟨⟩ -- because:   n ^ zero  =  suc zero
    suc 0
  ≡⟨⟩ -- because:   zero + n = n
    suc (0 + 0)
  ≡⟨⟩ -- because:   (suc m) + n = suc (m + n)
    suc 0 + 0
  ≡⟨⟩ -- Agda builtin option "NATURAL" allows us to write `1` instead of `suc 0`
    1 + 0
  ≡⟨⟩ -- because:   zero    * n  =  zero
    1 + 0 * 1
  ≡⟨⟩ -- because:   (suc m) * n  =  n + (m * n)
    suc 0 * 1
  ≡⟨⟩ -- `1` instead of `suc 0`
    1 * 1
  ≡⟨⟩ -- because:   n ^ zero  =  suc zero
    1 * (n ^ 0)
  ≡⟨⟩ -- because:   n ^ zero  =  suc zero
    (m ^ 0) * (n ^ 0)
  ∎

-- Agda computes every expression it is given until it can't apply any more
-- rules, i.e, Agda reduces every expression to its normal form.
-- For example, if we give Agda the expression `2 + 1`, it will apply the
-- rule `(suc m) + n = suc (m + n)` until it arrives to
-- `suc (suc (suc zero))`, `3`.
--
-- Notice that every line in the previous proof reduces to `1`. This means
-- that Agda read the previous proof as:
--
--distr-^-* m n zero =
--  begin
--    1
--  ≡⟨⟩
--    1
--  ≡⟨⟩ -- ...
--    1
--  ≡⟨⟩
--    1
--  ∎
--
-- So we could have written a shorter proof saying that `1 ≡⟨⟩ 1 ∎` and it
-- would sufficient for Agda to approve our proof.
--
--distr-^-* _ _ zero = 1 ≡⟨⟩ 1 ∎
--
-- In fact, `1 ≡⟨⟩ 1 ∎` can be written as `refl` in Agda.
--
--distr-^-* _ _ zero = refl

-- The following two base cases can be easily proven with the same technique as
-- the previous proof:
distr-^-* zero _ (suc p) = refl

-- For the last base case, we need a little more human involvement because the
-- expressions do not reduce completly to a common normal form.
distr-^-* (suc m) zero (suc p) =
  begin
    (suc m * 0) ^ suc p
  ≡⟨⟩ -- Agda reduces the expression to
    m * 0 * (m * 0) ^ p
  -- Applying `right-zero-*` (`n * 0 ≡ 0`) lemma to the left side
  ≡⟨ cong (_* (m * 0) ^ p) (right-zero-* m) ⟩
    0 * (m * 0) ^ p
  ≡⟨⟩ -- reduces to
    0
  -- Applying `sym ∘ right-zero-*` (`0 ≡ n * 0`) lemma to the left side
  ≡⟨ sym (right-zero-* (suc m ^ suc p)) ⟩
    (suc m ^ suc p) * 0
  ≡⟨⟩ -- reduces to
    (suc m ^ p + m * suc m ^ p) * 0
  ≡⟨⟩ -- reduced from our goal
    (suc m ^ suc p) * (0 ^ suc p)
  ∎
--
-- Notice how the previous proof required us to tell Agda which "lemmas" to use
-- to be able to link the left side of the proof with the right side.
--
-- As before, we have more ways to write the previous proof. We can forgo all
-- the intermediate steps and tell Agda which lemmas the proof needs.
--
--distr-^-* (suc m) 0 (suc p) =
--  trans
--    (cong (_* (m * 0) ^ p) (right-zero-* m))
--    (sym (right-zero-* (suc m ^ suc p)))
--
-- An even shorter proof can be written using `rewrite`. `rewrite` will try to
-- apply the `lemmas` we tell it to apply once, descending order. If the
-- rewriting process works by the end, we will only need `refl` to indicate
-- that both sides of the proof are the same.
--
--distr-^-* (suc m) zero (suc p)
--  rewrite
---- We start with what we want to proof:
----
----  (suc m * zero) ^ suc p ≡ suc m ^ suc p * zero ^ suc p
----
----  which reduces to
----
----  m * 0 * (m * 0) ^ p ≡ (suc m ^ p + m * suc m ^ p) * 0
----
--    right-zero-* m -- asking to rewrite all appearances of `m * 0` for `0` 
----
----  λ m p → 0 * 0 ^ p ≡ (suc m ^ p + m * suc m ^ p) * 0
----
----  which reduces to
----
----  0 ≡ (suc m ^ p + m * suc m ^ p) * 0
----
--  -- asking to rewrite all appearances of `(suc m ^ suc p) * 0` for `0`
--  -- (notice that: `(suc m ^ suc p)` reduces to `(suc m ^ p + m * suc m ^ p)`)
--  | right-zero-* (suc m ^ suc p)
----
----  0 ≡ 0
----
----  We have arrived to a expression where both sides are equal. Thus we have
----  found a proof.
--  = refl
--



--  The final part of the inductive proof requires us to proof the inductive
--  case:
distr-^-* (suc m) (suc n) (suc p)
  rewrite
-- First we start with expression:
--
--  (m * n) ^ p ≡ (m ^ p) * (n ^ p)
--
--  Reducing the expression  to its normal form:
--
--    suc (n + m * suc n) ^ p + (n + m * suc n) * suc (n + m * suc n) ^ p
--  ≡ (suc m ^ p + m * suc m ^ p) * (suc n ^ p + n * suc n ^ p)
--  Applying distributivity of _+_ over _*_
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
  | distr-^-* (suc m) (suc n) p -- INDUCTIVE HYPOTHESIS!!
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
--
-- We finally find that the two expressions are equal!
--
--    suc m ^ p * suc n ^ p +
--    (n * (suc m ^ p * suc n ^ p) +
--     (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p)))
--    ≡
--    suc m ^ p * suc n ^ p +
--    (n * (suc m ^ p * suc n ^ p) +
--     (m * (suc m ^ p * suc n ^ p) + n * m * (suc m ^ p * suc n ^ p)))
--
-- QED
  = refl

-- The full proof without comments:
--distr-^-* : ∀ (m n p) → (m * n) ^ p ≡ (m ^ p) * (n ^ p)
--distr-^-* _       _       zero    = refl
--distr-^-* zero    _       (suc p) = refl
--distr-^-* (suc m) zero    (suc p)
--  rewrite
--    right-zero-* m
--  | right-zero-* (suc m ^ suc p)
--  = refl
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
