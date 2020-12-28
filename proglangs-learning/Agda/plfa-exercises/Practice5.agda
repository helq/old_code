module plfa-exercises.Practice5 where

open import Data.Nat using (ℕ; zero; suc)
open import Data.String using (String; _≟_)
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl; cong)
open import Relation.Nullary using (Dec; yes; no; ¬_)
open import plfa.part1.Isomorphism using (_≲_)

Id : Set
Id = String

infix  5  ƛ_⇒_ μ_⇒_
infixl 7  _·_
infix  8  `suc_
infix  9  `_

data Term : Set where
  `_                      :  Id → Term
  ƛ_⇒_                    :  Id → Term → Term
  _·_                     :  Term → Term → Term
  `zero                   :  Term
  `suc_                   :  Term → Term
  case_[zero⇒_|suc_⇒_]    :  Term → Term → Id → Term → Term
  μ_⇒_                    :  Id → Term → Term

--ƛ "x" ⇒ `suc `zero

one = `suc `zero
two = `suc one

plus : Term
plus = μ "+" ⇒ ƛ "m" ⇒ ƛ "n" ⇒
         case ` "m"
           [zero⇒ ` "n"
           |suc "m" ⇒ `suc (` "+" · ` "m" · ` "n") ]

--plus · two · two

twoᶜ : Term
twoᶜ =  ƛ "s" ⇒ ƛ "z" ⇒ ` "s" · (` "s" · ` "z")

plusᶜ : Term
plusᶜ =  ƛ "m" ⇒ ƛ "n" ⇒ ƛ "s" ⇒ ƛ "z" ⇒
         ` "m" · ` "s" · (` "n" · ` "s" · ` "z")

sucᶜ : Term
sucᶜ = ƛ "n" ⇒ `suc (` "n")

----- Little detour into the same definitions but working in Agda
--plus c                    one           two
--(λ m n s z → m s (n s z)) (λ s z → s z) (λ s z → s (s z)) suc zero

Nat : Set₁
Nat = ∀ {A : Set} → (A → A) → A → A

--my_two : ∀ {A : Set} → (A → A) → A → A
my_two : Nat
my_two = (λ s z → s (s z))

--my_four : ∀ {A : Set} → (A → A) → A → A
my_four : Nat
my_four = (λ s z → s (s (s (s z))))

--(λ m n s z → m s (n s z)) my_two my_four
--my_add : ∀ {A : Set} → ((A → A) → A → A) → ((A → A) → A → A) → (A → A) → A → A
my_add : Nat → Nat → Nat
my_add = (λ m n s z → m s (n s z))
--my_add = (λ m n s → (m s) ∘ (n s))

-- my_add my_two my_four   ≡   λ s z → s (s (s (s (s (s z)))))

six : ℕ
six = my_add my_two my_four suc zero
--six = 6

--my_mul : ∀ {A : Set} → ((A → A) → A → A) → ((A → A) → A → A) → (A → A) → A → A
my_mul : Nat → Nat → Nat
my_mul = (λ m n s z → m (n s) z)
--my_mul = (λ m n s → m (n s))

--my_mul my_two my_four  ≡  λ s z → s (s (s (s (s (s (s (s z)))))))
eight_true : my_mul my_two my_four suc zero ≡ 8
eight_true = refl
----- End of detour

-- Exercises
mult : Term
mult = μ "*" ⇒ ƛ "m" ⇒ ƛ "n" ⇒
         case ` "m"
           [zero⇒ `zero
           |suc "m" ⇒ plus · (` "*" · ` "m" · ` "n") · ` "n" ]

multᶜ : Term
multᶜ =  ƛ "m" ⇒ ƛ "n" ⇒ ƛ "s" ⇒ ƛ "z" ⇒
         ` "m" · (` "n" · ` "s") · ` "z"
--- End Exercises

data Value : Term → Set where
  V-ƛ : ∀ {x N} → Value (ƛ x ⇒ N)
  V-zero : Value `zero
  V-suc : ∀ {V} → Value V → Value (`suc V)


-- Notice that this only works if we are working with _closed_ terms.
-- Open terms require more care

infix 9 _[_:=_]

-- Wrote them partly by myself
_[_:=_] : Term → Id → Term → Term
(` x) [ x' := V ] with x ≟ x'
... | yes _ = V
... | no _  = ` x
(ƛ x ⇒ M) [ x' := V ] with x ≟ x'
... | yes _ = ƛ x ⇒ M
... | no _  = ƛ x ⇒ (M [ x' := V ])
(M · N) [ x := V ] = (M [ x := V ]) · (N [ x := V ])
`zero [ _ := _ ] = `zero
(`suc M) [ x := V ] = `suc (M [ x := V ])
(case n [zero⇒ M |suc n' ⇒ N ]) [ x := V ] with x ≟ n'
... | yes _ = case (n [ x := V ]) [zero⇒ (M [ x := V ]) |suc n' ⇒ N ]
... | no _  = case (n [ x := V ]) [zero⇒ (M [ x := V ]) |suc n' ⇒ (N [ x := V ]) ]
(μ f ⇒ M) [ x := V ] with f ≟ x
... | yes _ = μ f ⇒ M
... | no _  = μ f ⇒ (M [ x := V ])

-- (ƛ "y" ⇒ ` "x" · (ƛ "x" ⇒ ` "x")) [ "x" := `zero ]


infix 4 _—→_

data _—→_ : Term → Term → Set where

  ξ-·₁ : ∀ {L L′ M}
    → L —→ L′
      -----------------
    → L · M —→ L′ · M

  ξ-·₂ : ∀ {V M M′}
    → Value V
    → M —→ M′
      -----------------
    → V · M —→ V · M′

  β-ƛ : ∀ {x N V}
    → Value V
      ------------------------------
    → (ƛ x ⇒ N) · V —→ N [ x := V ]

  ξ-suc : ∀ {M M′}
    → M —→ M′
      ------------------
    → `suc M —→ `suc M′

  ξ-case : ∀ {x L L′ M N}
    → L —→ L′
      -----------------------------------------------------------------
    → case L [zero⇒ M |suc x ⇒ N ] —→ case L′ [zero⇒ M |suc x ⇒ N ]

  β-zero : ∀ {x M N}
      ----------------------------------------
    → case `zero [zero⇒ M |suc x ⇒ N ] —→ M

  β-suc : ∀ {x V M N}
    → Value V
      ---------------------------------------------------
    → case `suc V [zero⇒ M |suc x ⇒ N ] —→ N [ x := V ]

  β-μ : ∀ {x M}
      ------------------------------
    → μ x ⇒ M —→ M [ x := μ x ⇒ M ]


_ : (ƛ "x" ⇒ `suc (`suc (` "x"))) · (`suc `zero) —→ `suc (`suc (`suc `zero))
_ = β-ƛ (V-suc V-zero)

_ : (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x")  —→  (ƛ "x" ⇒ ` "x")
_ = β-ƛ V-ƛ

_ : (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x")  —→  (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x")
_ = ξ-·₁ (β-ƛ V-ƛ)

_ : twoᶜ · sucᶜ · `zero  —→  (ƛ "z" ⇒ sucᶜ · (sucᶜ · ` "z")) · `zero
_ = ξ-·₁ (β-ƛ V-ƛ)

--- Detour
t : ∀ {A B : Set} → A → B → A
t = λ x y → x -- true
f : ∀ {A B : Set} → A → B → B
f = λ x y → y -- false
--is0 : ∀ {A : Set} → ((A → A → A → A) → (A → A → A) → A → A → A) → A → A → A
--is0 {A} = λ n → n (λ x → f {A}) (t {A})
--is0 : ∀ {A B : Set} → ((A → A → A → A) → (A → A → A) → A → A → A) → (A → A) → A → A
--is0 {A} {B} = λ n → n (λ x → f {A} {B}) (t {A} {B})

-- is0 {ℕ} (λ s z → z)   -- returns true
-- is0 (λ s z → s z) -- returns false
--- End detour

infix  2 _—↠_
infix  1 begin_
infixr 2 _—→⟨_⟩_ _—→⟨⟩_
infix  3 _∎

data _—↠_ : Term → Term → Set where
  _∎ : ∀ M
      ---------
    → M —↠ M

  _—→⟨_⟩_ : ∀ L {M N}
    → L —→ M
    → M —↠ N
      ---------
    → L —↠ N

begin_ : ∀ {M N}
  → M —↠ N
    ------
  → M —↠ N
begin M—↠N = M—↠N

_—→⟨⟩_ : ∀ L {N}
  → L —↠ N
    ---------
  → L —↠ N
_—→⟨⟩_ l l—↠n = l—↠n

trans : ∀ {L M N}
      → L —↠ M
      → M —↠ N
      → L —↠ N
trans (m ∎) m—↠n = m—↠n
trans (l —→⟨ l—→o ⟩ o—↠m) m—↠n = l —→⟨ l—→o ⟩ (trans o—↠m m—↠n)

_ : (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x")  —↠  (ƛ "x" ⇒ ` "x")
_ =
  begin
    (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x") —→⟨ ξ-·₁ (ξ-·₁ (β-ƛ V-ƛ)) ⟩
    (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x")                   —→⟨ ξ-·₁ (β-ƛ V-ƛ) ⟩
    (ƛ "x" ⇒ ` "x") · (ƛ "x" ⇒ ` "x")                                     —→⟨ β-ƛ V-ƛ ⟩
    ƛ "x" ⇒ ` "x"
  ∎


data _—↠′_ : Term → Term → Set where

  step′ : ∀ {M N}
    → M —→ N
      -------
    → M —↠′ N

  refl′ : ∀ {M}
      -------
    → M —↠′ M

  trans′ : ∀ {L M N}
    → L —↠′ M
    → M —↠′ N
      -------
    → L —↠′ N

↠≲—↠′ : ∀ t t' → (t —↠ t') ≲ (t —↠′ t')
↠≲—↠′ t t' = record {
   to      = to
 ; from    = from
 ; from∘to = from∘to
 }
 where
    to : ∀ {t t'} → t —↠ t' → t —↠′ t'
    to (m ∎) = refl′ {m}
    to (l —→⟨ l—→m ⟩ m—↠n) = trans′ {l} (step′ l—→m) (to m—↠n)
    
    from : ∀ {t t'} → t —↠′ t' → t —↠ t'
    from (step′ {m} {n} m—→n) = m —→⟨ m—→n ⟩ n ∎
    from (refl′ {m}) = m ∎
    from (trans′ {l} {m} {n} l—↠′m m—↠′n) =
      trans (from l—↠′m) (from m—↠′n)
    
    from∘to : ∀ {l n} (x : l —↠ n) → from (to x) ≡ x
    from∘to (n ∎) = refl
    from∘to (l —→⟨ l—→m ⟩ m—↠n) = cong (l —→⟨ l—→m ⟩_) (from∘to m—↠n)
    
    --to∘from : ∀ {l n} (x : l —↠′ n) → to (from x) ≡ x
    --to∘from (step′ {m} {n} m—→n) = ?
    ---- here lies the problem:
    ----   to (from (step′ m—→n)) ≡ step′ m—→n
    ---- is converted into:
    ----   trans′ (step′ m—→n) refl′ ≡ step′ m—→n
    ---- which cannot be true :/, both are constructors, both
    ---- create the same type. Lesson, always make sure your data
    ---- definitions make unique elements
    --to∘from = ?

_ : twoᶜ · sucᶜ · `zero —↠ `suc `suc `zero
_ = begin
  twoᶜ · sucᶜ · `zero                                       —→⟨⟩  -- def
  (ƛ "s" ⇒ ƛ "z" ⇒ ` "s" · (` "s" · ` "z")) · sucᶜ · `zero  —→⟨ ξ-·₁ (β-ƛ V-ƛ) ⟩
  (ƛ "z" ⇒ sucᶜ · (sucᶜ · ` "z")) · `zero                   —→⟨ β-ƛ V-zero ⟩
  sucᶜ · (sucᶜ · `zero)                                     —→⟨⟩
  (ƛ "n" ⇒ `suc (` "n")) · ((ƛ "n" ⇒ `suc (` "n")) · `zero) —→⟨ ξ-·₂ V-ƛ (β-ƛ V-zero) ⟩
  (ƛ "n" ⇒ `suc (` "n")) · `suc `zero                       —→⟨ β-ƛ (V-suc V-zero) ⟩
  `suc `suc `zero  ∎

oneᶜ : Term
oneᶜ = ƛ "s" ⇒ ƛ "z" ⇒ ` "s" · ` "z"

_ : plusᶜ · oneᶜ · oneᶜ · sucᶜ · `zero —↠ `suc `suc `zero
_ = begin
  plusᶜ · oneᶜ · oneᶜ · sucᶜ · `zero                                           —→⟨⟩
  plusᶜ · oneᶜ · oneᶜ · sucᶜ · `zero                                           —→⟨⟩
  (ƛ "m" ⇒ ƛ "n" ⇒ ƛ "s" ⇒ ƛ "z" ⇒
         ` "m" · ` "s" · (` "n" · ` "s" · ` "z")) · oneᶜ · oneᶜ · sucᶜ · `zero —→⟨ ξ-·₁ (ξ-·₁ (ξ-·₁ (β-ƛ V-ƛ))) ⟩
  (ƛ "n" ⇒ ƛ "s" ⇒ ƛ "z" ⇒
         oneᶜ · ` "s" · (` "n" · ` "s" · ` "z")) · oneᶜ · sucᶜ · `zero         —→⟨ ξ-·₁ (ξ-·₁ (β-ƛ V-ƛ)) ⟩
  (ƛ "s" ⇒ ƛ "z" ⇒ oneᶜ · ` "s" · (oneᶜ  · ` "s" · ` "z")) · sucᶜ · `zero      —→⟨ ξ-·₁ (β-ƛ V-ƛ) ⟩
  (ƛ "z" ⇒ oneᶜ · sucᶜ  · (oneᶜ  · sucᶜ · ` "z")) · `zero                      —→⟨ β-ƛ V-zero ⟩
  oneᶜ · sucᶜ  · (oneᶜ  · sucᶜ · `zero)                                        —→⟨⟩
  (ƛ "s" ⇒ ƛ "z" ⇒ ` "s" · ` "z") · sucᶜ  · (oneᶜ  · sucᶜ · `zero)             —→⟨ ξ-·₁ (β-ƛ V-ƛ) ⟩
  (ƛ "z" ⇒ sucᶜ · ` "z") · (oneᶜ  · sucᶜ · `zero)                              —→⟨ ξ-·₂ V-ƛ (ξ-·₁ (β-ƛ V-ƛ)) ⟩
  (ƛ "z" ⇒ sucᶜ · ` "z") · ((ƛ "z" ⇒ sucᶜ · ` "z") · `zero)                    —→⟨ ξ-·₂ V-ƛ (β-ƛ V-zero) ⟩
  (ƛ "z" ⇒ sucᶜ · ` "z") · (sucᶜ · `zero)                                      —→⟨ ξ-·₂ V-ƛ (β-ƛ V-zero) ⟩
  (ƛ "z" ⇒ sucᶜ · ` "z") · (`suc `zero)                                        —→⟨ β-ƛ (V-suc V-zero) ⟩
  sucᶜ · (`suc `zero)                                                          —→⟨ β-ƛ (V-suc V-zero) ⟩
  `suc `suc `zero ∎
