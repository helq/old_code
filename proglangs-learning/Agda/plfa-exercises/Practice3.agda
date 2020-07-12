module plfa-exercises.Practice3 where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; sym; trans; cong)
open Eq.≡-Reasoning using (_≡⟨⟩_; _≡⟨_⟩_; begin_; _∎)
open import Data.Bool using (Bool; true; false; T; _∧_; _∨_; not)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_; _≤_; s≤s; z≤n)
open import Data.Nat.Properties using
  (+-assoc; +-identityˡ; +-identityʳ; *-assoc; *-identityˡ; *-identityʳ; *-distribʳ-+; +-suc; *-suc; *-comm)
open import Relation.Nullary using (¬_; Dec; yes; no)
open import Data.Product using (_×_; ∃; ∃-syntax) renaming (_,_ to ⟨_,_⟩)
open import Function using (_∘_; flip)
open import Level using (Level)
open import plfa.part1.Isomorphism using (_≃_; _⇔_)
--import Data.List.All using (All; []; _∷_)
open import Data.List using (List; []; _∷_; _++_; length; reverse; map; foldr; downFrom; foldl)
open import Axiom.Extensionality.Propositional using (Extensionality)
import Data.Nat.Solver as NatSolver
open import Data.Sum using (_⊎_; inj₁; inj₂)

-- Exercises from plfa Lists

map∘compose₀ : ∀ {A B C : Set} (f : A → B) (g : B → C) (xs : List A) → map (g ∘ f) xs ≡ (map g ∘ map f) xs
map∘compose₀ f g [] = refl
map∘compose₀ f g (x ∷ xs) = cong (g (f x) ∷_) (map∘compose₀ f g xs)

module _ (ext : ∀ {ℓ ℓ′} → Extensionality ℓ ℓ′) where

  map∘compose : ∀ {A B C : Set}
                (f : A → B) (g : B → C)
              → map (g ∘ f) ≡ (map g ∘ map f)
  map∘compose f g = ext (map∘compose₀ f g)


map-++-distribute : ∀ {A B : Set}
                    (f : A → B) (xs ys : List A)
                  → map f (xs ++ ys) ≡ map f xs ++ map f ys
map-++-distribute f []       ys = refl
map-++-distribute f (x ∷ xs) ys = cong (f x ∷_) (map-++-distribute f xs ys)


data Tree (A B : Set) : Set where
  leaf : A → Tree A B
  node : Tree A B → B → Tree A B → Tree A B

map-Tree : ∀ {A B C D : Set} → (A → C) → (B → D) → Tree A B → Tree C D
map-Tree f g (leaf a) = leaf (f a)
map-Tree f g (node lt b rt) = node (map-Tree f g lt) (g b) (map-Tree f g rt)

--node (leaf 5) (3 ∷ []) (leaf 0)
--map-Tree suc (6 ∷_) (node (leaf 5) (3 ∷ []) (leaf 0))

product : List ℕ → ℕ
product = foldr _*_ 1

_ : product (1 ∷ 2 ∷ 3 ∷ 4 ∷ []) ≡ 24
_ = refl

sum : List ℕ → ℕ
sum = foldr _+_ 0

sum-downFrom : ∀ (n : ℕ) → sum (downFrom n) * 2 ≡ n * (n ∸ 1)
sum-downFrom zero = refl
sum-downFrom 1    = refl
sum-downFrom (suc (suc n))
  rewrite
--    sum (downFrom (suc (suc n))) * 2 ≡ suc (suc n) * (suc (suc n) ∸ 1)
--    suc (suc ((n + (n + foldr _+_ 0 (downFrom n))) * 2)) ≡ suc (n + suc (n + n * suc n))
      *-distribʳ-+ 2 n (foldr _+_ 0 (downFrom (suc n)))
--    suc (suc (n * 2 + (n + foldr _+_ 0 (downFrom n)) * 2)) ≡ suc (n + suc (n + n * suc n))
    | sum-downFrom (suc n)
--    suc (suc (n * 2 + (n + n * n))) ≡ suc (n + suc (n + n * suc n))
    | +-suc n (n + n * suc n)
--    suc (suc (n * 2 + (n + n * n))) ≡ suc (suc (n + (n + n * suc n)))
    | *-suc n n
--    suc (suc (n * 2 + (n + n * n))) ≡ suc (suc (n + (n + (n + n * n))))
    | *-comm n 2
--    suc (suc (n + (n + 0) + (n + n * n))) ≡ suc (suc (n + (n + (n + n * n))))
    | +-identityʳ n
--    suc (suc (n + n + (n + n * n))) ≡ suc (suc (n + (n + (n + n * n))))
    | +-assoc n n (n + n * n)
--    suc (suc (n + (n + (n + n * n)))) ≡ suc (suc (n + (n + (n + n * n))))
  = refl

sum-downFrom` : ∀ (n : ℕ) → sum (downFrom n) * 2 ≡ n * (n ∸ 1)
sum-downFrom` zero = refl
sum-downFrom` 1    = refl
sum-downFrom` (suc (suc n))
  rewrite
--    sum (downFrom (suc (suc n))) * 2 ≡ suc (suc n) * (suc (suc n) ∸ 1)
--    suc (suc ((n + (n + foldr _+_ 0 (downFrom n))) * 2)) ≡ suc (n + suc (n + n * suc n))
      *-distribʳ-+ 2 n (foldr _+_ 0 (downFrom (suc n)))
--    suc (suc (n * 2 + (n + foldr _+_ 0 (downFrom n)) * 2)) ≡ suc (n + suc (n + n * suc n))
    | sum-downFrom` (suc n)
--    suc (suc (n * 2 + (n + n * n))) ≡ suc (n + suc (n + n * suc n))
  = simplified n
  where
    open NatSolver using (module +-*-Solver)
    open +-*-Solver using (solve; _:+_; _:*_; _:=_; con)

    simplified : ∀ n → 2 + (n * 2 + (n + n * n)) ≡ 1 + (n + (1 + (n + n * (1 + n))))
    simplified = solve 1 (λ n → con 2 :+ (n :* con 2 :+ (n :+ n :* n)) := con 1 :+ (n :+ (con 1 :+ (n :+ n :* (con 1 :+ n))))) refl

-- solve 1 (λ n → n :+ con 1 := con 1 :+ n) refl
--open NatSolver using (module +-*-Solver)
--open +-*-Solver using (solve; _:+_; _:*_; _:=_; con; Polynomial)
--
--lemma : ∀ x y → x + y * 1 + 3 ≡ 2 + 1 + y + x
--lemma = solve 2 (λ x y → x :+ y :* con 1 :+ con 3 := con 2 :+ con 1 :+ y :+ x) refl

record IsMonoid {A : Set} (_⊗_ : A → A → A) (e : A) : Set where
  field
    assoc : ∀ (x y z : A) → (x ⊗ y) ⊗ z ≡ x ⊗ (y ⊗ z)
    identityˡ : ∀ (x : A) → e ⊗ x ≡ x
    identityʳ : ∀ (x : A) → x ⊗ e ≡ x

open IsMonoid

foldr-monoid : ∀ {A : Set} (_⊗_ : A → A → A) (e : A) → IsMonoid _⊗_ e →
  ∀ (xs : List A) (y : A) → foldr _⊗_ y xs ≡ foldr _⊗_ e xs ⊗ y
foldr-monoid _⊗_ e ⊗-monoid [] y =
  begin
    foldr _⊗_ y []
  ≡⟨⟩
    y
  ≡⟨ sym (identityˡ ⊗-monoid y) ⟩
    (e ⊗ y)
  ≡⟨⟩
    foldr _⊗_ e [] ⊗ y
  ∎
foldr-monoid _⊗_ e ⊗-monoid (x ∷ xs) y =
  begin
    foldr _⊗_ y (x ∷ xs)
  ≡⟨⟩
    x ⊗ (foldr _⊗_ y xs)
  ≡⟨ cong (x ⊗_) (foldr-monoid _⊗_ e ⊗-monoid xs y) ⟩
    x ⊗ (foldr _⊗_ e xs ⊗ y)
  ≡⟨ sym (assoc ⊗-monoid x (foldr _⊗_ e xs) y) ⟩
    (x ⊗ foldr _⊗_ e xs) ⊗ y
  ≡⟨⟩
    foldr _⊗_ e (x ∷ xs) ⊗ y
  ∎

--foldl : {A B : Set} (f : B → A → B) → B → List A → B
--foldl f b [] = b
--foldl f b (a ∷ as') = foldl f (f b a) as'

_ : foldl _∸_ 20 (4 ∷ 3 ∷ []) ≡ 13
_ =
 begin
  foldl _∸_ 20             (4 ∷ 3 ∷ []) ≡⟨⟩
  foldl _∸_ (20 ∸ 4)       (3 ∷ [])     ≡⟨⟩
  foldl _∸_ ((20 ∸ 4) ∸ 3) []           ≡⟨⟩
  (20 ∸ 4) ∸ 3                          ≡⟨⟩
  13
 ∎

_ : foldr _∸_ 20 (4 ∷ 3 ∷ []) ≡ 4
_ =
 begin
  foldr _∸_ 20 (4 ∷ 3 ∷ []) ≡⟨⟩
  4 ∸ foldr _∸_ 20 (3 ∷ []) ≡⟨⟩
  4 ∸ (3 ∸ foldr _∸_ 20 []) ≡⟨⟩
  4 ∸ (3 ∸ 20) ≡⟨⟩
  4 ∸ 0 ≡⟨⟩
  4
 ∎

foldl-monoid : {A : Set}
               (_⊗_ : A → A → A) (e : A)
             → IsMonoid _⊗_ e
             → ∀ (y : A) (xs : List A)
             → (y ⊗ foldl _⊗_ e xs) ≡ foldl _⊗_ y xs
foldl-monoid _⊗_ e monoid-⊗ y [] = identityʳ monoid-⊗ y
foldl-monoid _⊗_ e monoid-⊗ y (x ∷ xs) =
  begin
    y ⊗ foldl _⊗_ e (x ∷ xs)  ≡⟨⟩
    y ⊗ foldl _⊗_ (e ⊗ x) xs  ≡⟨ cong (λ exp → y ⊗ foldl _⊗_ exp xs) (identityˡ monoid-⊗ x) ⟩
    y ⊗ foldl _⊗_ x xs        ≡⟨ cong (y ⊗_) (sym (foldl-monoid _⊗_ e monoid-⊗ x xs)) ⟩
    y ⊗ (x ⊗ foldl _⊗_ e xs)  ≡⟨ sym (assoc monoid-⊗ y x (foldl _⊗_ e xs)) ⟩
    (y ⊗ x) ⊗ foldl _⊗_ e xs  ≡⟨ foldl-monoid _⊗_ e monoid-⊗ (y ⊗ x) xs ⟩
    foldl _⊗_ (y ⊗ x) xs      ≡⟨⟩
    foldl _⊗_ y (x ∷ xs)
  ∎

foldr-monoid-foldl₀ : ∀ {A : Set} {_⊗_ : A → A → A} {e : A}
                   → (xs : List A)
                   → IsMonoid _⊗_ e
                   → foldr _⊗_ e xs ≡ foldl _⊗_ e xs
foldr-monoid-foldl₀ [] _ = refl
foldr-monoid-foldl₀ {_} {_⊗_} {e} (x ∷ xs) monoid-⊗ =
  begin
    foldr _⊗_ e (x ∷ xs)   ≡⟨⟩
    x ⊗ foldr _⊗_ e xs     ≡⟨ cong (x ⊗_) (foldr-monoid-foldl₀ xs monoid-⊗) ⟩
    x ⊗ foldl _⊗_ e xs     ≡⟨ foldl-monoid _⊗_ e monoid-⊗ x xs ⟩
    foldl _⊗_ x xs         ≡⟨ cong (λ exp → foldl _⊗_ exp xs) (sym (identityˡ monoid-⊗ x)) ⟩
    foldl _⊗_ (e ⊗ x) xs   ≡⟨⟩
    foldl _⊗_ e (x ∷ xs)
  ∎

module _ (ext : ∀ {ℓ ℓ′} → Extensionality ℓ ℓ′) where
  foldr-monoid-foldl : ∀ {A : Set} {_⊗_ : A → A → A} {e : A}
                     → IsMonoid _⊗_ e
                     → foldr _⊗_ e ≡ foldl _⊗_ e
  foldr-monoid-foldl monoid-⊗ = ext (λ xs → foldr-monoid-foldl₀ xs monoid-⊗)


data All {A : Set} (P : A → Set) : List A → Set where
  []  : All P []
  _∷_ : ∀ {x : A} {xs : List A} → P x → All P xs → All P (x ∷ xs)

pattern [_] z = z ∷ []
pattern [_,_] y z = y ∷ z ∷ []
pattern [_,_,_] x y z = x ∷ y ∷ z ∷ []
pattern [_,_,_,_] w x y z = w ∷ x ∷ y ∷ z ∷ []
pattern [_,_,_,_,_] v w x y z = v ∷ w ∷ x ∷ y ∷ z ∷ []
pattern [_,_,_,_,_,_] u v w x y z = u ∷ v ∷ w ∷ x ∷ y ∷ z ∷ []

_ : All (_≤ 2) [ 0 , 1 , 2 ]  -- These brackes desugar to ∷ and [] for List definition
_ = [ z≤n , s≤s z≤n , s≤s (s≤s z≤n) ]  -- ∷ and [] from All constructors

++-assoc : ∀ {A : Set} (xs ys zs : List A)
  → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
++-assoc [] ys zs = refl
++-assoc (x ∷ xs) ys zs =
  begin
    ((x ∷ xs) ++ ys) ++ zs ≡⟨⟩
    x ∷ (xs ++ ys) ++ zs   ≡⟨ cong (x ∷_) (++-assoc xs ys zs) ⟩
    x ∷ xs ++ ys ++ zs     ≡⟨⟩
    (x ∷ xs) ++ ys ++ zs
  ∎

length-++ : ∀ {A : Set} (xs ys : List A)
  → length (xs ++ ys) ≡ length xs + length ys
length-++ [] ys = refl
length-++ (x ∷ xs) ys =
  begin
    length ((x ∷ xs) ++ ys)      ≡⟨⟩
    suc (length (xs ++ ys))      ≡⟨ cong suc (length-++ xs ys) ⟩
    suc (length xs + length ys)  ≡⟨⟩
    suc (length xs) + length ys  ≡⟨⟩
    length (x ∷ xs) + length ys
  ∎

++-[] : ∀ {A : Set} (xs : List A) → xs ++ [] ≡ xs
++-[] [] = refl
++-[] (x ∷ xs) = cong (x ∷_) (++-[] xs)

data Any {A : Set} (P : A → Set) : List A → Set where
  here  : ∀ {x : A} {xs : List A} → P x → Any P (x ∷ xs)
  there : ∀ {x : A} {xs : List A} → Any P xs → Any P (x ∷ xs)

infix 4 _∈_ _∉_

_∈_ : ∀ {A : Set} (x : A) (xs : List A) → Set
x ∈ xs = Any (x ≡_) xs

_∉_ : ∀ {A : Set} (x : A) (xs : List A) → Set
x ∉ xs = ¬ (x ∈ xs)

_ : 0 ∈ [ 0 , 1 , 0 , 2 ]
_ = here refl
--_ = there (there (here refl))

---- Left as exercise for another day
--reverse-inv : ∀ {A : Set} (x : A) (xs : List A)
--  → reverse (x ∷ xs) ≡ reverse xs ++ [ x ]
--reverse-inv _ [] = refl
--reverse-inv y (x ∷ xs) =
--  begin
--    reverse (y ∷ x ∷ xs)  ≡⟨⟩
--    foldl _∷`_ [ x , y ] xs  ≡⟨ ? ⟩ -- LEFT FOR LATER
--    foldl _∷`_ [ x ] xs ++ [ y ]  ≡⟨⟩
--    reverse (x ∷ xs) ++ [ y ]
--  ∎
---- λ y x xs →  ≡ 
---- λ y x xs → foldl (λ y₁ x₁ → x₁ ∷ y₁) [ x , y ] xs ≡ foldl (λ y₁ x₁ → x₁ ∷ y₁) [ x ] xs ++ [ y ]
--  where
--  _∷`_ = flip _∷_
--
--reverse-++-distrib : ∀ {A : Set} (xs ys : List A)
--  → reverse (xs ++ ys) ≡ reverse ys ++ reverse xs
--reverse-++-distrib [] ys = sym (++-[] (reverse ys))
--reverse-++-distrib (x ∷ xs) ys =
--  begin
--    reverse ((x ∷ xs) ++ ys)                  ≡⟨⟩
--    reverse (x ∷ (xs ++ ys))                  ≡⟨ reverse-inv x (xs ++ ys) ⟩
--    reverse (xs ++ ys) ++ [ x ]               ≡⟨ cong (_++ [ x ]) (reverse-++-distrib xs ys) ⟩
--    (reverse ys ++ reverse xs) ++ [ x ]       ≡⟨ ++-assoc (reverse ys) (reverse xs) [ x ] ⟩
--    reverse ys ++ (reverse xs ++ [ x ])       ≡⟨ sym (cong (reverse ys ++_) (reverse-inv x xs)) ⟩
--    reverse ys ++ reverse (x ∷ xs)
--  ∎
--    --foldl _∷`_ [ x ] (xs ++ ys)               ≡⟨ ? ⟩
--    --foldl _∷`_ [] ys ++ foldl _∷`_ [ x ] xs   ≡⟨⟩

exercise-823→ : ∀ {A : Set} {z : A} (xs ys : List A)
              → z ∈ (xs ++ ys) → (z ∈ xs) ⊎ (z ∈ ys)
exercise-823→ []       [] ()
exercise-823→ []       _  z∈[]++ys         = inj₂ z∈[]++ys
exercise-823→ (_ ∷ _)  _  (here refl)      = inj₁ (here refl)
exercise-823→ (x ∷ xs) ys (there z∈xs++ys) with exercise-823→ xs ys z∈xs++ys
...                                       | inj₁ z∈xs = inj₁ (there z∈xs)
...                                       | inj₂ z∈ys = inj₂ z∈ys

exercise-823← : ∀ {A : Set} {z : A} (xs ys : List A)
             → (z ∈ xs) ⊎ (z ∈ ys) → z ∈ (xs ++ ys)
exercise-823← []       _  (inj₁ ())
exercise-823← []       _  (inj₂ z∈ys) = z∈ys
exercise-823← (_ ∷ _)  [] (inj₂ ())
exercise-823← (_ ∷ _)  _  (inj₁ (here z∈xs)) = here z∈xs
exercise-823← (x ∷ xs) ys (inj₁ (there z∈xs)) with exercise-823← xs ys (inj₁ z∈xs)
...                                           | z∈xs++ys = there z∈xs++ys
exercise-823← (_ ∷ xs) ys (inj₂ z∈y∷ys) with exercise-823← xs ys (inj₂ z∈y∷ys)
...                                     | z∈xs++y∷ys = there z∈xs++y∷ys

exercise-823 : ∀ {A : Set} {z : A} (xs ys : List A)
             → (z ∈ xs ++ ys) ⇔ (z ∈ xs ⊎ z ∈ ys)
exercise-823 xs ys = record { to = exercise-823→ xs ys ; from = exercise-823← xs ys }
