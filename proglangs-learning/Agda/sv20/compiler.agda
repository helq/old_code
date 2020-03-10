module sv20.compiler where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_)
--open import Data.Nat.DivMod using (_/_)
open import Data.List using (List; _++_; []; _∷_; head)
open import Data.Maybe as DM
open DM using (Maybe; just; nothing; maybe; _>>=_; from-just; From-just)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; sym; trans; cong; subst)
open import Data.Product using (∃-syntax) renaming (_,_ to ⟨_,_⟩)
open import Function.Base using (_∘_; flip)

pattern [_] z = z ∷ []
pattern [_,_] y z = y ∷ z ∷ []
pattern [_,_,_] x y z = x ∷ y ∷ z ∷ []
pattern [_,_,_,_] w x y z = w ∷ x ∷ y ∷ z ∷ []
pattern [_,_,_,_,_] v w x y z = v ∷ w ∷ x ∷ y ∷ z ∷ []
pattern [_,_,_,_,_,_] u v w x y z = u ∷ v ∷ w ∷ x ∷ y ∷ z ∷ []

data ExOp : Set where
  sum diff prod : ExOp

data Exp : Set where
  const : ℕ → Exp
  exop : ExOp → Exp → Exp → Exp

--exop sum (const 5) (const 2)

prim-ex : ExOp → ℕ → ℕ → ℕ
prim-ex sum  = _+_
prim-ex diff = _∸_
prim-ex prod = _*_

interpreter : Exp → ℕ
interpreter (const ℕ) = ℕ
interpreter (exop op x y) = prim-ex op (interpreter x) (interpreter y)

--interpreter (exop sum (const 5) (const 2))

data BinOp : Set where
  add sub mul : BinOp

data StackCmd : Set where
  push : ℕ → StackCmd
  binop : BinOp → StackCmd

Program = List StackCmd
Stack = List ℕ

prim-op : BinOp → ℕ → ℕ → ℕ
prim-op add = _+_
prim-op sub = _∸_
prim-op mul = _*_

run-vm : Program → Stack → Maybe Stack
run-vm [] st = just st
run-vm (push n ∷ ps) st = run-vm ps (n ∷ st)
run-vm (binop op ∷ ps) (n ∷ m ∷ st) = run-vm ps ((prim-op op n m) ∷ st)
run-vm _ _ = nothing

exop-binop : ExOp → BinOp
exop-binop sum = add
exop-binop diff = sub
exop-binop prod = mul

compile : Exp → Program
compile (const n) = [ push n ]
compile (exop op x y) = compile y ++ compile x ++ [ binop (exop-binop op) ]

-- (3 - 2) + 6  =>  + (- 3 2) 6   =>  "sum (diff 3 2) 6"
_ : compile (exop sum (exop diff (const 3) (const 2)) (const 6))
    ≡ [ push 6 , push 2 , push 3 , binop sub , binop add ]
_ = refl

_ : run-vm (compile (const 3)) [] ≡ just [ interpreter (const 3) ]
_ = refl

--run-vm [ push 2 , push 1 , binop add ] []
--run-vm [ push 1 , binop add ] [ 2 ]
--run-vm [ binop add ] [ 1 , 2 ]
_ :   (run-vm [ push 1 ] [ 2 ] >>= run-vm [ binop add ])
    ≡ run-vm [ push 1 , binop add ] [ 2 ]
_ = refl

lemma₁ : ∀ (a b : Program) (s : Stack) → run-vm (a ++ b) s ≡ (run-vm a s >>= run-vm b)
lemma₁ [] _ _ = refl
lemma₁ (push n ∷ ps) b s rewrite lemma₁ ps b (n ∷ s) = refl
lemma₁ (binop _ ∷ ps) b          []  = refl
lemma₁ (binop _ ∷ _)  _     (_ ∷ []) = refl
lemma₁ (binop op ∷ ps) b (n ∷ m ∷ st) rewrite lemma₁ ps b (prim-op op n m ∷ st) = refl

lemma₂ : ∀ {op : ExOp} → prim-op (exop-binop op) ≡ prim-ex op
lemma₂ {sum} = refl
lemma₂ {diff} = refl
lemma₂ {prod} = refl

compiler-correctness₁ : ∀ {e : Exp} {s : Stack} → run-vm (compile e) s ≡ just (interpreter e ∷ s)
compiler-correctness₁ {const n} = refl
compiler-correctness₁ {exop op x y} {s}
  rewrite
--    run-vm (compile (exop op x y)) s ≡ just (interpreter (exop op x y) ∷ s)
--
--    run-vm (compile y ++ compile x ++ [ binop (exop-binop op) ]) s ≡ just (prim-ex op (interpreter x) (interpreter y) ∷ s)
    lemma₁ (compile y) (compile x ++ [ binop (exop-binop op) ]) s
--    (run-vm (compile y) s >>= run-vm (compile x ++ [ binop (exop-binop op) ])) ≡ just (prim-ex op (interpreter x) (interpreter y) ∷ s)
  | compiler-correctness₁ {y} {s}
--    (just (interpreter y ∷ s) >>= run-vm (compile x ++ [ binop (exop-binop op) ])) ≡ just (prim-ex op (interpreter x) (interpreter y) ∷ s)
--    run-vm (compile x ++ [ binop (exop-binop op) ]) (interpreter y ∷ s) ≡ just (prim-ex op (interpreter x) (interpreter y) ∷ s)
  | lemma₁ (compile x) [ binop (exop-binop op) ] (interpreter y ∷ s)
--    (run-vm (compile x) (interpreter y ∷ s) >>= run-vm [ binop (exop-binop op) ]) ≡ just (prim-ex op (interpreter x) (interpreter y) ∷ s)
  | compiler-correctness₁ {x} {interpreter y ∷ s}
--    (just (interpreter x ∷ interpreter y ∷ s) >>= run-vm [ binop (exop-binop op) ]) ≡ just (prim-ex op (interpreter x) (interpreter y) ∷ s)
--    just (prim-op (exop-binop op) (interpreter x) (interpreter y) ∷ s) ≡ just (prim-ex op (interpreter x) (interpreter y) ∷ s)
  | lemma₂ {op}
  = refl

compiler-correctness : ∀ (e : Exp) → run-vm (compile e) [] ≡ just [ interpreter e ]
compiler-correctness e = compiler-correctness₁ {e} {[]}

-- Which algorithm is it using when executed "interpreter" or "compile then
-- run-vm"?
-- Well, if I understand correctly. "from-just" will return the value
-- contained in "just" and "subst" will pass the values untouched. Which
-- would mean that there is an associated cost in proving stuff but the code
-- that is being used is "compile then run-vm"
run-vm∘compile : Exp → Maybe ℕ
run-vm∘compile e =
  let
    justln = from-just (run-vm (compile e) [])
    list = subst From-just (compiler-correctness e) justln
  in head list

_ : run-vm∘compile (const 5) ≡ just 5
_ = refl

reduce : {A : Set} → Maybe (List A) → Maybe A
reduce (just (x ∷ _)) = just x
reduce _ = nothing

_ : reduce (just [ 5 ]) ≡ just 5
_ = refl


-- Look, it is possible to execute compile and run-vm and get an unwrapped result
-- For an even prettier proof look at the end of the file
run-vm∘compile` : Exp → ℕ
run-vm∘compile` e =
  let
    computing = reduce (run-vm (compile e) [])
    justn = from-just computing
    redruncomp≡jinter = cong reduce (compiler-correctness e) -- reduce (run-vm (compile e) []) ≡ just (interpreter e)
  in subst From-just redruncomp≡jinter justn

_ : run-vm∘compile` (const 5) ≡ 5
_ = refl

---- So. I wanted to generalise the approach used before, but I'm stuck. Agda
---- seems not able to infer some variable.
---- It's because it is never using 'g', I think
---- Ans: NO. IT WAS BECAUSE x WAS IMPLICIT IN eq
--
--unwrap : {A B : Set}
--       → {g : A → B}
--       → (f : A → Maybe (List B))
--       → ({x : A} → f x ≡ just [ g x ])   -- <- This was the problem! It should be explicit!
--       --------------------------------
--       → A → B  -- Computed using f
--unwrap {A} {B} {g} f eq x =
--  let
--    computing = reduce (f x)  -- type: Maybe B
--    justb = from-just computing    -- type: From-just (f x)
--  in subst From-just new-eq justb -- type: B      -- thanks to the magic of From-just
--
--  where
--    new-eq : {x : A} → reduce (f x) ≡ just (g x)
--    new-eq {x} rewrite eq {x} = refl
--
---- Why doesn't this work!? Ans: See above or below
----run-vm∘compile`` : Exp → ℕ
----run-vm∘compile`` = unwrap {Exp} {ℕ}
----                          {interpreter}
----                          (λ e → run-vm (compile e) [])
----                          compiler-correctness

unwrap : {A B : Set} {g : A → B}
       → (f : A → Maybe (List B))
       → ((x : A) → f x ≡ just [ g x ])
       --------------------------------
       → A → B  -- Computed using f not g (g might still be run to prove the property correct)
unwrap f eq x =
  let
    -- Proof: f x ≡ just [ g x ]
    fx≡mlgx = eq x
    -- New proof: reduce (f x) ≡ just (g x)
    redfx≡gx = cong reduce {x = f x} fx≡mlgx
    compute = reduce (f x)          -- type: Maybe B
    justredfx = from-just compute       -- type: From-just (reduce (f x))  -- lifting to the type level, so we can prove stuff
  in subst From-just redfx≡gx justredfx -- type: From-just (just (g x))    which is the same as  `B`      -- thanks to the magic of From-just

--from-just (just 3)   -- evaluates to: 3
--                     -- with type:  From-just (just 3)
--                                 -- which evaluates to: ℕ
-- ie,
-- from-just (just 3) : From-just (just 3)
-- from-just (just 3) : ℕ
--                  3 : ℕ
--                  3 : From-just (just 3)

_ : from-just (just 3) ≡ 3
_ = refl

_ : From-just (just 3) ≡ ℕ
_ = refl

_ : Set
_ = From-just (just 3)

_ : From-just (just 3)  -- same as ℕ
_ = 3
_ = from-just (just 3)
_ = 2 -- This is an ℕ too

run-vm∘compile`` : Exp → ℕ
run-vm∘compile`` = unwrap ((flip run-vm []) ∘ compile) compiler-correctness
--run-vm∘compile`` = unwrap (λ e → run-vm (compile e) []) compiler-correctness

--LESSON LEARNT:
--Don't use implicit paramaters unless it makes for a clearer proof.
--Always use explicit parameters and make them implicit as you use the
--functions in more places (without breaking anything)

_ : run-vm∘compile`` (const 5) ≡ 5
_ = refl

I-am-a-number : ℕ
I-am-a-number = run-vm∘compile`` (exop sum (exop diff (const 3) (const 2)) (const 20))
