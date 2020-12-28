open import Data.Bool using (Bool; false; true; _∧_)
open import Data.Maybe
open import Data.Nat
open import Data.Nat.Properties --using (_≟_; _<?_)
open import Data.Product
open import Relation.Binary.PropositionalEquality --using (_≡_; refl)
open import Relation.Nullary --using (Dec; yes; no)
--open import Relation.Nullary.Decidable --using (from-yes)

_ : Dec (0 ≡ 2)
_ = no (λ())
--_ = 0 ≟ 2
--_ = 0 ≟ 3

_ : Dec (2 ≡ 2)
_ = yes refl

fun : ℕ → ℕ → Maybe ℕ
fun x y with x <? y
... | yes _ = nothing
... | no _  = just y

_ : fun 3 5 ≡ nothing
_ = refl

_ : fun 3 2 ≡ just 2
_ = refl

--prop-:S : ∀ (x y)
--        → fun x y ≡ nothing
--        → x < y
--prop-:S 0 0 ()
--prop-:S 0 (suc y) refl = s≤s z≤n
--prop-:S (suc x) 0 ()
--prop-:S (suc x) (suc y) funsxsy≡nothing = s≤s (prop-:S x y ?)

--prop : ∀ (x y)
--     → fun x y ≡ nothing
--     → x < y
--prop x y with fun x y
--... | just _  = λ()
--... | nothing = λ{refl → ?} -- from-yes (x <? y)}
--
---- This fails because the pattern matching is incomplete,
---- but it shouldn't. There are no other cases
--prop' : ∀ (x y)
--      → fun x y ≡ nothing
--      → x < y
--prop' x y with fun x y | x <? y
--... | nothing | yes x<y = λ{refl → x<y}
--... | just _  | no _    = λ()
--... | _ | _ = ?

-- From: https://stackoverflow.com/a/63119870/1744344
prop : ∀ x y → fun x y ≡ nothing → x < y
prop x y with x <? y
... | yes p = λ _ → p

prop-old : ∀ x y → fun x y ≡ nothing → x < y
prop-old x y _ with x <? y
prop-old _ _ refl | yes p = p
prop-old _ _ () | no _

data Comp : Set where
  greater equal less : Comp

compare' : ℕ → ℕ → Comp
compare' x y with x <ᵇ y
...          | false with y <ᵇ x
...                  | false = equal
...                  | true  = greater
compare' x y | true = less

--fun2 : Maybe (ℕ × ℕ) → Maybe ℕ
--fun2 pair with pair
--... | just (x , y) with x <? y
--... | just (x , y) | yes _ = nothing
--... | just (x , y) | no _  = just y
--fun2 _ | nothing = nothing

prop-comp : ∀ x y
          → compare' x y ≡ greater
          → (y <ᵇ x) ≡ true
prop-comp x y _ with x <ᵇ y
... | false with y <ᵇ x
...         | true = refl
--same as
--prop-comp x y _ | false with y <ᵇ x
--prop-comp x y _ | false | true = refl

prop-comp← : ∀ x y
           → (y <ᵇ x) ≡ true
           → compare' x y ≡ greater
prop-comp← x y _ with x <ᵇ y
prop-comp← x y _  | true  with y <ᵇ x
prop-comp← _ _ _  | true | true = ?  -- This impossible!
prop-comp← _ _ () | true | false
prop-comp← x y _  | false with y <ᵇ x
prop-comp← _ _ _  | false | true = refl
--prop-comp← x y () | false | false

prop-new← : ∀ x y
           → (x <ᵇ y) ≡ false
           → (y <ᵇ x) ≡ false
           → compare' x y ≡ equal
--Why is this not possible?
--prop-new← x y _ with x <ᵇ y | y <ᵇ x
--... | false | false = refl
prop-new← x y _ _ with x <ᵇ y
... | false with y <ᵇ x
...   | false = refl

--a : inspect 3 2
--a = ?
