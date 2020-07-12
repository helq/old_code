open import Data.Bool using (Bool; true; false)
open import Data.Float using (Float)
open import Avionics.Maybe using (Maybe; just; nothing; _>>=_; map)
--open import Data.Maybe using (Maybe; just; nothing; _>>=_; map)
open import Data.Nat using (ℕ)
open import Function using (_∘_)
open import Level using (0ℓ; _⊔_) renaming (suc to lsuc)
open import Relation.Binary using (Decidable)
open import Relation.Binary.Definitions using (Transitive; Trans)
open import Relation.Binary.PropositionalEquality using (_≡_)
open import Relation.Nullary using (Dec; yes; no)
open import Relation.Nullary.Decidable using (False; ⌊_⌋; True)
open import Relation.Unary using (Pred; _∈_)

-- stack exec -- agda -c --ghc-dont-call-ghc --no-main code.agda

infix  4 _≤_ _≤ᵇ_ _≤?_

postulate
  ℝ : Set

  fromFloat : Float → Maybe ℝ
  toFloat : ℝ → Float

  0ℝ : ℝ

  _≤_ : ℝ → ℝ → Set
  _≤?_ : (m n : ℝ) → Dec (m ≤ n)

_≤ᵇ_ : ℝ → ℝ → Bool
p ≤ᵇ q = ⌊ p ≤? q ⌋

postulate
  √_ : (x : ℝ) → .{0≤x : True (0ℝ ≤? x)} → ℝ

--Subset : Set → Set _
--Subset A = Pred A 0ℓ
--
--postulate
--  [0,∞⟩ : Subset ℝ
--
--  √_ : (x : ℝ) → .{0≤x : x ∈ [0,∞⟩} → ℝ


{-# COMPILE GHC ℝ = type Double #-}

-- `fromFloat` should check whether it is a valid floating point
-- number
{-# COMPILE GHC fromFloat = \x -> Just x #-}
{-# COMPILE GHC toFloat = \x -> x #-}

{-# COMPILE GHC 0ℝ = 0 #-}

{-# FOREIGN GHC type UnitErase a b = () #-}
{-# COMPILE GHC _≤_ = type UnitErase #-}
--{-# COMPILE GHC _≤_ = \ _ _ -> () #-}
{-# COMPILE GHC _≤ᵇ_ = (<=) #-}

--{-# FOREIGN GHC type UnitErase erase = () #-}
--{-# COMPILE GHC [0,∞⟩ = type UnitErase #-}

-- TODO: Find out how to perform the `sqrt` of a value!!
--       Here lies the main problem of compilation failure!
{-# COMPILE GHC √_  = \x _ -> sqrt x #-}
-- Agda is actually doing the right thing not accepting dubious
-- translations of code into Haskell


export√ : Float → Maybe Float
export√ x = fromFloat x >>= sqrt >>= just ∘ toFloat
  where
    sqrt : ℝ → Maybe ℝ
    sqrt y with 0ℝ ≤ᵇ y
    ... | false = nothing
    ... | true = just ((√ y) {y≥0})
      where postulate y≥0 : True (0ℝ ≤? y)
      --where postulate y≥0 : y ∈ [0,∞⟩
{-# COMPILE GHC export√ as exportSqrt #-}
