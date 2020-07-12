module general-exercises.RunningHaskellCode where

-- To compile code: stack exec -- agda general-exercises/RunningHaskellCode.agda -c

open import Data.String using (String)
open import Agda.Builtin.Unit using (⊤)

-- Working directly with Haskell code, kinda dangerous stuff
--
-- This didn't actually work because all Agda stuff is wrapped around colists
-- and there is even more wrapping around printError to make it work. It is not
-- that it is impossible, it is just a matter of copying the code from
-- `IO.Primitive`, which I don't want to do right now
--
--open import Agda.Builtin.IO using (IO)
--
--{-# FOREIGN GHC import qualified System.IO as SIO #-}
--{-# FOREIGN GHC import qualified Data.Text #-}
--
--postulate printError : String → IO ⊤
--
--{-# COMPILE GHC printError = SIO.hPutStrLn SIO.stderr $ Data.Text.unpack #-}
--
--main = printError "Printing to stderr!"

-- Using the Standard Library
open import IO using (run; putStrLn)

main = run (putStrLn "Hello freaking World!")

-- Exporting Agda binaries to use in Haskell code
--open import Data.Nat using (ℕ; suc; zero)
--
--double : ℕ → ℕ
--double zero = zero
--double (suc n) = suc (suc (double n))
--{-# COMPILE GHC double as doubleNat #-}
