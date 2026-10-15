{-# OPTIONS --guardedness #-}

open import Data.Vec using (Vec ; _∷_ ; [])
open import Data.Nat using (ℕ ; zero ; suc ; _<ᵇ_)
open import Data.String using (String ; _++_)
open import Data.Bool using (Bool ; true ; false)
open import Data.Maybe using (Maybe ; just ; nothing)
open import Function using (_$_)
open import IO

data So : Bool → Set where
  oh : So true

nth : {n : ℕ} (v : Vec String n) (m' : ℕ) → So (m' <ᵇ n) → String
nth (x ∷ _)  zero    _ = x
nth (_ ∷ xs) (suc m) p = nth xs m p

example : Vec String 5
example = "a" ∷ "b" ∷ "c" ∷ "d" ∷ "e" ∷ []

third : String
third = nth example 2 oh

-- Compiler error
-- seventh : String
-- seventh = nth example 6 oh

decide : (b : Bool) → Maybe (So b)
decide true  = just oh
decide false = nothing

nth? : {n : ℕ} (xs : Vec String n) (m : ℕ) → Maybe String
nth? {n} xs m with decide (m <ᵇ n)
... | just p  = just $ nth xs m p
... | nothing = nothing

showMaybe : Maybe String → String
showMaybe (just s) = "just " ++ s
showMaybe nothing  = "nothing"

main : Main
main = run do
  putStrLn third
  -- putStrLn seventh
  putStrLn $ showMaybe $ nth? example 2
  putStrLn $ showMaybe $ nth? example 7
