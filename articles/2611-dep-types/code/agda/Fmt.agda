{-# OPTIONS --guardedness #-}

open import Agda.Builtin.String using () renaming (primShowNat to showℕ)
open import Data.Char.Base      using (Char)
open import Data.List           using (List; []; _∷_)
open import Data.Nat            using (ℕ)
open import Data.String         using (String; toList; fromChar; _++_)
open import Function            using (_$_)
open import IO                  using (Main; run; putStrLn; _>>_)

data Format : Set where
  int : Format → Format
  str : Format → Format
  lit : Char → Format → Format
  end : Format

parse : List Char → Format
parse ('%' ∷ 'd' ∷ cs) = int (parse cs)
parse ('%' ∷ 's' ∷ cs) = str (parse cs)
parse (c ∷ cs)         = lit c (parse cs)
parse []               = end

Printf : Format → Set
Printf (int f)   = ℕ → Printf f
Printf (str f)   = String → Printf f
Printf (lit _ f) = Printf f
Printf end       = String

render : (f : Format) → String → Printf f
render (int f)   acc = λ n → render f (acc ++ showℕ n)
render (str f)   acc = λ s → render f (acc ++ s)
render (lit c f) acc = render f (acc ++ fromChar c)
render end       acc = acc

cfmt : (fmt : String) → Printf (parse (toList fmt))
cfmt fmt = render (parse (toList fmt)) ""

main : Main
main = run do
  putStrLn $ cfmt "Spin of %s equals %d/2" "electron" 1
  putStrLn $ cfmt "Spin of %s equals %d/2" "Ω baryon" 3
  -- putStrLn $ cfmt "Spin of %s equals %d/2" 1 "proton" -- compiler error
  -- putStrLn $ cfmt "Spin of %s equals %d/2" "photon"   -- compiler error
