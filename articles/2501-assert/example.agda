module example where

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

infix 4 _≡_

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

data _<_ : ℕ → ℕ → Set where
  z<s : {n : ℕ} → zero < suc n
  s<s : {n m : ℕ} → n < m → suc n < suc m

infix 4 _<_

_+_ : ℕ → ℕ → ℕ
zero  + m = m
suc n + m = suc (n + m)

infixl 6 _+_

subst : {A : Set} {x y : A} (P : A → Set) → x ≡ y → P x → P y
subst P refl px = px

sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

+-suc : (n m : ℕ) → n + suc m ≡ suc (n + m)
+-suc zero    m = refl
+-suc (suc n) m = subst (λ k → suc n + suc m ≡ suc k) (+-suc n m) refl

1<suc-n+suc-m : {n m : ℕ} → suc zero < suc n + suc m
1<suc-n+suc-m {n} {m} = s<s (subst (λ k → zero < k) (sym (+-suc n m)) z<s)

data 𝕍 (A : Set) : ℕ → Set where
  []  : 𝕍 A zero
  _∷_ : {n : ℕ} → A → 𝕍 A n → 𝕍 A (suc n)

infixr 5 _∷_

_++_ : {A : Set} {n m : ℕ} → 𝕍 A n → 𝕍 A m → 𝕍 A (n + m)
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

infixr 5 _++_

nth : {A : Set} {n : ℕ} → 𝕍 A n → (i : ℕ) → i < n → A
nth []       i       ()
nth (x ∷ xs) zero    (z<s)     = x
nth (x ∷ xs) (suc i) (s<s i<n) = nth xs i i<n

-- Takes two non-empty vectors and returns the second element of their concatenation
second : {A : Set} {n m : ℕ} → 𝕍 A (suc n) → 𝕍 A (suc m) → A
second {A} {n} {m} vec1 vec2 =
  let one = suc zero
      concatenated : 𝕍 A (suc n + suc m)
      concatenated = vec1 ++ vec2
      -- Proof that index 1 is in bounds
      index-proof : one < suc n + suc m
      index-proof = 1<suc-n+suc-m
  in nth concatenated one index-proof

example1 : 𝕍 ℕ 3
example1 = 10 ∷ 20 ∷ 30 ∷ []

example2 : 𝕍 ℕ 2
example2 = 100 ∷ 200 ∷ []

1<3 : suc zero < 3
1<3 = s<s z<s

test1 : ℕ
test1 = nth example1 (suc zero) 1<3

test2 : ℕ
test2 = second example1 example2

test3 : ℕ
test3 = second (5 ∷ []) (7 ∷ 9 ∷ [])
