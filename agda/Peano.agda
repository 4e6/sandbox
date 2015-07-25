-- http://williamdemeo.github.io/2014/02/27/learn-you-an-agda/

module Peano where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

_+_ : ℕ -> ℕ -> ℕ
zero    + zero = zero
zero    + n    = n
(suc n) + m    = suc (n + m)

data _even : ℕ → Set where
  ZERO : zero even
  STEP : ∀ {x} → x even → suc (suc x) even

proof₁ : suc (suc (suc (suc zero))) even
proof₁ = STEP (STEP ZERO)

proof₂′ : (A : Set) → A → A
proof₂′ _ x = x

proof₂ : ℕ → ℕ
proof₂ = proof₂′ ℕ
