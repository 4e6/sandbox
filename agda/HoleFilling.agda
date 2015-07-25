-- http://williamdemeo.github.io/2014/02/27/learn-you-an-agda/

module HoleFilling where

data Bool : Set where
  false : Bool
  true : Bool

∧ : Bool → Bool → Bool
∧ false false = false
∧ true false = false
∧ false true = false
∧ true true = true
