-- http://williamdemeo.github.io/2014/02/27/learn-you-an-agda/

module IPL where

data _∧_ (P : Set) (Q : Set) : Set where
  ∧-intro : P → Q → (P ∧ Q)

proof₁ : {P Q : Set} → (P ∧ Q) → P
proof₁ (∧-intro p q) = p

proof₂ : {P Q : Set} -> (P ∧ Q) -> Q
proof₂ (∧-intro p q) = q

_⇔_ : (P : Set) → (Q : Set) → Set
a ⇔ b = (a → b) ∧ (b → a)

∧-comm′ : {P Q : Set} → (P ∧ Q) → (Q ∧ P)
∧-comm′ (∧-intro p q) = ∧-intro q p

∧-comm : {P Q : Set} → (P ∧ Q) ⇔ (Q ∧ P)
∧-comm = ∧-intro ∧-comm′ ∧-comm′

∧-assoc₁ : {P Q R : Set} → ((P ∧ Q) ∧ R) → (P ∧ (Q ∧ R))
∧-assoc₁ (∧-intro (∧-intro p q) r) = ∧-intro p (∧-intro q r)

∧-assoc₂ : {P Q R : Set} → (P ∧ (Q ∧ R)) → ((P ∧ Q) ∧ R)
∧-assoc₂ (∧-intro p (∧-intro q r)) = ∧-intro (∧-intro p q) r

∧-assoc : {P Q R : Set} → ((P ∧ Q) ∧ R) ⇔ (P ∧ (Q ∧ R))
∧-assoc = ∧-intro ∧-assoc₁ ∧-assoc₂

data _∨_ (P Q : Set) : Set where
  ∨-intro₁ : P → P ∨ Q
  ∨-intro₂ : Q → P ∨ Q

∨-elim : {A B C : Set} → (A → C) → (B → C) → (A ∨ B) → C
∨-elim ac bc (∨-intro₁ a) = ac a
∨-elim ac bc (∨-intro₂ b) = bc b

∨-comm′ : {P Q : Set} → (P ∨ Q) → (Q ∨ P)
∨-comm′ (∨-intro₁ p) = ∨-intro₂ p
∨-comm′ (∨-intro₂ q) = ∨-intro₁ q

∨-comm : {P Q : Set} → (P ∨ Q) ⇔ (Q ∨ P)
∨-comm = ∧-intro ∨-comm′ ∨-comm′

data ⊥ : Set where

¬ : Set → Set
¬ A = A → ⊥
