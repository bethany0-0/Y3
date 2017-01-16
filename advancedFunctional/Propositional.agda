module Propositional where

data Bool : Set where
  true : Bool
  false : Bool

not : Bool → Bool
not true = false
not false = true

_&_ : Bool → Bool → Bool
true & true = true
false & true = false
true & false = false
false & false = false

infixl 6 _&_

--define blanks at front as below to allow for _ in definition

_||_ : Bool → Bool → Bool
_||_ false false = false
_||_ _  _ = true

infixl 5 _||_   

_xor_ : Bool → Bool → Bool
_xor_ false false = false
_xor_ true true = false
_xor_ _ _ = true

infixl 5 _xor_

data ⊤ : Set where
  tt : ⊤

data ⊥ : Set where 

-- _,_ = pair 

data _∧_ (A B : Set) : Set where
   _,_ : A → B → A ∧ B

data _∨_ (A B : Set) : Set where
  inl : A → A ∨ B 
  inr : B → A ∨ B

infixl 5 _∨_

-- Hilbert-style laws

then-1 : {A B : Set} → A → B → A
then-1 a _ = a

then-2 : {A B C : Set} → (A → B → C) → (A → B) → (A → C)
then-2 f g a = f a (g a) 

and-1 : {A B : Set} → A ∧ B → A
and-1 (a , _) = a

and-2 : {A B : Set} → A ∧ B → B
and-2 (_ , b) = b

and-3 : {A B : Set} → A → B → A ∧ B
and-3 a b = a , b

or-1 : {A B : Set} → A → A ∨ B
or-1 a = inl a

or-2 : {A B : Set} → B → A ∨ B
or-2 b = inr b

-- NOTE: PATTERN MATCHING
or-3 : {A B C : Set} → (A → C) → (B → C) → (A ∨ B → C) 
or-3 f g (inl x) = f x
or-3 f g (inr x) = g x

falsity : {A : Set} → ⊥ → A 
falsity = λ () -- the empty pattern

-- NEGATION

¬ : Set → Set 
¬ A = A → ⊥

not-1 : {A B : Set} → (A → B) → (A → ¬ B) → ¬ A
not-1 p q a = q a (p a) 

not-2 : {A B : Set} → A → ¬ A → B
not-2 a p = falsity (p a)

-- CLASSICAL LOGIC

-- Excluded middle implies double negation

postulate LEM : {A : Set} → A ∨ ¬ A

classical-1 : {A : Set} → ¬ (¬ A) → A
classical-1 f = or-3 (λ z → z) (λ a' → falsity (f a')) LEM

-- Double negation implies exclude middle

postulate DNE : {A : Set} → ¬ (¬ A) → A

classical-2 : {A : Set} → A ∨ ¬ A
classical-2 = DNE (λ z → z (inr (λ x → z (inl x))))

-- Contrapositive 

-- Note auto
contrapos : {A B : Set} → (A → B) → ¬ B → ¬ A
contrapos = λ z z₁ z₂ → z₁ (z z₂) 

-- Note auto hints
contrapos' : {A B : Set} → (¬ A → ¬ B) → B → A
contrapos' = λ z z₁ → DNE (λ z₂ → z z₂ z₁) 


