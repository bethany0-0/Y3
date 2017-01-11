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
