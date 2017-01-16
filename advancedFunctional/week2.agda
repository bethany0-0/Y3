module week2 where

-- BOOLEAN VALUES

data Bool : Set where
  true  : Bool
  false : Bool

not : Bool → Bool
not true  = true
not false = false

_&_ : Bool → Bool → Bool
_&_ true true = true
_&_ _    _    = false

infixr 6 _&_

_||_ : Bool → Bool → Bool
_||_ false false = false
_||_ _     _     = true

infixr 5 _||_

if_then_else_ : Bool → Bool → Bool → Bool
if true  then x else _ = x
if false then _ else y = y

-- PROPOSITIONAL LOGIC

data ⊤ : Set where
  tt : ⊤ 

data ⊥ : Set where

data _∧_ (A B : Set) : Set where
  _,_ : A → B → A ∧ B

infixl 6 _∧_

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


------------------------------ASSIGNMENT-------------------------------------

-- Excluded middle implies double negation

----postulate LEM : {A : Set} → A ∨ ¬ A

----classical-1 : {A : Set} → ¬ (¬ A) → A
----classical-1 f = or-3 (λ z → z) (λ a' → falsity (f a')) LEM

classical-1 : {A : Set} → (A ∨ ¬ A) → (¬ (¬ A) → A)
classical-1 (inl x) f = x
classical-1 (inr x) f = falsity (f x)


-- Double negation implies exclude middle

----postulate DNE : {A : Set} → ¬ (¬ A) → A

----classical-2 : {A : Set} → A ∨ ¬ A
----classical-2 = DNE (λ z → z (inr (λ x → z (inl x))))

classical-2 : { A : Set } → (¬ (¬ A) → A) → (A ∨ ¬ A)
classical-2 f = inl {!falsity!}
