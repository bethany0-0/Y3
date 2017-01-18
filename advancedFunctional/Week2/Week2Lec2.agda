module Week2Lec2 where

-- Basic

data ⊤ : Set where
  • : ⊤ 

data ⊥ : Set where

-- Booleans

data Bool : Set where
  true  : Bool
  false : Bool

not : Bool → Bool
not true = false
not false = true

_&_ : Bool → Bool → Bool
_&_ true true = true
_&_ _ _ = false

infixr 6 _&_

_||_ : Bool → Bool → Bool
_||_ false false = false
_||_ _ _ = true

infixr 5 _||_

if_then_else_ : Bool → Bool → Bool → Bool
if true then x else _ = x
if false then _ else y = y

-- Natural numbers

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

₀ = zero
₁ = suc ₀
₂ = suc ₁
₃ = suc ₂  

-- Lists 

-- Note that the data type is parameterised by a type A
data List (A : Set) : Set where 
  nil : List A
  _∷_ : A → List A → List A

infixr 4 _∷_

l : List Nat
l = ₀ ∷ ₁ ∷ ₂ ∷ nil

l' : List (List Nat)
l' = (₀ ∷ ₂ ∷ nil) ∷ l ∷ nil

-- TASKS IN CLASS

-- 1. Implement addition _+_ on natural numbers

_+_ : Nat → Nat → Nat
zero + y = y
(suc x) + y = suc (x + y) 

-- 2. Implement concatentation _++_ for lists

_++_ : {A : Set} → List A → List A → List A
_++_ nil list = list
_++_ (x ∷ x') y = x ∷ (x' ++ y) 


-- 3. Show that List ⊤ and Nat are isomorphic (by implementing bijections between the two types)
bij1 : List ⊤ → Nat
bij1 nil = zero
bij1 (x ∷ xs) = suc (bij1 xs) 

bij2 : Nat → List ⊤ 
bij2 zero = nil
bij2 (suc nat) = • ∷ (bij2 nat)

-- 4. Show that List ⊥ and ⊤ are isomorphic (by implementing bijections between the two types)

List⊥→⊤ : List ⊥ → ⊤
List⊥→⊤ nil = •
List⊥→⊤ (() ∷ _ )

⊤→List⊥ : ⊤ → List ⊥
⊤→List⊥ • = nil
