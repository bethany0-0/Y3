-- {-# OPTIONS --no-termination-check  #-}

module Notes where

-- Week 2 Lecture 2

-- Basic

data ⊤ : Set where
  • : ⊤

data ⊥ : Set where

-- Booleans

data Bool : Set where
  true  : Bool
  false : Bool

not : Bool → Bool
not true  = false
not false = true

_&_ : Bool → Bool → Bool
_&_ true true = true
_&_ _ _ = false

infixr 6 _&_

_||_ : Bool → Bool → Bool
_||_ false false = false
_||_ _ _ = true

infixr 5 _||_

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
--_+_ : Nat → Nat → Nat
--zero + y = y
--(suc x) + y = suc (x + y)

-- 2. Implement concatentation _++_ for lists
_++_ : {A : Set} → List A → List A → List A
_++_ nil list = list
_++_ (x ∷ x') y = x ∷ (x' ++ y) 

-- 3. Show that List ⊤ and Nat are isomorphic (by implementing bijections between the two types)

f : List ⊤ → Nat
f nil = zero
f (• ∷ xs) = suc (f xs)

g : Nat → List ⊤
g zero = nil
g (suc n) = • ∷ (g n)

-- 4. Show that List ⊥ and ⊤ are isomorphic (by implementing bijections between the two types)

h : List ⊥ → ⊤
h xs = •

i : ⊤ → List ⊥
i • = nil 

-- TUTORIAL PROGRAMMING TASKS

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infixl 2 _,_

_<_ : Nat → Nat → Bool
zero < zero       = false
zero < (suc n)    = true
(suc m) < zero    = false
(suc m) < (suc n) = m < n 

if_then_else_ : {A : Set} → Bool → A → A → A
if true then x else y = x
if false then x else y = y 

insert : Nat → List Nat → List Nat
insert m nil = m ∷ nil
insert m (n ∷ ns) = if  m < n then m ∷ n ∷ ns else (n ∷ insert m ns)

insert-sort : List Nat → List Nat
insert-sort nil = nil
insert-sort (x ∷ xs) = insert x (insert-sort xs) 

xs = insert-sort (₀ ∷ ₁ ∷ ₀ ∷ nil)

-- Merge sort

fst : {A B : Set} → A × B → A
fst (a , _) = a

snd : {A B : Set} → A × B → B
snd (_ , b) = b

split : {A : Set} → List A → (List A × List A)
split nil = nil , nil
split (x ∷ nil) = x ∷ nil , nil
split (x ∷ x' ∷ xs) = x ∷ (fst (split xs)) , x' ∷ (snd (split xs))  

merge : List Nat × List Nat → List Nat
merge (nil , nil) = nil
merge (nil , ys) = ys
merge (xs , nil) = xs
merge (x ∷ xs , y ∷ ys) = if x < y
                          then x ∷ merge (xs , (y ∷ ys))
                          else (y ∷ merge (x ∷ xs , ys)) 

_▷_ : {A B : Set} → A → (A → B) → B
x ▷ f = f x

infixl 7 _▷_ 

_⊗_ : {A B C D : Set} → (A → B) → (C → D) → (A × C) → (B × D)
(f ⊗ g) (x , y) = f x , g y  

infixl 7 _⊗_

--merge-sort : List Nat → List Nat
--merge-sort xs = xs ▷ split ▷ (merge-sort ⊗ merge-sort) ▷ merge


--WEEK 3 : INDUCTIVE PROOFS AND EQUALITY

-- Equalities at types?

_=⟨Nat⟩_ : Nat → Nat → Bool
zero  =⟨Nat⟩ zero  = true
zero  =⟨Nat⟩ suc n = false
suc m =⟨Nat⟩ zero  = false
suc m =⟨Nat⟩ suc n = m =⟨Nat⟩ n 

_+_ : Nat → Nat → Nat
zero  + n = n
suc m + n = m + suc n 

infixr 10 _+_

_+'_ : Nat → Nat → Nat
zero  +' n = n
suc m +' n = suc (m + n)

infixr 10 _+'_

-- What to prove they are the same
-- +=+' : (m n : Nat) → m + n =⟨Nat⟩ m + n'
-- +=+' m n = ? 
-- But propositions must be TYPES and =⟨Nat⟩ is not a TYPE

data _≡⟨Nat⟩_ : Nat → Nat → Set where
  zero-eq : zero ≡⟨Nat⟩ zero
  ind-eq  : ∀ m n → m ≡⟨Nat⟩ n → suc m ≡⟨Nat⟩ suc n

infix 5 _≡⟨Nat⟩_

-- Equality of + and +'

refl⟨Nat⟩ : ∀ n → n ≡⟨Nat⟩ n
refl⟨Nat⟩ zero = zero-eq
refl⟨Nat⟩ (suc n) = ind-eq n n (refl⟨Nat⟩ n) 

prop2 : ∀ m n → m + suc (suc n) ≡⟨Nat⟩ suc (m + suc n)
prop2 zero n = ind-eq (suc n) (suc n) (ind-eq n n (refl⟨Nat⟩ n))
prop2 (suc m) n = prop2 m (suc n)

prop1 : ∀ m →  m + suc zero ≡⟨Nat⟩ suc (m + zero)
prop1 zero = ind-eq zero zero zero-eq
prop1 (suc m) = prop2 m zero 

+=+' : ∀ m n → m + n ≡⟨Nat⟩ m +' n
+=+' zero zero = zero-eq
+=+' zero (suc n) = ind-eq n n (+=+' zero n)
+=+' (suc m) zero = prop1 m
+=+' (suc m) (suc n) = prop2 m n

-- Equality as an INDEXED type

data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x

-- Sanity checks : pattern-matching on proofs
prop3 : ∀ m n  → m ≡ n → m ≡⟨Nat⟩ n
prop3 m .m refl = refl⟨Nat⟩ m

cong-suc : ∀ m n → m ≡ n → suc m ≡ suc n
cong-suc m .m refl = refl 

prop4 : ∀ m n → m ≡⟨Nat⟩ n → m ≡ n
prop4 .zero .zero zero-eq = refl
prop4 .(suc m) .(suc n) (ind-eq m n p) = Goal where
  IH : m ≡ n
  IH = prop4 m n p
  Goal : suc m ≡ suc n
  Goal = cong-suc m n IH 

-- basic properties of equality

trans-≡ : {A : Set} → (a b c : A) → a ≡ b → b ≡ c → a ≡ c
trans-≡ a .a .a refl refl = refl

sym-≡ : {A : Set} → (a b : A) → a ≡ b → b ≡ a
sym-≡ a .a refl = refl

cong-≡ : {A B : Set} → (a b : A) (f : A → B) → a ≡ b → f a ≡ f b
cong-≡ a .a p refl = refl

-- Uniqueness of proof is a basic ingredient of pattern matching [research in progress]

uip : {A : Set} → (a b : A) → (p : a ≡ b) → (q : a ≡ b) → p ≡ q
uip a .a refl refl = refl

-- type required from bottom goal in uip is type of this, must also include hypothesis as type which is p ≡ q
goal : ∀ m n → (p : m ≡⟨Nat⟩ n) → (q : m ≡⟨Nat⟩ n) → (p ≡ q) → (ind-eq m n p) ≡ (ind-eq m n q)
goal m n p .p refl = refl

uip⟨Nat⟩ : ∀ m n → (p : m ≡⟨Nat⟩ n) → (q : m ≡⟨Nat⟩ n) → p ≡ q
uip⟨Nat⟩ .zero .zero zero-eq zero-eq = refl
uip⟨Nat⟩ .(suc m) .(suc n) (ind-eq m n p) (ind-eq .m .n q) = Goal where
  IH : p ≡ q
  IH = uip⟨Nat⟩ m n p q
  Goal : (ind-eq m n p) ≡ (ind-eq m n q)
  Goal = goal m n p q IH 

-- WEEK 3 LECTURE 2 LAB TASK
-- PROVE THAT LIST CONCATENATION IS A MONOID

id1 : {A : Set} → (xs : List A) → ((nil ++ xs) ≡ xs)
id1 a = refl

--Oi oi
id2 : {A : Set} → (xs : List A) → xs ++ nil ≡ xs
id2 nil = refl
id2 (x ∷ xs) = cong-≡ (xs ++ nil) xs (_∷_ x) (id2 xs)

assoc-[] : {A : Set} → (x y z : List A) → ((x ++ y) ++ z) ≡ (x ++ (y ++ z))
assoc-[] nil _ _ = refl
assoc-[] (x ∷ xs) y      z = cong-≡ LHS RHS (_∷_ x) IH where
  IH : (xs ++ y) ++ z ≡ xs ++ (y ++ z)
  IH = assoc-[] xs y z
  LHS = (xs ++ y) ++ z
  RHS = xs ++ (y ++ z)


--different way !!!!!!
assoc : {A : Set} → (a b c : List A) → (a ++ b) ++ c ≡ a ++ (b ++ c)
assoc nil b c = refl
assoc (x ∷ a) b c = Goal where
    IH : (a ++ b) ++ c ≡ a ++ (b ++ c)
    IH = assoc a b c
    Goal : ((x ∷ a) ++ b) ++ c ≡ (x ∷ a) ++ (b ++ c)
    Goal = cong-≡ ((a ++ b) ++ c) (a ++ (b ++ c)) (_∷_ x) IH

--different different way
assoc2 : {A : Set} → (xs ys zs : List A) → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
assoc2 nil _ _ = refl
assoc2 (x ∷ xs) ys zs = cong-≡ (xs ++ ys ++ zs) (xs ++ (ys ++ zs)) (_∷_ x) (assoc2 xs ys zs)

-- different way that explains “_::_ x” slightly better worse
assoc-++ : {A : Set} → (x y z : List A) → (((x ++ y) ++ z) ≡ (x ++ (y ++ z)))
assoc-++ nil _ _ = refl
assoc-++ (x ∷ xs) y z = Goal where
  IH : (xs ++ y) ++ z ≡ xs ++ (y ++ z)
  IH = assoc-++ xs y z
  Goal : x ∷ ((xs ++ y) ++ z) ≡ x ∷ (xs ++ (y ++ z))
  Goal = cong-≡ ((xs ++ y) ++ z) (xs ++ (y ++ z)) (λ ■ → x ∷ ■) IH
-- what’s the square ■?
-- It’s a hole between the goal and the inductive proof. So it’s like proving it for xs and then adding on x (I THINK) - the square could be another symbol like ☆ :)

