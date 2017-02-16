-- {-# OPTIONS --no-termination-check  #-}

module Tutorial3 where

-- Week 4


-- Lists 

-- Note that the data type is parameterised by a type A
data List (A : Set) : Set where 
  nil : List A
  _∷_ : A → List A → List A

infixr 4 _∷_


-- TASKS IN CLASS

-- 2. Implement concatentation _++_ for lists
_++_ : {A : Set} → List A → List A → List A
_++_ nil list = list
_++_ (x ∷ x') y = x ∷ (x' ++ y) 

infix 5 _++_

-- TUTORIAL PROGRAMMING TASKS

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infixl 2 _,_
 


-- Merge sort

fst : {A B : Set} → A × B → A
fst (a , _) = a

snd : {A B : Set} → A × B → B
snd (_ , b) = b

split : {A : Set} → List A → (List A × List A)
split nil = nil , nil
split (x ∷ nil) = x ∷ nil , nil
split (x ∷ x' ∷ xs) = x ∷ (fst (split xs)) , x' ∷ (snd (split xs))  


_▷_ : {A B : Set} → A → (A → B) → B
x ▷ f = f x

infixl 7 _▷_ 

_⊗_ : {A B C D : Set} → (A → B) → (C → D) → (A × C) → (B × D)
(f ⊗ g) (x , y) = f x , g y  

infixl 7 _⊗_

--merge-sort : List Nat → List Nat
--merge-sort xs = xs ▷ split ▷ (merge-sort ⊗ merge-sort) ▷ merge


--WEEK 3 : INDUCTIVE PROOFS AND EQUALITY

-- Equality of + and +'


-- Equality as an INDEXED type

data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x



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


-- WEEK 3 LECTURE 2 LAB TASK
-- PROVE THAT LIST CONCATENATION IS A MONOID

id1 : {A : Set} → (xs : List A) → ((nil ++ xs) ≡ xs)
id1 a = refl

--Oi oi
id2 : {A : Set} → (xs : List A) → ((xs ++ nil) ≡ xs)
id2 nil = refl
id2 (x ∷ xs) = cong-≡ (xs ++ nil) xs (_∷_ x) (id2 xs)

assoc-[] : {A : Set} → (x y z : List A) → ((x ++ y) ++ z) ≡ (x ++ (y ++ z))
assoc-[] nil _ _ = refl
assoc-[] (x ∷ xs) y      z = cong-≡ LHS RHS (_∷_ x) IH where
  IH : ((xs ++ y) ++ z) ≡ (xs ++ (y ++ z))
  IH = assoc-[] xs y z
  LHS = (xs ++ y) ++ z
  RHS = xs ++ (y ++ z)


--different way !!!!!!
assoc : {A : Set} → (a b c : List A) → ((a ++ b) ++ c) ≡ (a ++ (b ++ c))
assoc nil b c = refl
assoc (x ∷ a) b c = Goal where
    IH : ((a ++ b) ++ c) ≡ (a ++ (b ++ c))
    IH = assoc a b c
    Goal : (((x ∷ a) ++ b) ++ c) ≡ ((x ∷ a) ++ (b ++ c))
    Goal = cong-≡ ((a ++ b) ++ c) (a ++ (b ++ c)) (_∷_ x) IH

--different different way
assoc2 : {A : Set} → (xs ys zs : List A) → ((xs ++ ys) ++ zs) ≡ (xs ++ (ys ++ zs))
assoc2 nil _ _ = refl
assoc2 (x ∷ xs) ys zs = cong-≡ ((xs ++ ys) ++ zs) (xs ++ (ys ++ zs)) (_∷_ x) (assoc2 xs ys zs)

-- different way that explains “_::_ x” slightly better worse
assoc-++ : {A : Set} → (x y z : List A) → (((x ++ y) ++ z) ≡ (x ++ (y ++ z)))
assoc-++ nil _ _ = refl
assoc-++ (x ∷ xs) y z = Goal where
  IH : ((xs ++ y) ++ z) ≡ (xs ++ (y ++ z))
  IH = assoc-++ xs y z
  Goal : (x ∷ ((xs ++ y) ++ z)) ≡ (x ∷ (xs ++ (y ++ z)))
  Goal = cong-≡ ((xs ++ y) ++ z) (xs ++ (y ++ z)) (λ ■ → x ∷ ■) IH
-- what’s the square ■?
-- It’s a hole between the goal and the inductive proof. So it’s like proving it for xs and then adding on x (I THINK) - the square could be another symbol like ☆ :)




----------------------Tutorial 3

rev : {A : Set} → List A → List A
rev nil = nil
rev (x ∷ xs) = (rev xs) ++ (x ∷ nil)

rev++-≡ : {A : Set} → (x : A) → (ys : List A) → (x ∷ (rev ys)) ≡ (rev ( ys ++ (x ∷ nil)))
rev++-≡ x nil = refl
rev++-≡ x (y ∷ ys) = cong-≡ (x ∷ rev ys) (rev (ys ++ (x ∷ nil))) (λ z → z ++ (y ∷ nil) ) (rev++-≡ x ys)

rev≡rev : {A : Set} → (xs : List A) → xs ≡ rev(rev(xs))
rev≡rev nil = refl
rev≡rev (x ∷ xs) = cong-≡ xs (rev(rev(xs))) {!rev++-≡!} (rev≡rev xs)

