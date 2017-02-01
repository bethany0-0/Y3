module Lab4 where

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


-- 2. Implement concatentation _++_ for lists

_++_ : {A : Set} → List A → List A → List A
nil ++ ys = ys
(x ∷ xs) ++ ys = x ∷ xs ++ ys

infixl 5 _++_

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
-- +=+' : (m n : Nat) → ?
-- But propositions must be TYPES and =⟨Nat⟩ is not a TYPE

data _≡⟨Nat⟩_ : Nat → Nat → Set where
  zero-eq : zero ≡⟨Nat⟩ zero
  ind-eq  : ∀ m n → m ≡⟨Nat⟩ n → suc m ≡⟨Nat⟩ suc n

infix 5 _≡⟨Nat⟩_

-- TO DO INTERACTIVELY
-- start with prop1, notice instance of prop2
-- to prove prop2 we need refl then simplify the proof

refl⟨Nat⟩ : ∀ n → n ≡⟨Nat⟩ n
refl⟨Nat⟩ zero = zero-eq
refl⟨Nat⟩ (suc n) = ind-eq n n (refl⟨Nat⟩ n) 

suc+ : ∀ m n →  m + suc n ≡⟨Nat⟩ suc (m + n)
suc+ zero n = refl⟨Nat⟩ (suc n)
suc+ (suc m) n = suc+ m (suc n)

+=+' : ∀ m n → m + n ≡⟨Nat⟩ m +' n
+=+' zero zero = zero-eq
+=+' zero (suc n) = ind-eq n n (+=+' zero n)
+=+' (suc m) zero = suc+ m zero
+=+' (suc m) (suc n) = suc+ m (suc n)  

-- Equality as an INDEXED type

data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x

infix 3 _≡_ 

-- Sanity checks : pattern-matching on proofs
prop3 : ∀ m n  → m ≡ n → m ≡⟨Nat⟩ n
prop3 m .m refl = refl⟨Nat⟩ m 

-- try and fail to use induction by getting stuck on this:
--prop4 : ∀ m n → m ≡⟨Nat⟩ n → m ≡ n
--prop4 .zero .zero zero-eq = refl
--prop4 .(suc m) .(suc n) (ind-eq m n p) = Goal where
--  q : suc m ≡⟨Nat⟩ suc n
--  q = ind-eq m n p 
--  Goal : suc m ≡ suc n
--  Goal = prop4 (suc m) (suc n) {!q!}

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
cong-≡ a .a f refl = refl 

-- Uniqueness of proof is a basic ingredient of pattern matching [research in progress]

uip : {A : Set} → (a b : A) → (p : a ≡ b) → (q : a ≡ b) → p ≡ q
uip a .a refl refl = refl 

uip⟨Nat⟩ : ∀ m n → (p : m ≡⟨Nat⟩ n) → (q : m ≡⟨Nat⟩ n) → p ≡ q
uip⟨Nat⟩ .zero .zero zero-eq zero-eq = refl
uip⟨Nat⟩ .(suc m) .(suc n) (ind-eq m n p) (ind-eq .m .n q) = Goal where
  IH : p ≡ q
  IH = uip⟨Nat⟩ m n p q 
  Goal : ind-eq m n p ≡ ind-eq m n q
  Goal = cong-≡ p q (λ x → ind-eq m n x) IH 

-- WEEK 3 LECTURE 2 LAB TASK
-- PROVE THAT LIST CONCATENATION IS A MONOID

nil-++ : {A : Set} → (xs : List A) → nil ++ xs ≡ xs
nil-++ xs = refl

++-nil : {A : Set} → (xs : List A) → xs ++ nil ≡ xs
++-nil nil = refl
++-nil (x ∷ xs) = Goal where
  IH : xs ++ nil ≡ xs
  IH = ++-nil xs 
  Goal : x ∷ (xs ++ nil) ≡ x ∷ xs
  Goal = cong-≡ (xs ++ nil) xs (λ ■ → x ∷ ■) IH 

++-assoc : {A : Set} → (xs ys zs : List A) → xs ++ ys ++ zs ≡ xs ++ (ys ++ zs)
++-assoc nil ys zs = refl
++-assoc (x ∷ xs) ys zs = Goal where
  IH :  xs ++ ys ++ zs ≡ xs ++ (ys ++ zs)
  IH = ++-assoc xs ys zs
  Goal : x ∷ (xs ++ ys ++ zs) ≡ x ∷ (xs ++ (ys ++ zs))
  Goal = cong-≡ (xs ++ ys ++ zs) (xs ++ (ys ++ zs)) (λ ■ → x ∷ ■) IH 

-- ADVANCED TASK: IMPLEMENT LIST REVERSE AND PROVE IT IS IDEMPOTENT
-- TIP: YOU NEED A MORE GENERAL PROPERTY INVOLVING REVERSE AND APPEND
-- rev (xs ++ ys) ≡ ?????
-- WHICH YOU CAN APPLY

rev : {A : Set} → List A → List A
rev nil = nil
rev (x ∷ xs) = rev xs ++ (x ∷ nil)

rev-++ : {A : Set} → (xs ys : List A) → rev (xs ++ ys) ≡ (rev ys) ++ (rev xs)
rev-++ {A} nil ys = sym-≡ (rev ys ++ nil) (rev ys) (++-nil (rev ys))
rev-++ {A} (x ∷ xs) ys = Goal where
  IH : (ys : List A) →  rev (xs ++ ys) ≡ rev ys ++ rev xs
  IH = rev-++ xs
  p1 :  rev (xs ++ ys) ++ (x ∷ nil) ≡  rev ys ++ rev xs ++ (x ∷ nil)
  p1 = cong-≡ _ _ (λ ■ → ■ ++ (x ∷ nil)) (IH ys)
  p2 :  rev ys ++ rev xs ++ (x ∷ nil) ≡  rev ys ++ (rev xs ++ (x ∷ nil))
  p2 = ++-assoc (rev ys) (rev xs) (x ∷ nil)
  Goal : rev (xs ++ ys) ++ (x ∷ nil) ≡ rev ys ++ (rev xs ++ (x ∷ nil))
  Goal = trans-≡ _ _ _ p1 p2 

idemp-rev : {A : Set} → (xs : List A) → rev (rev xs) ≡ xs
idemp-rev nil = refl
idemp-rev (x ∷ xs) = Goal where
  IH : rev (rev xs) ≡ xs
  IH = idemp-rev xs
  p1 :  rev (rev xs ++ (x ∷ nil)) ≡ (rev (x ∷ nil)) ++ (rev (rev xs))
  p1 = rev-++ (rev xs) (x ∷ nil)
  p2 :  (rev (x ∷ nil)) ++ (rev (rev xs)) ≡ (rev (x ∷ nil)) ++ xs
  p2 = cong-≡ _ _ (λ ■ → (rev (x ∷ nil)) ++ ■) IH 
  Goal : rev (rev xs ++ (x ∷ nil)) ≡ x ∷ xs
  Goal = trans-≡ (rev (rev xs ++ (x ∷ nil))) (x ∷ rev (rev xs)) (x ∷ xs) p1 p2 


-- ASSIGNMENT W3 SOLUTIONS

-- Addition is a commutative monoid
+suc : ∀ n m → n + suc m ≡ suc (n + m)
+suc zero m = refl
+suc (suc n) m = Goal where
  IH : ∀ m →  n + suc m ≡ suc (n + m)
  IH m = +suc n m 
  Goal : n + suc (suc m) ≡ suc (n + suc m)
  Goal = IH (suc m) 

+0 : ∀ n → zero + n ≡ n
+0 n = refl 

0+ : ∀ n → n + zero ≡ n
0+ zero = refl
0+ (suc n) = Goal where
  p : n + suc zero ≡ suc (n + zero)
  p = +suc n zero 
  IH' :  suc (n + zero) ≡ suc n
  IH' = cong-≡ _ _ (λ ■ → suc ■) (0+ n)
  Goal : n + suc zero ≡ suc n
  Goal = trans-≡ (n + suc zero) (suc (n + zero)) (suc n) p IH'

assoc+ : ∀ m n p → (m + n) + p ≡ m + (n + p)
assoc+ zero n p = refl
assoc+ (suc m) n p = Goal where
  p1 : (m + suc n) + p ≡ suc (m + n) + p
  p1 = cong-≡ _ _ (λ ■ → ■ + p) (+suc m n)
  p2 : suc (m + n) + p ≡ suc ((m + n) + p)
  p2 = +suc (m + n) p
  p3 : suc ((m + n) + p) ≡ suc (m + (n + p))
  p3 = cong-≡ _ _ (λ ■ → suc ■) (assoc+ m n p )
  p4 : suc (m + (n + p)) ≡  m + suc (n + p)
  p4 = sym-≡ _ _ (+suc m (n + p))
  Goal : (m + suc n) + p ≡ m + suc (n + p)
  Goal = trans-≡ ((m + suc n) + p) ((m + n) + suc p) (m + suc (n + p)) p1
        (trans-≡ ((m + n) + suc p) (suc ((m + n) + p)) (m + suc (n + p)) p2
        (trans-≡ (suc ((m + n) + p)) (suc (m + n + p)) (m + suc (n + p)) p3 p4)) 

comm+ : ∀ m n → m + n ≡ n + m
comm+ zero n = sym-≡ _ _ (0+ n)
comm+ (suc m) n = Goal where
  p1 : m + suc n ≡ suc (m + n)
  p1 = +suc m n
  p2 : suc (m + n) ≡ suc (n + m)
  p2 = cong-≡ _ _ (λ ■ → suc ■) (comm+ m n )
  p3 : suc (n + m) ≡ n + suc m
  p3 = sym-≡ (n + suc m) (suc (n + m)) (+suc n m)
  Goal : m + suc n ≡ n + suc m
  Goal = trans-≡ (m + suc n) (suc (m + n)) (n + suc m) p1
        (trans-≡ (suc (m + n)) (suc (n + m)) (n + suc m) p2 p3)


------------------------------------------------------------------
-- Lecture W4 -- reference solutions
------------------------------------------------------------------
revh : {A : Set} → List A → List A → List A
revh nil acc      = acc
revh (x ∷ xs) acc = revh xs (x ∷ acc) 

trev : {A : Set} → List A → List A 
trev xs = revh xs nil

revap : {A : Set} → (xs ys : List A) → rev xs ++ ys ≡ revh xs ys
revap nil ys = refl
revap (x ∷ xs) ys = Goal where
  IH : ∀ ys → rev xs ++ ys ≡ revh xs ys
  IH ys = revap xs ys
  p : rev xs ++ (x ∷ nil) ++ ys ≡ rev xs ++ (x ∷ ys)
  p = ++-assoc (rev xs) (x ∷ nil) ys
  Goal : rev xs ++ (x ∷ nil) ++ ys ≡ revh xs (x ∷ ys)
  Goal = trans-≡ _ _ _ p (IH (x ∷ ys)) 

rev≡trev : {A : Set} → (xs : List A) → rev xs ≡ trev xs
rev≡trev nil = refl
rev≡trev (x ∷ xs) = Goal where
  IH : rev xs ≡ revh xs nil
  IH = rev≡trev xs
  Goal : rev xs ++ (x ∷ nil) ≡ revh xs (x ∷ nil)
  Goal = revap xs (x ∷ nil) 

-- Zipping two lists and property

zip : {A : Set} → List A → List A → List A
zip nil ys = ys
zip xs nil = xs
zip (x ∷ xs) (y ∷ ys) = x ∷ y ∷ zip xs ys 

len : {A : Set} → List A → Nat
len nil = zero
len (x ∷ xs) = suc (len xs)

prop5 : {A : Set} → (xs : List A) → zip xs nil ≡ xs
prop5 nil = refl
prop5 (x ∷ xs) = refl 

prop6 : ∀ m n → suc (suc (m + n)) ≡ m + suc (suc n)
prop6 m n = Goal where
  p : suc (suc (m + n)) ≡ suc (m + suc n)
  p = cong-≡ _ _ suc (sym-≡ _ _ (+suc m n))            -- auto 3 hints
  Goal : suc (suc (m + n)) ≡ m + suc (suc n)
  Goal = trans-≡ _ _ _ p (sym-≡ _ _ (+suc m (suc n)))  -- auto with 2 hints

lenzip : {A : Set} → (xs ys : List A)
       → len (zip xs ys) ≡ len xs + len ys
lenzip nil ys = refl
lenzip xs nil = Goal where
  p :  len (zip xs nil) ≡ len xs
  p = cong-≡ _ _ len (prop5 xs)
  Goal : len (zip xs nil) ≡ len xs + zero
  Goal = trans-≡ _ _ _ p (sym-≡ _ _ (0+ (len xs))) 
lenzip (x ∷ xs) (y ∷ ys) = Goal where
  -- There are 3 useful IHs depending on whether we reduce xs, ys or both!
  -- The useful one is on both
  IH : len (zip xs ys) ≡ len xs + len ys
  IH = lenzip xs ys
  p : suc (suc (len (zip xs ys))) ≡ suc (suc (len xs + len ys))
  p = cong-≡ _ _ (λ ■ → suc (suc ■)) IH
  Goal : suc (suc (len (zip xs ys))) ≡ len xs + suc (suc (len ys))
  Goal = trans-≡ _ _ _ p (prop6 (len xs) (len ys)) 

-- induction on both because the function is defined on both !


-- Lab Tasks

map : {A B : Set} → (f : A → B) → List A → List B 
map f nil = nil
map f (x ∷ xs) = (f x) ∷ (map f xs) 

len≡len : {A B : Set} → (f : A → B) → (xs : List A) →  len (map f xs) ≡ len xs
len≡len f nil = refl
len≡len f (x ∷ xs) = cong-≡ (len (map f xs)) (len xs) suc (len≡len f xs)

map++≡ : {A B : Set} → (f : A → B) → (xs ys : List A) → map f (xs ++ ys) ≡ map f xs ++ map f ys
map++≡ f nil ys = refl
       --                                                               Stupid bit must be infix function written like this
map++≡ f (x ∷ xs) ys = cong-≡ ( map f (xs ++ ys)) ( map f xs ++ map f ys) (_∷_ (f x)) (map++≡ f xs ys)

_○_ : {A B C : Set} → (B → C) → (A → B) → (A → C)
_○_ f g x = f (g x) 

infix 5 _○_

doubleMap≡ :  {A B C : Set} → (f : B → C) → (g : A → B) → (xs : List A) → 
              map f (map g xs) ≡  map (f ○ g) xs
doubleMap≡ f g nil = refl
doubleMap≡ f g (x ∷ xs) = cong-≡ (map f (map g xs)) (map (f ○ g) xs) (_∷_ (f (g x))) (doubleMap≡ f g xs)

mapRev≡ : {A B C : Set} → (f : A → B) → (xs : List A) → map f (rev xs) ≡ rev (map f xs)
mapRev≡ f₁ nil = refl
mapRev≡ f (x ∷ xs) = cong-≡ (map f (rev xs)) (rev (map f xs)) {!_++_ (f x ∷ nil)!} (mapRev≡ f xs) 
