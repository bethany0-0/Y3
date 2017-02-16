{-# OPTIONS --without-K #-}

module vectorsExercises where

data _≡_ {X : Set} : X → X → Set where
 refl : {x : X} → x ≡ x

sym : {A : Set} {a₀ a₁ : A} → a₀ ≡ a₁ → a₁ ≡ a₀
sym refl = refl

trans : {A : Set} {a₀ a₁ a₂ : A} → a₀ ≡ a₁ → a₁ ≡ a₂ → a₀ ≡ a₂
trans refl p = p

cong : {X Y : Set} (f : X → Y) {x₀ x₁ : X} → x₀ ≡ x₁ → f x₀ ≡ f x₁
cong f refl = refl

data ℕ : Set where
 zero : ℕ
 succ : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero   + y = y
succ x + y = succ(x + y)

+-assoc : ∀ l m n → (l + m) + n ≡ l + (m + n)
+-assoc zero     m n = refl
+-assoc (succ l) m n = goal
 where
  IH : (l + m) + n ≡ l + (m + n)
  IH = +-assoc l m n
  goal : succ ((l + m) + n) ≡ succ (l + (m + n))
  goal = cong succ IH

data List (X : Set) : Set where
  []  : List X
  _∷_ : X → List X → List X

length : {X : Set} → List X → ℕ
length [] = zero
length (x ∷ xs) = succ (length xs)

_++_ : ∀{X} → List X → List X → List X
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

++-assoc : ∀ {X} (xs ys zs : List X)
         → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
++-assoc []       ys zs = refl
++-assoc (x ∷ xs) ys zs = goal
 where
  IH : (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
  IH = ++-assoc xs ys zs
  goal : x ∷ ((xs ++ ys) ++ zs)  ≡  x ∷ (xs ++ (ys ++ zs))
  goal = cong (λ ws → x ∷ ws) IH

data Vec (X : Set) : ℕ → Set where
  []  : Vec X zero
  _∷_ : ∀{n} → X → Vec X n → Vec X (succ n)

hd : {X : Set} {n : ℕ} → Vec X (succ n) → X
hd (x ∷ xs) = x

tl : {X : Set} {n : ℕ} → Vec X (succ n) → Vec X n
tl (x ∷ xs) = xs

data Fin : ℕ → Set where
 fzero : {n : ℕ} → Fin (succ n)
 fsucc : {n : ℕ} → Fin n → Fin (succ n)

fetch : ∀ {X} n → Vec X n → Fin n → X
fetch (succ n) (x ∷ xs)  fzero    = x
fetch (succ n) (x ∷ xs) (fsucc i) = fetch n xs i

_+++_ : ∀{X m n} → Vec X m → Vec X n → Vec X (m + n)
[]       +++ ys = ys
(x ∷ xs) +++ ys = x ∷ (xs +++ ys)

_≡[_]_ : ∀{X m n} → Vec X m → m ≡ n → Vec X n → Set
xs ≡[ refl ] ys   =   xs ≡ ys

cong-cons : ∀{X m n} (x : X) {xs : Vec X m} {ys : Vec X n} (p : m ≡ n)
          → xs ≡[ p ] ys → x ∷ xs  ≡[ cong succ p ]  x ∷ ys
cong-cons _ refl refl = refl 


+++-assoc : ∀{X} l m n (xs : Vec X l) (ys : Vec X m) (zs : Vec X n)
         → (xs +++ ys) +++ zs  ≡[ +-assoc l m n ]  xs +++ (ys +++ zs)
+++-assoc zero     m n []       ys zs = refl
+++-assoc (succ l) m n (x ∷ xs) ys zs = goal
 where
  IH : (xs +++ ys) +++ zs  ≡[ +-assoc l m n ]  xs +++ (ys +++ zs)
  IH = +++-assoc l m n xs ys zs
  goal : x ∷ ((xs +++ ys) +++ zs)  ≡[ cong succ (+-assoc l m n) ]
         x ∷ (xs +++ (ys +++ zs))
  goal = cong-cons x (+-assoc l m n) IH

zrn : ∀ n → n + zero ≡ n
zrn zero = refl
zrn (succ n) = cong succ (zrn n)

ern : ∀ {X} n (xs : Vec X n)
   → xs +++ [] ≡[ zrn n ] xs
ern zero [] = refl
ern (succ n) (x ∷ xs) = cong-cons x (zrn n) (ern n xs)

module _ 
  (A : Set)
  (B : A → Set)
 where
  _≡⟦_⟧_ : {a₀ a₁ : A} → B a₀ → a₀ ≡ a₁ → B a₁ → Set
  b₀ ≡⟦ refl ⟧ b₁   =   b₀ ≡ b₁

  congd : (f : (a : A) → B a) {a₀ a₁ : A}
        → (p : a₀ ≡ a₁) → f a₀ ≡⟦ p ⟧ f a₁
  congd f refl = refl

  transport : {a₀ a₁ : A} → a₀ ≡ a₁ → B a₀ → B a₁
  transport refl b₀ = b₀

  exercise-subst-iso₀ : {a₀ a₁ : A} (p : a₀ ≡ a₁) (b₀ : B a₀)
                      → transport (sym p) (transport p b₀)  ≡ b₀
  exercise-subst-iso₀ refl b₀ = refl

  exercise-subst-iso₁ : {a₀ a₁ : A} (p : a₀ ≡ a₁) (b₁ : B a₁) 
                      → transport p (transport (sym p) b₁)  ≡ b₁
  exercise-subst-iso₁ refl b₁ = refl

  _≡'⟦_⟧_ : {a₀ a₁ : A} → B a₀ → a₀ ≡ a₁ → B a₁ → Set
  b₀ ≡'⟦ p ⟧ b₁   =   transport p b₀ ≡ b₁
  
  φ : {a₀ a₁ : A} {b₀ : B a₀} {p : a₀ ≡ a₁} {b₁ : B a₁}
    → b₀ ≡⟦ p ⟧ b₁ → b₀ ≡'⟦ p ⟧ b₁  
  φ {a₀} {.a₀} {b₀} {refl} refl = refl

  ψ : {a₀ a₁ : A} {b₀ : B a₀} {p : a₀ ≡ a₁} {b₁ : B a₁}
    → b₀ ≡'⟦ p ⟧ b₁ → b₀ ≡⟦ p ⟧ b₁  
  ψ {a₀} {.a₀} {b₀} {refl} refl = refl

  φψid : {a₀ a₁ : A} {b₀ : B a₀} {p : a₀ ≡ a₁} {b₁ : B a₁} (q : b₀ ≡'⟦ p ⟧ b₁)
       → φ {a₀} {a₁} {b₀} {p} (ψ q) ≡ q
  φψid {a₀} {.a₀} {b₀} {refl} refl = refl

  ψφid : {a₀ a₁ : A} {b₀ : B a₀} {p : a₀ ≡ a₁} {b₁ : B a₁} (q : b₀ ≡⟦ p ⟧ b₁)
       → ψ(φ q) ≡ q
  ψφid {a₀} {.a₀} {b₀} {refl} refl = refl

_≡'[_]_ : ∀{X m n} → Vec X m → m ≡ n → Vec X n → Set
xs ≡'[ p ] ys   =   _≡'⟦_⟧_ ℕ (Vec _) xs p ys

-- 
cong-cons' : ∀{X m n} (x : X) {xs : Vec X m} {ys : Vec X n} (p : m ≡ n)
          → xs ≡'[ p ] ys → x ∷ xs  ≡'[ cong succ p ]  x ∷ ys
cong-cons' _ refl refl = refl 

+++-assoc' : ∀{X} l m n (xs : Vec X l) (ys : Vec X m) (zs : Vec X n)
         → (xs +++ ys) +++ zs  ≡'[ +-assoc l m n ]  xs +++ (ys +++ zs)
+++-assoc' zero     m n []       ys zs = refl
+++-assoc' (succ l) m n (x ∷ xs) ys zs = goal
 where
  IH : (xs +++ ys) +++ zs  ≡'[ +-assoc l m n ]  xs +++ (ys +++ zs)
  IH = +++-assoc' l m n xs ys zs
  goal : x ∷ ((xs +++ ys) +++ zs)  ≡'[ cong succ (+-assoc l m n) ]
         x ∷ (xs +++ (ys +++ zs))
  goal = cong-cons' x (+-assoc l m n) IH

-- Convert a vector to a list of the same length:

v2l :  ∀ {X n} → Vec X n → List X
v2l = {!!}

vl : ∀ {X n} (xs : Vec X n) → length(v2l xs) ≡ {!!}
vl = {!!}

-- Other way round:
l2v : ∀ {X} (xs : List X) → Vec X (length xs)
l2v = {!!}

vl-id : ∀ {X} (xs : List X) → v2l (l2v xs) ≡ xs
vl-id = {!!}

lv-id : ∀ {X n} (xs : Vec X n) → l2v (v2l xs) ≡[ {!!} ] xs
lv-id = {!!}

-- A version of the fetch function for lists:

lfetch : ∀ {X} (xs : List X) → Fin(length xs) → X
lfetch = {!!}

-- Now we will prove that the two types Vec X n and (Fin n → X) are
-- isomorphic for all X and n. Thus, we can use this function type as
-- a representation for vectors, and we start with this idea.

Vec' : Set → ℕ → Set
Vec' X n = Fin n → X

-- We will xs',ys',zs' etc. for elements of the type Vec' X n,
-- i.e. functions Fin n → X.

-- Using this representation for vectors define functions _∶∶'_ , hd',
-- tl' and fetch', corresponding to those of the original definition
-- of Vectors.

_∷'_ : ∀{X n} → X → Vec' X n → Vec' X (succ n)
(x ∷' xs')  fzero    = x
(x ∷' xs') (fsucc n) = xs' n

hd' : {X : Set} {n : ℕ} → Vec' X (succ n) → X
hd' xs' = xs' fzero

tl' : {X : Set} {n : ℕ} → Vec' X (succ n) → Vec' X n
tl' xs' = λ i → xs'(fsucc i)

hd'-lemma : ∀ {X n} (x : X) (xs' : Vec' X n)
           → hd' (x ∷' xs') ≡ x
hd'-lemma = {!!}

tl'-lemma : ∀ {X n} (x : X) (xs' : Vec' X n)
           → tl' (x ∷' xs') ≡ xs'
tl'-lemma = {!!}

-- Function extensionality says that if two functions have equal
-- values then they are equal. This cannot be proved in Agda. But it
-- is consistent, and we will use it as an assumpion.

FunExt : Set₁
FunExt = {X Y : Set} {f g : X → Y}
       → ((x : X) → f x ≡ g x) → f ≡ g

cons'-lemma : FunExt
             → ∀ {X n} (xs' : Vec' X (succ n))
             → hd' xs' ∷' tl' xs' ≡ xs'
cons'-lemma funext xs' = funext lemma
 where
  lemma : ∀ i → (hd' xs' ∷' tl' xs') i ≡ xs' i
  lemma = {!!}

-- The following dependent version of function extensionality is also
-- consistent, but we won't need it here:
DepFunExt : Set₁
DepFunExt = (X : Set) (Y : X → Set) (f g : (x : X) → (Y x))
       → ((x : X) → f x ≡ g x) → f ≡ g

-- There is nothing to do here!:
fetch' : ∀ {X} n → Vec' X n → Fin n → X
fetch' _ xs' = xs'

-- We have a function fetch : ∀ {X n} → Vec X n → Fin n → X, whose
-- type can be rewritten as follows:

forth : ∀ {X} n → Vec X n → Vec' X n
forth = fetch

-- To define a function back in the other direction, we will need to
-- do induction on the implicit argument n, and so we make it
-- explicit.

back : ∀ {X} n → Vec' X n → Vec X n
back (zero)    _  = []
back (succ n) xs' = hd' xs' ∷ back n (tl' xs') 

-- The functions (forth n) and (back n) are mutually inverse, and
-- hence we conclude that Vec X n and Vec' n X are isomorphic types.

bfid : ∀ {X} n (xs : Vec X n) → back n (forth n xs) ≡ xs
bfid  zero     []      = refl
bfid (succ n) (x ∷ xs) = cong (λ ws → x ∷ ws) IH
 where
  IH : back n (fetch n xs) ≡ xs
  IH = {!!}

-- It will be difficult to prove the other direction, namely that
-- forth n (back n xs') ≡ xs', because this is an equality of two
-- functions of type Fin n → X.

-fbid : ∀ {X} n (xs' : Vec' X n) (i : Fin n) → forth n (back n xs') i ≡ xs' i
-fbid = {!!}

-- Use -fbid and funext to prove the following:
fbid : FunExt → ∀ {X} n (xs' : Vec' X n) → forth n (back n xs') ≡ xs'
fbid funext {X} n xs' = {!!}


-- Exercise. Define vector concatenation using the representation Vec' X n
-- indirectly, using the isomorphism back and forth:

iconcat : ∀{X m n} → Vec' X m → Vec' X n → Vec' X (m + n)
iconcat = {!!}

-- Exercise (harder). Define vector concatenation using the
-- representation Vec' X n directly, without using the isomorphism.

dconcat : ∀{X m n} → Vec' X m → Vec' X n → Vec' X (m + n)
dconcat = {!!}

infixr 5 _≡_
infixr 5 _≡[_]_
infixr 5 _≡'[_]_
infixl 6 _+_
infixl 6 _++_
infixl 6 _+++_
infixr 7 _∷_
