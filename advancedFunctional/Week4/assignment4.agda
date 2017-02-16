module assignment4 where

-- Nat ------------------------

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

₀ = zero
₁ = suc ₀
₂ = suc ₁
₃ = suc ₂

_+_ : Nat → Nat → Nat
zero  + n = n
suc m + n = suc (m + n) 

infixr 10 _+_

-- List --------------------------------------------

data List (A : Set) : Set where 
  nil : List A
  _∷_ : A → List A → List A

infixr 4 _∷_

_++_ : {A : Set} → List A → List A → List A
nil ++ ys = ys
(x ∷ xs) ++ ys = x ∷ xs ++ ys

infixl 5 _++_

len : {A : Set} → List A → Nat
len nil = zero
len (x ∷ xs) = suc (len xs)

-- Booleans --------------------

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
_||_ true _ = true 
_||_ false x = x

infixl 5 _||_

-- basic properties of equality

data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x

trans-≡ : {A : Set} → (a b c : A) → a ≡ b → b ≡ c → a ≡ c
trans-≡ a .a .a refl refl = refl 

sym-≡ : {A : Set} → (a b : A) → a ≡ b → b ≡ a
sym-≡ a .a refl = refl 

cong-≡ : {A B : Set} → (a b : A) (f : A → B) → a ≡ b → f a ≡ f b
cong-≡ a .a f refl = refl

cong-suc : ∀ m n → m ≡ n → suc m ≡ suc n
cong-suc m .m refl = refl

-- Part 1 ------------------------------

data BinTree (A : Set) : Set where
  nil : BinTree A
  _,_,_ : BinTree A → A → BinTree A → BinTree A

size : {A : Set} → BinTree A → Nat
size nil = zero
size (LeftTree , x , RightTree) = suc ((size LeftTree) + (size RightTree))  

inorder : {A : Set} → BinTree A → List A
inorder nil = nil
inorder (LeftTree , x , RightTree) = (inorder LeftTree) ++ (x ∷ (inorder RightTree))

-- Part 2 ----------------------------------------------------- 

len++ : {A : Set} → (xs ys : List A) → (len (xs ++ ys)) ≡ ((len xs) + (len ys))
len++ nil ys = refl
len++ (x ∷ xs) ys = cong-≡ (len (xs ++ ys)) ((len xs) + (len ys))  suc (len++ xs ys)

suc≡∷ : {A : Set} → (xs ys : List A) → (z : A) → suc (len xs + len ys) ≡
      (len xs + (len (z ∷ ys)))
suc≡∷ nil ys z = refl
suc≡∷ (x ∷ xs) ys z = cong-≡ (suc (len xs + len ys)) (len xs + (len (z ∷ ys))) suc (suc≡∷ xs ys z)

size≡len : {A : Set} → (tree : BinTree A) → (size tree) ≡ (len (inorder tree))
size≡len nil = refl
size≡len (Ltree , x , Rtree) = Goal where
  ---------Proof 1
  removeX : suc (size Ltree + size Rtree) ≡
              suc (len (inorder Ltree) + len (inorder Rtree))
  ------ in cong, prove the different bit, when apply the full function it is stil the same
  removeX = trans-≡ (suc (size Ltree + size Rtree)) (suc (len (inorder Ltree) + (size Rtree))) (suc (len (inorder Ltree) + len (inorder Rtree))) (cong-≡ (size Ltree) (len (inorder Ltree)) (λ x → (suc ( x + (size Rtree)))) (size≡len Ltree)) (cong-≡ (size Rtree) (len (inorder Rtree)) (λ x → (suc (len (inorder Ltree) + x))) (size≡len Rtree))
-------------Proof 2
  useLen++ : (len (inorder Ltree) + suc (len (inorder Rtree))) ≡
               len (inorder Ltree ++ (x ∷ inorder Rtree))
  useLen++ = sym-≡ (len (inorder Ltree ++ (x ∷ inorder Rtree))) (len (inorder Ltree) + suc (len (inorder Rtree))) (len++ (inorder Ltree) (x ∷ inorder Rtree))
  swapProof : suc (len (inorder Ltree) + len (inorder Rtree)) ≡
                len (inorder Ltree ++ (x ∷ inorder Rtree))
  swapProof = trans-≡ (suc (len (inorder Ltree) + len (inorder Rtree))) ((len (inorder Ltree)) + len ( x ∷ inorder Rtree)) (len (inorder Ltree ++ (x ∷ inorder Rtree))) (suc≡∷ (inorder Ltree) (inorder Rtree) x) useLen++
  -----------Goal ------------
  Goal : suc (size Ltree + size Rtree) ≡
           len (inorder Ltree ++ (x ∷ inorder Rtree))
  Goal = trans-≡ (suc (size Ltree + size Rtree)) (suc ((len (inorder Ltree)) + (len (inorder Rtree)))) (len (inorder Ltree ++ (x ∷ inorder Rtree))) removeX swapProof

-- Part 3 ----------------------------------------------------

existsTree : {A : Set} → BinTree A → (A → Bool) → Bool
existsTree nil f = false
existsTree (Ltree , x , Rtree) f = (f x) || (existsTree Ltree f) || (existsTree Rtree f)

existsList : {A : Set} → List A → (A → Bool) → Bool
existsList nil f = false
existsList (x ∷ list) f = (f x) || (existsList list f)

-- Part 4 -------------------------------------------------------

||ass≡ : (x y z : Bool) → ((x || y) || z) ≡ (x || (y || z))
||ass≡ true y z = refl
||ass≡ false true z = refl
||ass≡ false false true = refl
||ass≡ false false false = refl 

||com≡ : (x y : Bool) → (x || y) ≡ (y || x)
||com≡ true true = refl
||com≡ true false = refl
||com≡ false true = refl
||com≡ false false = refl

generalise : {A : Set} → (xs ys : List A) → (p : (A → Bool)) → ((existsList xs p) || (existsList ys p)) ≡ (existsList (xs ++ ys) p)
generalise nil ys p = refl
generalise (x ∷ xs) ys p = trans-≡ (p x || existsList xs p || existsList ys p) (p x || (existsList xs p || existsList ys p))  (p x || existsList (xs ++ ys) p) (||ass≡ (p x) (existsList xs p) (existsList ys p)) (cong-≡ (existsList xs p || existsList ys p) (existsList (xs ++ ys) p) (λ z → p x || z) (generalise xs ys p))

exists-≡ : {A : Set} → (tree : BinTree A) → (p : (A → Bool)) → (existsTree tree p) ≡ (existsList (inorder tree) p)
exists-≡ nil p = refl
exists-≡ (Ltree , x , Rtree) p = Goal where
  toLists : (p x || existsTree Ltree p || existsTree Rtree p) ≡
              (p x ||
               existsList (inorder Ltree) p || existsList (inorder Rtree) p)
  toLists = trans-≡ (p x || existsTree Ltree p || existsTree Rtree p) (p x || existsList (inorder Ltree) p || existsTree Rtree p) (p x ||
                                                                              existsList (inorder Ltree) p || existsList (inorder Rtree) p) (cong-≡ (existsTree Ltree p ) (existsList (inorder Ltree) p) (λ z → (p x || z || existsTree Rtree p)) (exists-≡ Ltree p)) (cong-≡ (existsTree Rtree p ) (existsList (inorder Rtree) p) (λ z → (p x || existsList (inorder Ltree) p || z)) (exists-≡ Rtree p))
  ------------------Proof2
  switchX : (p x || existsList (inorder Ltree) p ||
               existsList (inorder Rtree) p)
              ≡
              (existsList (inorder Ltree) p ||
              -- brackets around this last part make it right associative  switchX = trans-≡ (p x || existsList (inorder Ltree) p ||
                       existsList (inorder Rtree) p) ((existsList (inorder Ltree) p ||
                                                             p x || existsList (inorder Rtree) p)) (existsList (inorder Ltree) p ||
                                                             (p x || existsList (inorder Rtree) p)) (cong-≡ (p x || existsList (inorder Ltree) p) ((existsList (inorder Ltree) p) || (p x)) (λ z → z || existsList (inorder Rtree) p ) (||com≡ (p x) (existsList (inorder Ltree) p))) (||ass≡ (existsList (inorder Ltree) p) (p x)
                                                                                                                                                                                                                                                                                             (existsList (inorder Rtree) p))
               (p x || existsList (inorder Rtree) p))
  switchX = trans-≡ (p x || existsList (inorder Ltree) p ||
                       existsList (inorder Rtree) p) ((existsList (inorder Ltree) p ||
                                                             p x || existsList (inorder Rtree) p)) (existsList (inorder Ltree) p ||
                                                             (p x || existsList (inorder Rtree) p)) (cong-≡ (p x || existsList (inorder Ltree) p) ((existsList (inorder Ltree) p) || (p x)) (λ z → z || existsList (inorder Rtree) p ) (||com≡ (p x) (existsList (inorder Ltree) p))) (||ass≡ (existsList (inorder Ltree) p) (p x)
                                                                                                                                                                                                                                                                                             (existsList (inorder Rtree) p))
                                                             
  combineEx : (p x ||
                 existsList (inorder Ltree) p || existsList (inorder Rtree) p)
                ≡ existsList (inorder Ltree ++ (x ∷ inorder Rtree)) p
  combineEx = trans-≡ (p x ||
                         existsList (inorder Ltree) p || existsList (inorder Rtree) p) (existsList (inorder Ltree) p || existsList (x ∷ (inorder Rtree)) p) (existsList (inorder Ltree ++ (x ∷ inorder Rtree)) p) switchX (generalise (inorder Ltree) (x ∷ inorder Rtree) p )

  Goal : (p x || existsTree Ltree p || existsTree Rtree p) ≡
           existsList (inorder Ltree ++ (x ∷ inorder Rtree)) p
  Goal = trans-≡ (p x || existsTree Ltree p || existsTree Rtree p) (p x || existsList (inorder Ltree) p || existsList (inorder Rtree) p) (existsList (inorder Ltree ++ (x ∷ inorder Rtree)) p) toLists combineEx
