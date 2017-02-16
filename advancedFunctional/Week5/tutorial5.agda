module tutorial5 where

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

-- Tree

data BinTree (A : Set) : Set where
  nil : BinTree A
  _,_,_ : BinTree A → A → BinTree A → BinTree A

inorder : {A : Set} → BinTree A → List A
inorder nil = nil
inorder (LeftTree , x , RightTree) = (inorder LeftTree) ++ (x ∷ (inorder RightTree))

-- Implement Fold for tree and list

foldTree : {A B : Set} → (B → A → B) → B → BinTree A → B
foldTree f b nil = b
foldTree f b (Ltree , x , Rtree) = foldTree f (foldTree f (f b x) Ltree) Rtree

foldList : {A B : Set} → (B → A → B) → B → List A → B
foldList f b nil = b
foldList f b (x ∷ list) = foldList f (f b x) list

-- Show they are equivilent

{- useful functions for ≡ reasoning -}

begin_ : {A : Set}{a b : A} → a ≡ b → a ≡ b
begin refl = refl

_≡⟨_⟩_ : {A : Set}(a {b c} : A) → a ≡ b → b ≡ c → a ≡ c
a ≡⟨ refl ⟩ refl = refl

_∎ : {A : Set}(x : A) → x ≡ x
x ∎ = refl

infix 2 begin_
infixr 3 _≡⟨_⟩_
infix 4 _∎

foldTree≡foldList : (tree : BinTree Nat) → (foldTree _+_ zero tree) ≡ (foldList _+_ zero (inorder tree)) 
foldTree≡foldList nil = refl
foldTree≡foldList (Ltree , x , Rtree)
  = begin
   foldTree _+_ (foldTree _+_ (_+_ zero x) Ltree) Rtree
    ≡⟨ cong-≡ (foldTree _+_ (_+_ zero x) Ltree) (foldList _+_ (_+_ zero x) (inorder Ltree)) (λ z → foldTree _+_ z Rtree) {!foldTree≡foldList Ltree!} ⟩
    foldTree _+_ (foldList _+_ (_+_ zero x) (inorder Ltree)) Rtree
    ≡⟨ {!!} ⟩
    foldList _+_ (foldList _+_ (_+_ zero x) (inorder Ltree)) (inorder Rtree)
    ≡⟨ {!!} ⟩
       foldList _+_ (_+_ zero x) (inorder Ltree ++ inorder Rtree)
    ≡⟨ {!!} ⟩
    foldList _+_ zero (inorder Ltree ++ (x ∷ inorder Rtree))
    ∎
