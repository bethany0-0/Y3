{-# OPTIONS --copatterns #-}

open import Agda.Builtin.Nat public
open import Agda.Builtin.String public

record Person : Set where
  field
    name : String
    age : Nat
    phone : String
open Person

Alice : Person
Alice = record { name = "Alice" ; age = 42 ; phone = "03141 592653" }

Bob : Person
Bob = record { name = "Bob" ; age = 38 ; phone = "01618 033988" }

Carol : Person
name Carol = "Carol"
age Carol = 22
phone Carol = "06022 140857"

record _∧_ (A B : Set) : Set where
  field
    fst : A
    snd : B
open _∧_

pair : {A B : Set} → A → B → A ∧ B
fst (pair a b) = a
snd (pair a b) = b
-- pair a b = record { fst = a ; snd = b }

record Stream (A : Set) : Set where
  coinductive
  field
    hd : A
    tl : Stream A
open Stream

data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A
infixr 10 _∷_

take : Nat → {A : Set} → Stream A → List A
take zero xs = []
take (suc n) xs = (hd xs) ∷ (take n (tl xs))

repeat : {A : Set} → A → Stream A
hd (repeat a) = a
tl (repeat a) = repeat a

upFrom : Nat → Stream Nat
hd (upFrom n) = n
tl (upFrom n) = upFrom (suc n)

zipWith : {A B C : Set} → (A → B → C) → Stream A → Stream B → Stream C
hd (zipWith f xs ys) = f (hd xs) (hd ys)
tl (zipWith f xs ys) = zipWith f (tl xs) (tl ys)

evens = zipWith _*_ (upFrom 0) (repeat 2)
odds = zipWith _+_ evens (repeat 1)

-- fib : Stream Nat
-- hd fib = 0
-- hd (tl fib) = 1
-- tl (tl fib) = zipWith _+_ fib (tl fib)

record Show (A : Set) : Set where
  field
    show : A → String
open Show

instance-Show-String : Show String
show instance-Show-String s = s

data Color : Set where
  Red : Color
  Green : Color
  Blue : Color

instance-Show-Color : Show Color
show instance-Show-Color Red = "R"
show instance-Show-Color Green = "G"
show instance-Show-Color Blue = "B"

instance-Show-List : {A : Set} → Show A → Show (List A)
show (instance-Show-List showA) [] = "[]"
show (instance-Show-List showA) (a ∷ as) = primStringAppend (show showA a)
                                           (primStringAppend " ∷ "
                                            (show (instance-Show-List showA) as))

RGB : String
RGB = show (instance-Show-List instance-Show-Color) (Red ∷ Blue ∷ Green ∷ [])

