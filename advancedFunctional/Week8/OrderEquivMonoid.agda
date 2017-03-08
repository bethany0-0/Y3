{-# OPTIONS --copatterns #-}
-- a (pre)order on a type is a relation which is reflexive and transitive
record Order (A : Set) : Set₁ where
  field
    _≼_ : A → A → Set   -- ≼ is notated "\preceq"
    refl : ∀ a → a ≼ a
    trans : ∀ {a b c} → a ≼ b → b ≼ c → a ≼ c
  _≼⟨_⟩_ : ∀ x {y z} → x ≼ y → y ≼ z → x ≼ z
  _≼⟨_⟩_ x p q = trans p q
  _∎ : ∀ x → x ≼ x
  _∎ x = refl x

-- an equivalence on a type is a relation which is reflexive, transitive, and symmetric
record Equiv (A : Set) : Set₁ where
  field
    _≈_ : A → A → Set   -- ≈ is notated "\approx"
    refl : ∀ a → a ≈ a
    trans : ∀ {a b c} → a ≈ b → b ≈ c → a ≈ c
    sym : ∀ {a b} → a ≈ b → b ≈ a
  infixr 3 _≈⟨_⟩_
  infixr 4 _∎
  _≈⟨_⟩_ : ∀ x {y z} → x ≈ y → y ≈ z → x ≈ z
  _≈⟨_⟩_ x p q = trans p q
  _∎ : ∀ x → x ≈ x
  _∎ x = refl x

-- a monoid structure on a type equipped with some notion of
-- equivalence is a binary operation that respects that notion of
-- equivalence (i.e., satisfies "congruence axioms"), has a left/right
-- identity element, and is associative.
record Monoid (A : Set) : Set₁ where
  field
    isEquiv : Equiv A
  open Equiv isEquiv
  field
    _+_ : A → A → A
    zero : A
    congl : ∀ (a : A) {b1 b2 : A} → b1 ≈ b2 → (a + b1) ≈ (a + b2)
    congr : ∀ b {a1 a2} → a1 ≈ a2 → (a1 + b) ≈ (a2 + b)
    idl : ∀ a → (zero + a) ≈ a
    idr : ∀ a → (a + zero) ≈ a
    assoc : ∀ a b c → ((a + b) + c) ≈ (a + (b + c))

-- Recall the definition of conjunction/product types as a record:
record _∧_ (A B : Set) : Set where
  constructor pair
  field
    fst : A
    snd : B
open _∧_

{-
  Note that the "constructor pair" line above is essentially just a shorthand
  declaration for the following explicit definition:

    pair : A → B → A ∧ B
    fst (pair a b) = a
    snd (pair a b) = b

  But another convenient thing the declaration allows us to do is pattern-match
  against values of the form "pair a b", like so:

    foo : A ∧ B → C
    foo (pair a b) = blah a b

  Agda treats this as "syntactic sugar" for the following explicit definition
  using the destructors fst and snd:

    foo : A ∧ B → C
    foo x = blah (fst x) (snd x)
-}

-- We can similarly define existential types as a record:
record ∃ {A : Set} (B : A → Set) : Set where
  constructor pack
  field
    witness : A
    proof : B witness
open ∃

-- You are now going to consider a few constructions relating orders, equivalence
-- relations, and monoids...

-- Task 1: prove that any monoid induces an order via the following
-- construction (which you'll recall from last week!)
module Monoid→Order {A : Set} (monA : Monoid A) where
  open Monoid monA
  open Equiv isEquiv

  isOrder : Order A
  Order._≼_ isOrder x y = ∃ λ k → (x + k) ≈ y
  Order.refl isOrder a = pack zero (idr a)
  Order.trans isOrder {a} {b} {c} (pack j p) (pack k q) = pack ((j + k)) (trans (sym (assoc a j k)) (trans (congr k p) q)) 

-- For an example of this construction in action, let's first recall
-- the two canonical monoid structures on natural numbers under addition
-- and under multiplication (the equivalence relation is just equality).

module PropEq where
  data _≡_ {A : Set} : A → A → Set where
    refl : (a : A) → a ≡ a

  trans : {A : Set} {a b c : A} → a ≡ b → b ≡ c → a ≡ c
  trans (refl a) (refl .a) = refl a

  sym : {A : Set} {a b : A} → a ≡ b → b ≡ a
  sym (refl a) = refl a

  isEquiv : {A : Set} → Equiv A
  Equiv._≈_ isEquiv = _≡_
  Equiv.refl isEquiv = refl
  Equiv.trans isEquiv = trans
  Equiv.sym isEquiv = sym

  cong : {A B : Set} → (f : A → B) → {a₀ a₁ : A} → a₀ ≡ a₁ → f a₀ ≡ f a₁
  cong f (refl a) = refl (f a)

module NatMonoids where
  data ℕ : Set where
    zero : ℕ
    suc : ℕ → ℕ
  {-# BUILTIN NATURAL ℕ #-} -- declaring ℕ as a "BUILTIN" allows us to use literals 0, 1, 2, etc.


  _+_ : ℕ → ℕ → ℕ
  zero + n = n
  (suc m) + n = suc (m + n)

  open PropEq using (_≡_; isEquiv; cong)
  open Equiv (isEquiv {ℕ})

  plusMonoid : Monoid ℕ
  Monoid.isEquiv plusMonoid = isEquiv
  Monoid._+_ plusMonoid = _+_
  Monoid.zero plusMonoid = zero
  Monoid.congl plusMonoid m p = cong (λ n → m + n) p
  Monoid.congr plusMonoid n p = cong (λ m → m + n) p
  Monoid.idl plusMonoid m = refl m
  Monoid.idr plusMonoid zero = refl zero
  Monoid.idr plusMonoid (suc m) = cong suc (Monoid.idr plusMonoid m)
  Monoid.assoc plusMonoid zero n p = refl (n + p)
  Monoid.assoc plusMonoid (suc m) n p = cong suc (Monoid.assoc plusMonoid m n p)

  _*_ : ℕ → ℕ → ℕ
  zero * n = zero
  (suc m) * n = n + (m * n)

  timesMonoid : Monoid ℕ
  Monoid.isEquiv timesMonoid = isEquiv
  Monoid._+_ timesMonoid = _*_
  Monoid.zero timesMonoid = suc zero
  Monoid.congl timesMonoid m p = cong (λ n → m * n) p
  Monoid.congr timesMonoid n p = cong (λ m → m * n) p
  Monoid.idl timesMonoid m = Monoid.idr plusMonoid m 
  Monoid.idr timesMonoid zero = refl zero
  Monoid.idr timesMonoid (suc m) = cong suc (Monoid.idr timesMonoid m)
  Monoid.assoc timesMonoid zero n p = refl zero
  Monoid.assoc timesMonoid (suc m) n p =
               ((n + (m * n)) * p) ≈⟨ *+-distribr n (m * n) p ⟩
               ((n * p) + ((m * n) * p)) ≈⟨ Monoid.congl plusMonoid (n * p) (Monoid.assoc timesMonoid m n p) ⟩
               ((n * p) + (m * (n * p))) ∎ 
    where
      *+-distribr : (m n p : ℕ) → ((m + n) * p) ≡ ((m * p) + (n * p))
      *+-distribr zero n p = refl (n * p)
      *+-distribr (suc m) n p =
                  (p + ((m + n) * p)) ≈⟨ Monoid.congl plusMonoid p (*+-distribr m n p) ⟩
                  (p + ((m * p) + (n * p))) ≈⟨ sym (Monoid.assoc plusMonoid p (m * p) (n * p)) ⟩
                  ((p + (m * p)) + (n * p)) ∎

-- Task 2: verify that 7 ≼ 42 with respect to both the +-order and the *-order on ℕ
module Examples where
  open NatMonoids
  open PropEq

  module MOplus = Monoid→Order plusMonoid
  example1 : Order._≼_ (MOplus.isOrder) 7 42
  example1 = pack 35 (refl (35 + 7))

  module MOtimes = Monoid→Order timesMonoid
  example2 : Order._≼_ (MOtimes.isOrder) 7 42
  example2 = pack 6 (refl (6 * 7))

-- Task 3: prove that any order induces an equivalence relation, by considering
-- mutually ordered elements.
module Order→Equiv {A : Set} (preA : Order A) where
  open Order preA
  isEquiv : Equiv A
  Equiv._≈_ isEquiv a b = (a ≼ b) ∧ (b ≼ a)
  Equiv.refl isEquiv a = pair (refl a) (refl a)
  Equiv.trans isEquiv p q = pair (trans (fst p) (fst q)) (trans (snd q) (snd p))
  Equiv.sym isEquiv p = pair (snd p) (fst p)

-- Task 4 (hard): prove that any equivalence relation on a type A induces
-- a monoid of endofunctions A → A respecting the equivalence relation.
module Equiv→Monoid {A : Set} (eqA : Equiv A) where
  open Equiv eqA

  isMonoid : Monoid (∃ {A → A} λ f → ∀ {a b : A} → a ≈ b → f a ≈ f b)
  Equiv._≈_ (Monoid.isEquiv isMonoid) F G = ∀ a → witness F a ≈ witness G a
  Equiv.refl (Monoid.isEquiv isMonoid) F  = λ a → refl (witness F a)
  Equiv.trans (Monoid.isEquiv isMonoid) p q = λ a → trans (p a) (q a)
  Equiv.sym (Monoid.isEquiv isMonoid) p = λ a → sym (p a)
  Monoid._+_ isMonoid F G =  pack {!!} {!!} 
  Monoid.zero isMonoid = pack {!!} {!!}
  Monoid.congl isMonoid F p = λ a → {!!}
  Monoid.congr isMonoid G p = {!!}
  Monoid.idl isMonoid F = {!!}
  Monoid.idr isMonoid G = {!!}
  Monoid.assoc isMonoid F G H = {!!}

--  plusMonoid : Monoid ℕ
--  Monoid.isEquiv plusMonoid = isEquiv
--  Monoid._+_ plusMonoid = _+_
--  Monoid.zero plusMonoid = zero
--  Monoid.congl plusMonoid m p = cong (λ n → m + n) p
--  Monoid.congr plusMonoid n p = cong (λ m → m + n) p
--  Monoid.idl plusMonoid m = refl m
--  Monoid.idr plusMonoid zero = refl zero
--  Monoid.idr plusMonoid (suc m) = cong suc (Monoid.idr plusMonoid m)
--  Monoid.assoc plusMonoid zero n p = refl (n + p)
--  Monoid.assoc plusMonoid (suc m) n p = cong suc (Monoid.assoc plusMonoid m n p)
