{-# OPTIONS --copatterns #-}

record Equiv (A : Set) : Set₁ where
  field
    _≈_ : A → A → Set
    refl : ∀ a → a ≈ a
    trans : ∀ {a b c} → a ≈ b → b ≈ c → a ≈ c
    sym : ∀ {a b} → a ≈ b → b ≈ a
  infixr 3 _=⟨_⟩_
  infixr 4 _∎
  _=⟨_⟩_ : ∀ x {y z} → x ≈ y → y ≈ z → x ≈ z
  _=⟨_⟩_ x p q = trans p q
  _∎ : ∀ x → x ≈ x
  _∎ x = refl x

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

module TrivEq where
  data triveq {A : Set} : A → A → Set where
    * : ∀ {a1 a2} → triveq a1 a2

  isEquiv : {A : Set} → Equiv A
  Equiv._≈_ isEquiv = triveq
  Equiv.refl isEquiv = λ _ → *
  Equiv.trans isEquiv = λ _ _ → *
  Equiv.sym isEquiv = λ _ → *
  

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

record Group (A : Set) : Set₁ where
  field
    isMonoid : Monoid A
  open Monoid isMonoid
  open Equiv isEquiv
  field
    inv : A → A
    invl : ∀ a → (a + (inv a)) ≈ zero
    invr : ∀ a → ((inv a) + a) ≈ zero

module ListMonoid where
  data List (A : Set) : Set where
    [] : List A
    _∷_ : A → List A → List A
  infixr 10 _∷_

  _++_ : {A : Set} → List A → List A → List A
  [] ++ bs = bs
  (a ∷ as) ++ bs = a ∷ (as ++ bs)

  open PropEq
  ++-idr : {A : Set} → (as : List A) → (as ++ []) ≡ as
  ++-idr [] = refl []
  ++-idr (x ∷ as) = cong (_∷_ x) (++-idr as)

  ++-assoc : {A : Set} → (as bs cs : List A) → ((as ++ bs) ++ cs) ≡ (as ++ (bs ++ cs))
  ++-assoc [] bs cs = refl (bs ++ cs)
  ++-assoc (a ∷ as) bs cs = cong (_∷_ a) (++-assoc as bs cs)

  isMonoid : {A : Set} → Monoid (List A)
  Monoid.isEquiv isMonoid = isEquiv
  Monoid._+_ isMonoid = _++_
  Monoid.zero isMonoid = []
  Monoid.congl isMonoid = λ as p → cong (λ bs → as ++ bs) p
  Monoid.congr isMonoid = λ bs p → cong (λ as → as ++ bs) p
  Monoid.idl isMonoid = refl
  Monoid.idr isMonoid = ++-idr
  Monoid.assoc isMonoid = ++-assoc

  eval : {A X : Set} → Monoid X → (A → X) → List A → X
  eval monX f []  = Monoid.zero monX
  eval monX f (a ∷ as) = Monoid._+_ monX (f a) (eval monX f as)

module NatMonoids where
  data ℕ : Set where
    zero : ℕ
    suc : ℕ → ℕ

  _+_ : ℕ → ℕ → ℕ
  zero + n = n
  (suc m) + n = suc (m + n)

  open PropEq
  +-idr : (m : ℕ) → (m + zero) ≡ m
  +-idr zero = refl zero
  +-idr (suc m) = cong suc (+-idr m)

  +-assoc : (m n p : ℕ) → ((m + n) + p) ≡ (m + (n + p))
  +-assoc zero n p = refl (n + p)
  +-assoc (suc m) n p = cong suc (+-assoc m n p)

  plusMonoid : Monoid ℕ
  Monoid.isEquiv plusMonoid = isEquiv
  Monoid._+_ plusMonoid = _+_
  Monoid.zero plusMonoid = zero
  Monoid.congl plusMonoid = λ m p → cong (λ n → m + n) p
  Monoid.congr plusMonoid = λ n p → cong (λ m → m + n) p
  Monoid.idl plusMonoid = refl
  Monoid.idr plusMonoid = +-idr
  Monoid.assoc plusMonoid = +-assoc

  max : ℕ → ℕ → ℕ
  max zero n = n
  max (suc m) zero = suc m
  max (suc m) (suc n) = suc (max m n)

  max-idr : (m : ℕ) → max m zero ≡ m
  max-idr zero = refl zero
  max-idr (suc m) = refl (suc m)

  max-assoc : (m n p : ℕ) → (max (max m n) p) ≡ (max m (max n p))
  max-assoc zero n p = refl (max n p)
  max-assoc (suc m) zero p = refl (max (suc m) p)
  max-assoc (suc m) (suc n) zero = refl (suc (max m n))
  max-assoc (suc m) (suc n) (suc p) = cong suc (max-assoc m n p)

  maxMonoid : Monoid ℕ
  Monoid.isEquiv maxMonoid = isEquiv
  Monoid._+_ maxMonoid = max
  Monoid.zero maxMonoid = zero
  Monoid.congl maxMonoid = λ m p → cong (λ n → max m n) p
  Monoid.congr maxMonoid = λ n p → cong (λ m → max m n) p
  Monoid.idl maxMonoid = refl
  Monoid.idr maxMonoid = max-idr
  Monoid.assoc maxMonoid = max-assoc

module Test where
  open ListMonoid
  open NatMonoids

  length : {A : Set} → List A → ℕ
  length xs = eval NatMonoids.plusMonoid (λ x → suc zero) xs

  concat : {A : Set} → List (List A) → List A
  concat xs = eval ListMonoid.isMonoid  (λ x → x) xs

  sum : List ℕ → ℕ
  sum xs = eval NatMonoids.plusMonoid (λ x → x) xs

  maximum : List ℕ → ℕ
  maximum xs = eval NatMonoids.maxMonoid (λ x → x) xs

  xs = (suc zero) ∷ zero ∷ zero ∷ (suc zero) ∷ []
  ys = (suc (suc zero)) ∷ (suc zero) ∷ (suc (suc (suc zero))) ∷ []
  zs = xs ∷ ys ∷ xs ∷ ys ∷ []

  test1 = length xs
  test2 = sum xs
  test3 = length ys
  test4 = sum ys
  test5 = maximum ys
  test6 = concat zs
