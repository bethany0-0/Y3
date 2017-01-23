module week2 where

-- BOOLEAN VALUES

data Bool : Set where
  true  : Bool
  false : Bool

not : Bool → Bool
not true  = true
not false = false

_&_ : Bool → Bool → Bool
_&_ true true = true
_&_ _    _    = false

infixr 6 _&_

_||_ : Bool → Bool → Bool
_||_ false false = false
_||_ _     _     = true

infixr 5 _||_

if_then_else_ : Bool → Bool → Bool → Bool
if true  then x else _ = x
if false then _ else y = y

-- PROPOSITIONAL LOGIC

data ⊤ : Set where
  tt : ⊤ 

data ⊥ : Set where

data _∧_ (A B : Set) : Set where
  _,_ : A → B → A ∧ B

infixl 6 _∧_

data _∨_ (A B : Set) : Set where
  inl : A → A ∨ B
  inr : B → A ∨ B

infixl 5 _∨_

-- Hilbert-style laws

then-1 : {A B : Set} → A → B → A
then-1 a _ = a

then-2 : {A B C : Set} → (A → B → C) → (A → B) → (A → C)
then-2 f g a = f a (g a)

and-1 : {A B : Set} → A ∧ B → A
and-1 (a , _) = a

and-2 : {A B : Set} → A ∧ B → B
and-2 (_ , b) = b

and-3 : {A B : Set} → A → B → A ∧ B
and-3 a b = a , b

or-1 : {A B : Set} → A → A ∨ B
or-1 a = inl a

or-2 : {A B : Set} → B → A ∨ B
or-2 b = inr b

-- NOTE: PATTERN MATCHING
or-3 : {A B C : Set} → (A → C) → (B → C) → (A ∨ B → C) 
or-3 f g (inl x) = f x
or-3 f g (inr x) = g x

falsity : {A : Set} → ⊥ → A 
falsity = λ () -- the empty pattern

-- NEGATION

¬ : Set → Set 
¬ A = A → ⊥

not-1 : {A B : Set} → (A → B) → (A → ¬ B) → ¬ A
not-1 p q a = q a (p a) 

not-2 : {A B : Set} → A → ¬ A → B
not-2 a p = falsity (p a)


------------------------------ASSIGNMENT-------------------------------------

-- Excluded middle implies double negation

----postulate LEM : {A : Set} → A ∨ ¬ A

----classical-1 : {A : Set} → ¬ (¬ A) → A
----classical-1 f = or-3 (λ z → z) (λ a' → falsity (f a')) LEM

classical-1 : {A : Set} → (A ∨ ¬ A) → (¬ (¬ A) → A)
classical-1 (inl x) f = x
classical-1 (inr x) f = falsity (f x)


-- Double negation implies exclude middle

-- postulate DNE : {A : Set} → ¬ (¬ A) → A

-- classical-2 : {A : Set} → A ∨ ¬ A
-- classical-2 = DNE (λ z → z (inr (λ x → z (inl x))))

classical-2 : { A : Set } → ({ A : Set } → (¬ (¬ A) → A)) → (A ∨ ¬ A)
classical-2 dne = dne (λ ¬[a∨¬a] → ¬[a∨¬a] (inr (λ a → ¬[a∨¬a] (inl a))))


-- inl {!falsity!}


--   Part 2

-- data Law1 = Not(P And Q) Implies (Not P) Or (Not Q)


postulate DNE : {A : Set} → ¬ (¬ A) → A
postulate LEM : {A : Set} → A ∨ ¬ A

law1 : {P Q : Set} → (¬ (P ∧ Q)) → (¬ P ∨ ¬ Q)
law1 negAND = DNE (λ z → z (inl (λ x → z (inr (λ x₁ → negAND (x , x₁))))))

-- z = (P∧Q)        x = ¬ P        x₁ = 

-- data Law2 = (Not P) Or (Not Q) Implies Not(P And Q)

-- law2Proof :: Law2 p q
-- law2Proof = Law2 (\ or -> case or of
                     -- L left -> (\ (a, b) -> left a)
                     -- R right -> (\ (a, b) -> right b) 

law2 : {P Q : Set} → ((¬ P) ∨ (¬ Q)) → ¬ (P ∧ Q)
law2 (inl x) = λ y → not-2 (and-1 y) x
law2 (inr x) = λ y → not-2 (and-2 y) x  

-- data Law3 = Not(P Or Q) Implies (Not P) And (Not Q)

-- law3Proof :: Law3 p q
-- law3Proof = Law3 law3function

-- law3function :: (((OR p q) -> Empty) -> ((p -> Empty), (q -> Empty)))
-- law3function notOR = (left3, right3)
  -- where
    -- left3 = (\p -> notOR (L p))
    -- right3 = (\q -> notOR (R q))

law3 : {P Q : Set} → (¬ (P ∨ Q)) → (¬ P ∧ ¬ Q)
law3 negOR = (λ x → negOR (inl x)) , (λ x → negOR (inr x))

-- data Law4 = (Not P) And (Not Q) Implies Not(P Or Q)

-- law4Proof :: Law4 p q
-- law4Proof = Law4 law4function
-- -------create an or, match so left side is left function applied to a returns empty
-- law4function :: (((p -> Empty), (q -> Empty)) -> ((OR p q) -> Empty))
-- law4function (negA, negB) = (\or -> case or of
--                                 L a -> negA a
--                                 R b -> negB b

law4 : {P Q : Set} → (¬ P ∧ ¬ Q) → (¬ (P ∨ Q))
law4 (negP , _) (inl p) = negP p
law4 (_ , negQ) (inr q) = negQ q

-- z = ¬ (P ∨ Q)

-- ----Part 3 - prove 1 or 2


-- proof : {P Q : Set} → ((( P → Q) → P) → P) → (P ∨ ¬ P)
--classical-2 dne = dne (λ ¬[a∨¬a] → ¬[a∨¬a] (inr (λ a → ¬[a∨¬a] (inl a))))

-- (p -> q) -> p = falisty (¬p -> p)
               -- ¬(p∨¬p) -> (p∨¬p)

proof1 : {P : Set} → ({P Q : Set} → ((( P → Q) → P) → P)) → P ∨ ¬ P
proof1 peirce = peirce (λ ¬[p∨¬p] → falsity ( ¬[p∨¬p] (inr (λ p → ¬[p∨¬p] (inl p))) ))



proof2 : {P Q : Set} → (P ∨ ¬ P) → (( P → Q) → P) → P
proof2 (inl p) = λ _ → p
proof2 (inr notP) = λ [P→Q]→P → [P→Q]→P (λ p → falsity (notP p))
