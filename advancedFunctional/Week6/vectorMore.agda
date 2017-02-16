data 𝟙 : Set where
 ⋆ : 𝟙

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

-- Define Vec'' X n to be X × ... × X × 1 and define all vector
-- operations for this type (empty vector, head, tail, cons). Show
-- that Vec'' X n is isomorphic to Vec X n.
