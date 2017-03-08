------------------------------------------------------------------------

-- kleene closure of relations and related definitions
-- for AFP lab
-- ht 5.3.2017

------------------------------------------------------------------------

module kleene-holes where

-- kleene star for relation

data Kleene {A : Set} (r : A -> A -> Set) : A -> A -> Set where
  emp : {x : A} -> Kleene r x x
  step : {x y z : A}
    -> r x y
    -> Kleene r y z
    -> Kleene r x z

-- sequentially compose two Kleene stars

composekleene : {A : Set} {r : A -> A -> Set} {x y z : A}
  -> Kleene r x y
  -> Kleene r y z
  -> Kleene r x z
composekleene emp y = y
composekleene (step x1 x2) y = step x1 (composekleene x2 y)
  
-- functor from runs to derivations
-- extend from relation to its Kleene closure

extkleene : {A B : Set}
    {r1 : A -> A -> Set}
    {r2 : B -> B -> Set}
    (fobj : A -> B)
    (fmor : forall {a1 a2} -> r1 a1 a2 -> Kleene r2 (fobj a1) (fobj a2))
    {a3 a4 : A}
    (d : Kleene r1 a3 a4)
    -> Kleene r2 (fobj a3) (fobj a4)
extkleene fobj fmor emp = emp
extkleene fobj fmor (step x d) = composekleene (fmor x) (extkleene fobj fmor d)

funkleene :  {A B : Set}
    {r1 : A -> A -> Set}
    {r2 : B -> B -> Set}
    (fobj : A -> B)
    (fmor : forall {a1 a2} -> r1 a1 a2 -> r2 (fobj a1) (fobj a2))
    {a3 a4 : A}
    (d : Kleene r1 a3 a4)
    ->   Kleene r2 (fobj a3) (fobj a4)
funkleene fobj fmor emp = emp
funkleene fobj fmor (step x d) = step (fmor x) (funkleene fobj fmor d)

-- contravariant functor on Kleene

opfun :  {A B : Set}
    {r1 : A -> A -> Set}
    {r2 : B -> B -> Set}
    (fobj : A -> B)
    (fmor : forall {a1 a2} -> r1 a1 a2 -> Kleene r2 (fobj a2) (fobj a1))
    {a3 a4 : A}
    -> Kleene r1 a3 a4
    -> Kleene r2 (fobj a4) (fobj a3)
opfun fobj fmor emp = emp
opfun fobj fmor (step x d) = composekleene (opfun fobj fmor d) (fmor x)

-- tensoring on a type onto a derivation or machine run

otimeskleeneright : {A B : Set}
  (_op_ : A -> B -> A)
  (r : A -> A -> Set)
  (c : B)
  (f : forall {x y} -> r x y -> r (x op c) (y op c))
  -> forall {a d}
  -> Kleene r a d
  -> Kleene r (a op c) (d op c)
-- otimeskleeneright _op_ r c f emp = emp
-- otimeskleeneright _op_ r c f (step x d) = step (f x) (otimeskleeneright _op_ r c f d)

otimeskleeneright _op_ r c f k = funkleene (λ x → x op c) f k

otimeskleeneleft : {A B : Set}
  (_op_ : B -> A -> A)
  (r : A -> A -> Set)
  (c : B)
  (f : forall {x y} -> r x y -> r (c op x) (c op y))
  -> forall {a d}
  -> Kleene r a d
  -> Kleene r (c op a) (c op d)
otimeskleeneleft _op_ r c f k = funkleene (_op_ c ) f k

