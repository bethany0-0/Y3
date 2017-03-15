------------------------------------------------------------------------

-- lab for week 10
-- LL machine soundness proof
-- ht 13.3.2017

------------------------------------------------------------------------

open import Data.List 
open import Data.Sum 

open import grammar-afp

module LL-lab (gram : Grammar) where

open Grammar gram
open Strings gram


-- derivation step A -> alpha with beta on the left and gamma on the right

data derivstep : TNs -> TNs -> Set where
  derv : (beta : TNs) 
      {A : N} 
      {alpha : TNs}
      (r : rules A alpha) 
      (gamma : TNs) 
      -> derivstep (beta ++ [ inj₂ A ] ++ gamma) 
                   (beta ++ alpha ++ gamma)

-- leftmost derivation step A -> alpha with v on the left and gamma on the right

data lm : TNs -> TNs -> Set where
  lmstep : (v : List T)     -- restriction to terminals
      {A : N} {a : List (T ⊎ N)}
      (r : rules A a) 
      (gamma : List (T ⊎ N)) 
      -> lm (Data.List.map inj₁ v ++ [ inj₂ A ] ++ gamma) 
            (Data.List.map inj₁ v ++ a ++ gamma)


-- kleene star for relation

data Kleene {A : Set} (r : A -> A -> Set) : A -> A -> Set where
  emp : {x : A} -> Kleene r x x
  step : {x y z : A}
    -> r x y
    -> Kleene r y z
    -> Kleene r x z


-- the language of a grammar = string derivable from start symbol

data inLanguage : List T -> Set where
  derivablefromstart : {w : List T}
    -> (Kleene derivstep [ inj₂ S ] (TtoTN w))
    -> inLanguage w  

data LLstate : Set where
  LL :  List (T ⊎ N) -> List T -> LLstate

data LLstep : LLstate -> LLstate -> Set where
  match : (pi : List (T ⊎ N)) 
     (w : List T) 
     (c : T) 
    -> LLstep (LL ((inj₁ c) ∷ pi) (c ∷ w))  
              (LL pi w) 
  predict : {A : N} 
    {alpha : List (T ⊎ N)}
    (r : rules A alpha) 
    (pi : List (T ⊎ N)) 
    (w : List T) 
    -> LLstep (LL ((inj₂ A) ∷ pi) w) 
              (LL (alpha ++ pi) w)
              
LLinitial : List T -> LLstate
LLinitial w = LL [ inj₂ S ] w
  
LLfinal : LLstate
LLfinal = LL [] []

-- words accepted by LL machine

data LLaccepted : List T -> Set where
  acc : {w : List T}
    -> Kleene LLstep (LLinitial w) LLfinal
    -> LLaccepted w


-- tensor on a single terminal symbol from the left onto a leftmost derivation

lo1 : {phi psi : TNs}
  (b : T)
  -> (lm phi psi) 
  -> (lm ([ inj₁ b ] ++ phi) 
         ([ inj₁ b ] ++ psi)) 
lo1 b (lmstep v r gamma) = lmstep (b ∷ v) r gamma

-- tensor on a string of terminal symbols from the left onto a leftmost derivation

lefttensorlmstep : {phi psi : TNs}
  (v1 : List T)
  -> (lm phi psi) 
  -> (lm ((TtoTN v1) ++ phi) 
         ((TtoTN v1) ++ psi)) 
lefttensorlmstep [] p = p
lefttensorlmstep (hd ∷ tl) p = lo1 hd (lefttensorlmstep tl p)

-- left tensor on leftmost derivations

lefttensorleftmost : {phi psi :  TNs}
  (v : List T)
  -> (Kleene lm phi psi) 
  -> (Kleene lm ((TtoTN v) ++ phi) 
                ((TtoTN v) ++ psi))
lefttensorleftmost v emp = emp
lefttensorleftmost v (step x p) = step (lefttensorlmstep v x) (lefttensorleftmost v p)

--LL soundness functor on objects

LLsoundobj : LLstate -> Set
LLsoundobj (LL x p) = {!!}

-- LL soundness main lemma for single step
-- continuation passing style

LLsoundstepmor : {s1 s2 : LLstate}
  -> (LLstep s1 s2)
  -> (LLsoundobj s2)
  -> (LLsoundobj s1)           
LLsoundstepmor = {!!}

LLsoundfun : {s1 s2 : LLstate}
  -> (Kleene LLstep s1 s2)
  -> (LLsoundobj s2)
  -> (LLsoundobj s1)    
LLsoundfun = {!!}

LLsoundpredict : forall {pi w}
  -> Kleene LLstep (LL pi w) LLfinal
  -> Kleene derivstep pi (TtoTN w)
LLsoundpredict mr = {!!}

-- soundness theorem: accepting LL run => derivation in grammar

LLsoundThm : (w : List T)
  -> LLaccepted w
  -> inLanguage w
LLsoundThm = {!!}

