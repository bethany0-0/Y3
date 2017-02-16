      ==================
      Dependent equality
      ==================

      Advanced Functional Programming
      School of Computer Science
      University of Birmingham, UK

      Martín Hötzel Escardó
      Lecture of 6rd February 2017

      (Adapted from http://www.cs.bham.ac.uk/~mhe/agda/VecConcatAssoc.html)

      (The ideas discussed here come from "univalent foundations" and
      "homotopy type theory", but you don't need to know that in order
      to understand what is explained here.
      https://homotopytypetheory.org/book/)

Summary
=======

1. It is relatively easy to prove that list concatenation is
associative:

  (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs).

You have already seen this, and we will quickly review it.

2. When we generalize lists to vectors, then this becomes a
problem. It is not that this is difficult to *prove*. Rather, it
becomes difficult to even figure out *what needs to be proved*: The
above equation doesn't type check, when xs, ys, zs are vectors rather
than lists, because its left and right sides live in different types,
as we shall see.

3. But once we know what to prove, the proof becomes just as easy as
that of associativity of list concatenation.

4. What we need is a concept of dependent equality: an equality that
depends on another equality. This is what we study in these notes.

This is a literate agda file
============================

This file (ending in .lagda rather than .agda) is in literate
style. This means that everything is a "comment", except things
enclosed in a code environment.

Getting started
===============

This file is self-contained. We don't use libraries so that we know
exactly what is used for this task from first principles, and also so
that we can see explicitly that this can be done without much
machinery. (Although it requires a lot of explanation and
understanding effort.)

\begin{code}

{-# OPTIONS --without-K #-}

module DependentEquality where

\end{code}

Standard stuff for equality, which you have already seen:

\begin{code}

data _≡_ {X : Set} : X → X → Set where
 refl : {x : X} → x ≡ x

cong : {X Y : Set} (f : X → Y) {x₀ x₁ : X} → x₀ ≡ x₁ → f x₀ ≡ f x₁
cong {X} {Y} f (refl {x}) = refl {Y} {f x}

\end{code}

You have already defined lists and concatenation, and shown that
concatenation is associative. Here is a recap:

\begin{code}

module recap where

  data List (X : Set) : Set where
    []  : List X
    _∷_ : X → List X → List X

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

\end{code}

We have put the above in a separate submodule so that it doesn't
pollute our global name space and in particular the names _++_ and
++-assoc remain available for vector concatenation and associativity.

Standard stuff for natural numbers:

\begin{code}

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

\end{code}

Notice how the proof of associativity of addition is analogous to the
proof of associativity of concatenation.

Vectors
=======

Sometimes it is useful to consider lists of a given length. These are
called vectors, and the type of vectors of elements of type X with
length n is written Vec X n and defined as follows:

\begin{code}

data Vec (X : Set) : ℕ → Set where
  []  : Vec X zero
  _∷_ : ∀{n} → X → Vec X n → Vec X (succ n)

\end{code}

For example, with this we can define safe head and tail functions:

\begin{code}

hd : {X : Set} {n : ℕ} → Vec X (succ n) → X
hd (x ∷ xs) = x

tl : {X : Set} {n : ℕ} → Vec X (succ n) → Vec X n
tl (x ∷ xs) = xs

\end{code}

Notice that we don't have a case for the empty vector [] because it
doesn't belong to the type Vec X (succ n).

We can also define a safe indexing operation. We first need to define,
given a natural number n : ℕ, the type of numbers from 0 to n-1, which
is written Fin n. We do this so that Fin 0 is an empty type:

\begin{code}

data Fin : ℕ → Set where
 zero : {n : ℕ} → Fin (succ n)
 succ : {n : ℕ} → Fin n → Fin (succ n)

\end{code}

Now, given a vector xs of length k and a number in i the range
0,1,..,n,n-1, that is, an element of Fin n, we can fetch the ith
element of xs in a type-safe way:

\begin{code}

fetch : ∀ {X n} → Vec X n → Fin n → X
fetch (x ∷ xs)  zero    = x
fetch (x ∷ xs) (succ i) = fetch xs i

\end{code}

Vector concatenation is defined in the same way as list concatenation,
but with a different type:

\begin{code}

_++_ : ∀{X m n} → Vec X m → Vec X n → Vec X (m + n)
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

\end{code}

Dependent equality of Vectors
=============================

We now come to the heart of the problem considered here, the
formulation and proof of associativity of vector concatenation.

We try to prove this (uncomment to get an error):

\begin{code}

-- ++-assoc' : ∀{X} l m n (xs : Vec X l) (ys : Vec X m) (zs : Vec X n)
--          → (xs ++ ys) ++ zs  ≡ xs ++ (ys ++ zs)
-- ++-assoc' l m n xs ys zs = ?

\end{code} 

We get the cryptic error message 

  l != l + m of type ℕ
  when checking that the expression xs ++ (ys ++ zs) has type
  Vec X (l + m + n)

What is going on? We have

  (xs ++ ys) ++ zs : Vec X ((l + m) + n),
  xs ++ (ys ++ zs) : Vec X (l + (m + n)).

While we do have 

  (l + m) + n ≡ l + (m + n),

as proved above, we don't get that the types

  Vec X ((l + m) + n) and Vec X (l + (m + n))

are the same, which is perhaps counter-intuitive. 

We know that (l + m) + n ≡ l + (m + n) because we have proved it, but
when Agda type checks your code it doesn't use this equality. Why?

The only equalities Agda uses when type checking code are those that
it knows in advance, and that are built-in in the type checker, before
we write any code proving more equalities.

And the above associativity equality is not built-in: we had to prove
it. And it required some insight.

The only equalities that Agda uses when type checking your code are
those which are routine and don't require any insight whatsoever,
which can be proved mechanically, and which you could therefore call
obvious.

Hence the expressions

  (xs ++ ys) ++ zs and xs ++ (ys ++ zs)

live in different types as far as Agda is concerned.

But equality is only defined for elements of the same type. Hence the
two expressions cannot be equated.

To overcome this, we can explicitly tell Agda to use an equality we
have proved.

If we have

  p  : m ≡ n
  xs : Vec X m
  ys : Vec X n

we can define an equality

  xs ≡[ p ] ys

of xs and ys depending on the given equality p.

As usual, we define this by pattern matching on p. The neat thing is
that when p is refl, then Agda knows that xs and ys do live in the
same type, and hence we can use the usual equality to define our new
dependent equality:

\begin{code}

_≡[_]_ : ∀{X m n} → Vec X m → m ≡ n → Vec X n → Set
xs ≡[ refl ] ys   =   xs ≡ ys

\end{code}

Later we will generalize the above definition, because it actually has
little to do with vectors and is useful in many other similar
situations (which we will not explore in these notes).

The following generalizes our cong function to this situation, but
here we only consider the situation where the parameter to cong is
the "cons" function λ ws → x ∷ ws, which is what we needed above in
order to prove associativity of list concatenation, and which we will
need again to prove associativity of vector concatenation:

\begin{code}

cong-cons : ∀{X m n} (x : X) {xs : Vec X m} {ys : Vec X n} (p : m ≡ n)
          → xs ≡[ p ] ys → x ∷ xs  ≡[ cong succ p ]  x ∷ ys
cong-cons _ refl refl = refl 

\end{code}

Notice that if p : m ≡ n, then cong succ p : succ m ≡ succ n, and that
if

  xs : Vec X m and ys : Vec X n

then

  x ∷ xs : Vec X (succ m) and x ∷ ys : Vec X (succ n).

This is why from

  xs ≡[ p ] ys

we get

  x ∷ xs ≡[ cong succ p ] x ∷ ys

in the above definition.

So what we want to prove is that

  (xs ++ ys) ++ zs  ≡[ p ]  xs ++ (ys ++ zs)

for a suitable p : (l + m) + n ≡ l + (m + n), which we can take to be
+-assoc l m n. 

This statement does type check, and its proof is essentially the same
as that for the case of lists, recalled above. The crucial thing is
that now we need to use the fact that addition of natural numbers is
associative, as discussed above.

And with these remarks we are done. Here is the formulation and proof
that vector concatenation is associative:

\begin{code}

++-assoc : ∀{X} l m n (xs : Vec X l) (ys : Vec X m) (zs : Vec X n)
         → (xs ++ ys) ++ zs  ≡[ +-assoc l m n ]  xs ++ (ys ++ zs)
++-assoc zero     m n []       ys zs = refl
++-assoc (succ l) m n (x ∷ xs) ys zs = goal
 where
  IH : (xs ++ ys) ++ zs  ≡[ +-assoc l m n ]  xs ++ (ys ++ zs)
  IH = ++-assoc l m n xs ys zs
  goal : x ∷ ((xs ++ ys) ++ zs)  ≡[ cong succ (+-assoc l m n) ]
         x ∷ (xs ++ (ys ++ zs))
  goal = cong-cons x (+-assoc l m n) IH

\end{code}

If you have begun to understand these subtleties, then I have achieved
my learning objective.

Dependent equality more generally
=================================

But, to conclude, I am a little bit more ambitious.

We want to briefly generalize the above. Notice that

  Vec X : ℕ → Set.

We replace "Vec X" by an arbitrary "B", and "ℕ" by an arbitrary "A":

  A : Set,
  B : A → Set.

Then B is a family of sets indexed by A. In our main example,

  A = ℕ,
  B = Vec X.

Our generalized dependent equality is

  b₀ ≡⟦ p ⟧ b₁

for

  a₀ a₁ : A,
     p  : a₀ ≡ a₁,
     b₀ : B a₀,
     b₁ : B a₁.

The types are more general, but the definition is the same as in the
particular case:

\begin{code}

_≡⟦_⟧_ : {A : Set} {B : A → Set} {a₀ a₁ : A}
       → B a₀ → a₀ ≡ a₁ → B a₁ → Set
b₀ ≡⟦ refl ⟧ b₁   =   b₀ ≡ b₁

\end{code}

We then generalize cong defined above to a dependent version:

\begin{code}

congd : {A : Set} {B : A → Set} (f : (a : A) → B a) {a₀ a₁ : A}
     → (p : a₀ ≡ a₁) → f a₀ ≡⟦ p ⟧ f a₁
congd f refl = refl

\end{code}

Conclusion
==========

Equality is subtler than it seems at first sight, but with the right
concepts and tools, and with practical experience, it becomes natural.

Of course there is much more to say, and the systematic study of
(dependent or independent) equality in dependent type theory is the
topic of "univalent foundations" or "homotopy type theory", which is
beyond the scope of this module: https://homotopytypetheory.org/book

But in the past we have had successful reading groups and clubs with
undergrad, MSc, and PhD students, as well as with academic staff.

--------------------------------------------------------------------
Notational appendix. To complete our code so that it parses, we list
all fixities together for easy comparison of the operator precedences:

\begin{code}

infixr 5 _≡_
infixr 5 _≡[_]_
infixr 5 _≡⟦_⟧_
infixl 6 _++_
infixl 6 _+_
infixr 7 _∷_

\end{code}
