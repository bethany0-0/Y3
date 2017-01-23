-- neg == to empty
-- dis intro == L | R 
-- conj into == tuple creation
-- conj elim == fst/snd
-- dis elim == pattern matching both sides
-- imp intro == function from hypothesis to implied - from pattern matching to output
------imp into --- left side of lambda function is hypothesis
-- imp elim == application of the function

data Empty

data OR a b = L a | R b

--data Law1 = Not(P And Q) Implies (Not P) Or (Not Q)

data Law1 p q = Law1 (((p , q) -> Empty) -> (OR (p -> Empty) (q -> Empty)))

--data Law2 = (Not P) Or (Not Q) Implies Not(P And Q)

data Law2 p q = Law2 ((OR (p -> Empty) (q -> Empty)) -> ((p, q) -> Empty)) 

--data Law3 = Not(P Or Q) Implies (Not P) And (Not Q)

data Law3 p q = Law3 (((OR p q) -> Empty) -> ((p -> Empty), (q -> Empty)))

--data Law4 = (Not P) And (Not Q) Implies Not(P Or Q)

data Law4 p q = Law4 (((p -> Empty), (q -> Empty)) -> ((OR p q) -> Empty))


--Part 2

--Law2 proof
--made pretty with lambda
----hypothesise (a, b), left is left hand function of or, apply to a will give empty
law2Proof :: Law2 p q
law2Proof = Law2 (\ or -> case or of
                     L left -> (\ (a, b) -> left a)
                     R right -> (\ (a, b) -> right b)
                 )
                       
-- Law3 proof

law3Proof :: Law3 p q
law3Proof = Law3 law3function

----------------(\p -> notOR (L p)), hypothesise p, apply the function notOR to created object or with L p, this will give empty 
law3function :: (((OR p q) -> Empty) -> ((p -> Empty), (q -> Empty)))
law3function notOR = (left3, right3)
  where
    left3 = (\p -> notOR (L p))
    right3 = (\q -> notOR (R q))

--Law4 proof

law4Proof :: Law4 p q
law4Proof = Law4 law4function
---------create an or, match so left side is left function applied to a returns empty
law4function :: (((p -> Empty), (q -> Empty)) -> ((OR p q) -> Empty))
law4function (negA, negB) = (\or -> case or of
                                L a -> negA a
                                R b -> negB b
                            )
                      
--Part 3

--Law1 cannot be constructivly proven

 --Formally the proof for this law is proven by RAA by negagation of the premise. The proof is as follows:
            --1. (p, q) -> empty                                                    P
            --2. (OR (p -> empty) (q -> empty)) -> empty                            H
                 --3. p -> empty                                                    H
                       --4. OR (p -> empty) (q -> empty)                            dis intro 3
                       --5. empty                                                   contradiction 2, 4
                 --6. p -> empty -> epmty                                           RAA 3-5
                 --7. p                                                             RAA 3-5
                 --8. q -> empty                                                    H
                       --9. OR (p -> empty) (q -> empty)                            dis intro 8
                       --10. empty                                                  contradiction 9, 2
                 --11. q -> empty -> empty                                          RAA 8-10
                 --12. q                                                            RAA 8-10
                 --13. (p, q)                                                       conj intro 7, 12
                 --14. empty                                                        contradictions 13, 1
             --15. ((OR (p -> empty) (q -> empty)) -> empty) -> empty               RAA 2-14
             --16. OR (p -> empty) (q -> empty)                                     RAA 2-14
 
-- As stated in the lecture, constructive logic cannot include a double negative elimination. In the above proof lines 6-7, 11-12 and 15-16 all use double negation elimination so this proof cannot be implemented in constructive logic. This proof is allowed in natural logic as neg (neg A) -> A, however constructive logic cannot.

-- Looking at attempting to create a function, functional languages cannot create the flow by hypothesising a alone as it mus be piped though the input function, reuqiring (a, b)

-- law1function  fun ((a, b)-> Empty) =
                               
-- L ((\ a -> (\b ->(\ fun (a, b)))

-- This dictates that a relies on b also being true in the hypotesis thefore (a, b) has been proven not a. a is co-dependant on b because of the premise.


-----Empty problems

undefined = proof4

error "bla" :: Empty
myfun :: (Empty -> Empty) -> Empty
myfun f = f (myfun f)

fun :: Empty -> Empty
fun a = a

myfun fun = Empty

false :: A -> Empty

undefined :: Empty -> A
undefined = false
