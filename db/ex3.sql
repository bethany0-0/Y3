/*Ex3       ./ used for natural join

CREATE TABLE <T1>

1. σ Y >12∧V !=2 (T 1 )

 v | w | x | y  | z 
---+---+---+----+---
 1 | a | x | 15 | e

*/
SELECT *
FROM T1
WHERE Y > 12 AND V != 2;

/*2. Π X,Y (σ W != b (T 1 ))

 x | y  
---+----
 x | 15
 x | 10

*/

SELECT X, Y 
FROM T1
WHERE W != 'b';

/*3. σ D=p (Π X,Y (T 1 ) ./ Π X,D (T 2 ))

 x | y  | d 
---+----+---
 x | 10 | p
 y | 15 | p
 x | 15 | p

 */

SELECT DISTINCT * 
FROM 	(SELECT DISTINCT X, Y
	FROM T1) AS ONE
	NATURAL JOIN
	(SELECT DISTINCT X, D
	FROM T2) AS TWO
WHERE D = 'p';

/*4. Π D (σ V =1∧c65 (T 1 × T 2 )) 

 d 
---
 p
 t
*/

SELECT D 
FROM T1 , T2
WHERE V = 1 AND C <= 5;

/*5 SEE ABOVE */

/*6 Returns the title of the David Bowie albums which cost less than 7*/

SELECT title 
FROM Album
WHERE artist = 'David Bowie' AND price <= 7;

/*7. Returns the customer ID of cutomers that bought an album which cost over 8*/

SELECT Customer.custid
FROM Customer
	NATURAL JOIN Album
	NATURAL JOIN Sale15
WHERE Album.price >= 8;

/*8. See above */

/*9.

SELECT rating, customer.custid
FROM customer, review
WHERE customer.custid = review.custid AND rating = 5;

Π rating, customer.custid (σ customer.custid = review.custid ∧ rating = 5 (customer x review)) 

10.

SELECT album.title
FROM album NATURAL JOIN sale15 NATURAL JOIN customer
WHERE customer.lname = ’Matlock’;

Π album.title (σ customer.lname = ’Matlock’ (album ./ sale15 ./ customer))

11.

SELECT albumid
FROM
((SELECT albumid, artist FROM album WHERE artist = ’The Cure’) AS temp1
NATURAL JOIN
(SELECT albumid FROM review WHERE rating = 1) AS temp2);

Π albumid (
		p temp1 (Π albumid, artist(σ artist = ’The Cure’(album))) 
		./
		p temp2 (Π albumid (σ rating = 1 (review)))
	  )







