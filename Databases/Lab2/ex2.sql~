/*#Ex 2 

5.*/

SELECT title, avg from (SELECT title, avg(rating) FROM review INNER JOIN album on review.albumid=album.albumid group by title) as average where avg >3;

/*6*/

SELECT title, avg from (SELECT title, avg(rating) FROM review INNER JOIN album on review.albumid=album.albumid group by title) as average where avg > (SELECT avg(rating) FROM review);

/*7*/

SELECT (fname || ' ' || lname) as name from customer where custid in 
	(SELECT custid from review where text is null);

/*8*/

select count(albumid) as NoOfAlbums, count(DISTINCT genre) as NoOfGenres from album;

/*9*/

select (five.count*100)/rev.count as percentage from (select count(*) from review) as rev, (select count(*) from review where rating = '5') as five;

/*10*/
/* choose the right columns*/
Select 
	CASE
		WHEN id15 IS NULL
			THEN id16
		ELSE id15
	END AS AlbumID,
	CASE
		WHEN id15 IS NULL
			THEN count16
		WHEN id16 IS NULL
			THEN count15
		ELSE count15 + count16
	END AS TotalSales
FROM 	(SELECT albumid as id15, count(albumid) as count15 from sale15 group by albumid) 
	as fifteen 
	full join 
	(SELECT albumid as id16, count(albumid) as count16 from sale16 group by albumid order by albumid desc) 
	as sixteen 
	on id16=id15 ;

/*11*/

Select sixteen.genre, sum(c15+c16)
FROM
	(SELECT genre, count(genre) as c15 from sale15 natural join album WHERE genre IS  NOT NULL group by genre) as fifteen
	FULL JOIN
	(SELECT genre, count(genre) as c16 from sale16 natural join album WHERE genre IS NOT NULL group by genre) as sixteen
	ON fifteen.genre = sixteen.genre
WHERE c15 IS NOT NULL AND c15 IS NOT NULL
Group by sixteen.genre
ORDER BY SUM(c15+c16) DESC
LIMIT 1
;




















