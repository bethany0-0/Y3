/*#Ex 2 

1. Write a query to find the titles, artists and label of albums that do have a genre specified.*/

SELECT title, artist, label FROM album WHERE genre IS NULL;

/*2. Write a query to find the album IDs of albums sold in the first half of 2015 and the first half of 2016 to
customers 5 and 14.*/

select albumid, saledate from sale15 where saledate < '2015-07-01'::date and (custid='5' or custid='14') union select albumid, saledate from sale16 where saledate < '2016-07-01'::date and (custid='5' or custid='14');

/*could do with a different join?

3. Write a query to find the customer IDs of customers who bought an album in the last quarter of 2015 but
not in the last quarter of 2016.*/
 
select custid from sale15 where saledate >= '2015-10-01'::date and custid not IN (select albumid from sale16 where saledate < '2016-04-01'::date);

/*4. Write a query to find the average rating of each album that has been rated. Group by album ID. Show
the average rating to 2 decimal places.*/

select albumid, round(avg(rating)::numeric,2) from review group by albumid;

/*5. Write a query to find the album title and average rating of albums that have an average rating greater
than 3.*/

SELECT title, avg from (SELECT title, avg(rating) FROM review INNER JOIN album on review.albumid=album.albumid group by title) as average where avg >3;

/*6 Write a query to find the album title and average rating of albums that have a higher average rating than
the overall average rating.*/

SELECT title, avg from (SELECT title, avg(rating) FROM review INNER JOIN album on review.albumid=album.albumid group by title) as average where avg > (SELECT avg(rating) FROM review);

/*7 Write a query to find the full name of all users who have rated an album but not left a comment. The
name should appear in the result in the format first-name<space>last-name and the attribute should be
called ’name’.*/

SELECT (fname || ' ' || lname) as name from customer where custid in 
	(SELECT custid from review where text is null);

/*8. Write a query to find, in one table, the number of albums and the number of distinct genres in the album
table. Rename the attributes with suitable names.*/

select count(albumid) as NoOfAlbums, count(DISTINCT genre) as NoOfGenres from album;

/*9. Write a query to find the percentage of albums that have been rated that have a rating of 5.*/

select (five.count*100)/rev.count as percentage from (select count(*) from review) as rev, (select count(*) from review where rating = '5') as five;

/*10. Write a query to find the album ID and number of sales of all albums that sold in 2015 and 2016. The
result should be one table with the combined sales.*/
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

/*11. Write a query to find the best-selling album genre in 2015 and 2016 combined. Your query should ignore
null values.*/

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




















