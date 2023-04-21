

SELECT  nameFirst, nameLast, birthYear, birthCity
FROM players
WHERE birthYear != 1866 AND birthCity LIKE 'new%';  -- % is 임의의 문자열 _ 1개의 문자

/* SELECT data
	FROM table
	WHERE condition*/


SELECT TOP 10 PERCENT  *
FROM dbo.players
WHERE birthYear IS NOT NULL
ORDER BY birthYear DESC, birthMonth ASC, birthDay  ASC;  --ASC adn DESC


--we want 100- 200
SELECT  *
FROM dbo.players
WHERE birthYear IS NOT NULL
ORDER BY birthYear DESC, birthMonth ASC, birthDay  ASC  --ASC adn DESC
OFFSET 100 ROWS FETCH NEXT 100 ROWS ONLY;


SELECT 2021 - birthYear AS K_AGE
FROM players
WHERE deathYear IS NULL AND birthYear IS NOT NULL AND (2021-birthYear) <= 100
ORDER BY K_AGE DESC;


 -- UNICODE need N'String'
SELECT N'샤이무지';

SELECT SUBSTRING('20230421',1,4); --get  20230421[1:4]

SELECT nameFIRST + ' ' + NAMELast AS nameFull
FROM players
WHERE nameFirst IS NOT NULL AND nameLast IS NOT NULL;


