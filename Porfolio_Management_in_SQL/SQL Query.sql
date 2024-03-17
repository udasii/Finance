USE invest;

-- STEP 1: Identify your client (customer# 148, Paul Bistre) in your database - learn about your client and what they have.
SELECT p.date, p.ticker, a.account_id, 
       p.value AS Price, p.price_type, 
       SUM(p.value*quantity) AS Market_Value, 
       s.sec_type, s.major_asset_class, s.minor_asset_class
FROM pricing_daily_new p
INNER JOIN security_masterlist s
ON p.ticker = s.ticker
INNER JOIN holdings_current h
ON s.ticker = h.ticker
INNER JOIN account_dim a
ON h.account_id = a.account_id
INNER JOIN customer_details c
ON a.client_id = c.customer_id
WHERE p.price_type = 'Adjusted' AND 
	 c.customer_id = '148' AND
     p.date = '2022-09-09'
GROUP BY ticker;

SELECT s.major_asset_class AS Asset_Class, COUNT(DISTINCT s.ticker) AS assets, SUM(p.value*quantity) AS Market_Value
FROM pricing_daily_new p
INNER JOIN security_masterlist s
ON p.ticker = s.ticker
INNER JOIN holdings_current h
ON s.ticker = h.ticker
INNER JOIN account_dim a
ON h.account_id = a.account_id
INNER JOIN customer_details c
ON a.client_id = c.customer_id
WHERE p.price_type = 'Adjusted' AND 
	 c.customer_id = '148' AND
     p.date = '2022-09-09'
GROUP BY s.major_asset_class;

-- STEP 2: use the above data to create a VIEW in the invest schema with data for your client
CREATE VIEW Suraj_Udasi_4 AS
SELECT z.ticker, z.date, z.Price, z.price_type 
FROM
(
SELECT p.date, p.ticker, a.account_id, p.value AS Price, p.price_type 
FROM pricing_daily_new p
INNER JOIN security_masterlist s
ON p.ticker = s.ticker
INNER JOIN holdings_current h
ON s.ticker = h.ticker
INNER JOIN account_dim a
ON h.account_id = a.account_id
INNER JOIN customer_details c
ON a.client_id = c.customer_id
WHERE p.price_type = 'Adjusted' AND 
	 c.customer_id = '148' AND
     p.date > '2016-09-01') z;

SELECT *
FROM Suraj_Udasi_4;

-- Q1: Calculating Returns, individual tickers
CREATE VIEW returns_SU1 AS
SELECT z.ticker, z.date, (z.p1-z.p0_daily)/z.p0_daily AS returns_daily,
						 (z.p1-z.p0_monthly)/z.p0_monthly AS returns_monthly,
						 (z.p1-z.p0_12M)/z.p0_12M AS returns_12M, 
						 (POWER(1 + ((z.p1-z.p0_18M)/z.p0_18M), 12/18) - 1) AS 18M_ret, 
					     (POWER(1 + ((z.p1-z.p0_24M)/z.p0_24M), 12/24) - 1) AS 24M_ret
FROM
(
SELECT date, ticker, Price AS p1, LAG(Price, 1) OVER(
														PARTITION BY ticker
                                                        ORDER BY date
                                                        ) AS p0_daily
								   ,LAG(Price, 21) OVER(
														PARTITION BY ticker
                                                        ORDER BY date
                                                        ) AS p0_monthly
                                   ,LAG(Price, 250) OVER(
														PARTITION BY ticker
                                                        ORDER BY date
                                                        ) AS p0_12M
								   ,LAG(Price, 375) OVER(
														PARTITION BY ticker
                                                        ORDER BY date
                                                        ) AS p0_18M
								   ,LAG(Price, 500) OVER(
														PARTITION BY ticker
                                                        ORDER BY date
                                                        ) AS p0_24M
FROM Suraj_Udasi_4
WHERE date > '2017-08-01'
) z;

SELECT *
FROM returns_SU1
WHERE date = '2022-09-09'
GROUP BY ticker;

-- Q1: Calculating reutnrs for the portfolio

CREATE VIEW portfolio_SU3 AS
SELECT a.account_id, p.ticker, AVG(p.value) AS value, SUM(h.quantity) AS quantity
FROM pricing_daily_new p
INNER JOIN security_masterlist s
ON p.ticker = s.ticker
INNER JOIN holdings_current h
ON s.ticker = h.ticker
INNER JOIN account_dim a
ON h.account_id = a.account_id
INNER JOIN customer_details c
ON a.client_id = c.customer_id
WHERE p.price_type = 'Adjusted' AND 
	 c.customer_id = '148' AND
     p.date = '2022-09-09'
GROUP BY p.ticker;

-- CREATE VIEW mktvalue_SU AS
SELECT ticker, (value)*(quantity) AS mktvalue
FROM portfolio_SU3
GROUP BY ticker;

-- CREATE VIEW total_mktvalue_SU AS
SELECT sum(mktvalue) AS total
FROM mktvalue;

-- CREATE VIEW portfolio_weights_SU AS
SELECT m.ticker, m.mktvalue/t.total AS weights
FROM mktvalue m, total_mktvalue_1 t
GROUP BY ticker;

-- CALCULATING PORTFOLIO RETURNS
CREATE VIEW SU_calc_pret AS
SELECT *
FROM returns_SU1
WHERE date = '2022-09-09'
GROUP BY ticker;

-- CREATE VIEW porfolio_returns_SU2 AS
SELECT p.ticker, SUM(p.returns_12M*w.weights) AS 12M_pret, 
				 SUM(p.18M_ret*w.weights) AS 18M_pret, 
                 SUM(p.24M_ret*w.weights) AS 24M_pret
FROM SU_calc_pret p 
INNER JOIN portfolio_weights_SU w
ON p.ticker = w.ticker;

-- Q2: What is the most recent 12months sigma (risk) for each of the securities? What is the average daily return for each of the securities? 

SELECT *
FROM returns_SU1;

SELECT r.ticker, AVG(r.returns_daily) AS Avg_daily_return, AVG(r.returns_12M) AS Avg_return, STD(r.returns_12M) AS Sigma, 
											AVG(r.returns_12M)/STD(r.returns_12M) AS Risk_adj_returns,
                                            s.major_asset_class
FROM returns_SU1 r
LEFT JOIN security_masterlist s
ON r.ticker = s.ticker
GROUP BY ticker
ORDER BY Risk_adj_returns DESC;

-- Q3 - Suggest adding a new investment to your portfolio - what would it be and how much risk (sigma) would it add to your client?

CREATE VIEW new_sec_SU_12M AS
SELECT ticker, date, (p1-p0)/p0 as ret
FROM
(
SELECT ticker, date, value as p1, LAG(value, 250) OVER(PARTITION BY ticker
                                                        ORDER BY date
                                                        ) as p0
FROM pricing_daily_new p
WHERE p.price_type = 'Adjusted' AND
date > '2017-08-01') n;

SELECT c.ticker, AVG(c.ret) AS Avg_return, STD(c.ret) AS Sigma, 
											AVG(c.ret)/STD(c.ret) AS Risk_adj_returns,
                                            s.major_asset_class
FROM new_sec_SU_12M c
LEFT JOIN security_masterlist s
ON c.ticker = s.ticker
GROUP BY ticker
ORDER BY Risk_adj_returns DESC
LIMIT 10;