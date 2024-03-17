USE H_Accounting;

-- A stored procedure, or a stored routine, is like a function in other programming languages
-- We write the code once, and the code can de reused over and over again
-- We can pass on arguments into the stored procedure. i.e. we can give a specific input to a store procedure
-- For example we could determine the specific for which we want to produce the profit and loss statement


#  FIRST thing you MUST do whenever writting a stored procedure is to change the DELIMTER
#  The default deimiter in SQL is the semicolon ;
#  Since we will be using the semicolon to start and finish sentences inside the stored procedure
#  The compiler of SQL won't know if the semicolon is closing the entire Stored procedure or an line inside
#  Therefore, we change the DELIMITER so we can be explicit about whan we are closing the stored procedure, vs. when
#  we are closing a specific Select  command

DROP PROCEDURE IF EXISTS `t4_balsheet_sp`;

-- The tpycal delimiter for Stored procedures is a double dollar sign
DELIMITER $$
CREATE PROCEDURE `t4_balsheet_sp`(varCalendarYear SMALLINT)
BEGIN
    -- We receive as an argument the year for which we will calculate the revenues
    -- This value is stored as an 'YEAR' type in the variable `varCalendarYear`
    -- It could be confusing which are schema fields from a table vs. which are argument variables
    -- Therefore, a good practice is to adopt a naming convention for all variables
    -- In these lines of code we are naming prefixing every variable as "var"

    DECLARE varCAThisYear DOUBLE DEFAULT 0;
    DECLARE varCALastYear DOUBLE DEFAULT 0;
    DECLARE varFAThisYear DOUBLE DEFAULT 0;
    DECLARE varFALastYear DOUBLE DEFAULT 0;
    DECLARE varDAThisYear DOUBLE DEFAULT 0;
    DECLARE varDALastYear DOUBLE DEFAULT 0;
    DECLARE varCLThisYear DOUBLE DEFAULT 0;
    DECLARE varCLLastYear DOUBLE DEFAULT 0;
    DECLARE varLLLThisYear DOUBLE DEFAULT 0;
    DECLARE varLLLLastYear DOUBLE DEFAULT 0;
    DECLARE varDLThisYear DOUBLE DEFAULT 0;
    DECLARE varDLLastYear DOUBLE DEFAULT 0;
    DECLARE varEQThisYear DOUBLE DEFAULT 0;
    DECLARE varEQLastYear DOUBLE DEFAULT 0;


    -- WE NOW CALCULATE THE CURRENT ASSETS
    SELECT (sum(jeli.debit) - sum(jeli.credit))
    INTO varCAThisYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "CA"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear + 1;


    SELECT (sum(jeli.debit) - sum(jeli.credit))
    INTO varCALastYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "CA"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear;

    -- WE NOW CALCULATE THE FIXED ASSETS
    SELECT (sum(jeli.debit) - sum(jeli.credit))
    INTO varFAThisYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "FA"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear + 1;


    SELECT (sum(jeli.debit) - sum(jeli.credit))
    INTO varFALastYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "FA"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear;

-- WE NOW CALCULATE THE DEF ASSETS
    SELECT (sum(jeli.debit) - sum(jeli.credit))
    INTO varDAThisYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "DA"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear + 1;


    SELECT (sum(jeli.debit) - sum(jeli.credit))
    INTO varDALastYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "DA"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear;

-- WE NOW CALCULATE THE CURRENT LIABILITIES
    SELECT (sum(jeli.credit) - sum(jeli.debit))
    INTO varCLThisYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "CL"
 AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear + 1;


    SELECT (sum(jeli.credit) - sum(jeli.debit))
    INTO varCLLastYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "CL"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear;

-- WE NOW CALCULATE THE LONG TERM LIABILITIES
    SELECT (sum(jeli.credit) - sum(jeli.debit))
    INTO varLLLThisYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "LLL"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear + 1;


    SELECT (sum(jeli.credit) - sum(jeli.debit))
    INTO varLLLLastYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "LLL"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear;

-- WE NOW CALCULATE THE DEF LIABILITIES
    SELECT (sum(jeli.credit) - sum(jeli.debit))
    INTO varDLThisYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "DL"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear + 1;


    SELECT (sum(jeli.credit) - sum(jeli.debit))
    INTO varDLLastYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "DL"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear;

-- WE NOW CALCULATE THE EQUITY
    SELECT (sum(jeli.credit) - sum(jeli.debit))
    INTO varEQThisYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "EQ"
 AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear + 1;


    SELECT (sum(jeli.credit) - sum(jeli.debit))
    INTO varEQLastYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
    WHERE ss.statement_section_code = "EQ"
       AND je.cancelled = 0
      AND YEAR(je.entry_date) < varCalendarYear;

    -- WE DROP THE TMP, TO INSERT THE VALUES INTO THE TMP FORMATTED
    DROP TABLE IF EXISTS sudasi_tmp;


    -- WE CALCULATE THE GROWTH ON REVENUE vs. LAST YEAR
    CREATE TABLE sudasi_tmp AS
    SELECT "   "                                 AS `Op`,
           "CA"                                  AS 'Balance Sheet Items',
           FORMAT(COALESCE(varCAThisYear, 0), 1) AS 'Current Year ($US)',
           FORMAT(COALESCE(varCALastYear, 0), 1) AS 'Previous Year ($US)',
           CASE
               WHEN COALESCE(varCALastYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT(((COALESCE(varCAThisYear, 0) / varCALastYear) - 1) * 100, 1)
               END                               AS 'YoY Change (%)';

    INSERT INTO sudasi_tmp
    SELECT "(+)",
           "FA",
           FORMAT(COALESCE(varFAThisYear, 0), 1),
           FORMAT(COALESCE(varFALastYear, 0), 1),
           CASE
               WHEN COALESCE(varFALastYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT(((COALESCE(varFAThisYear, 0) / varFALastYear) - 1) * 100, 1)
               END;

    INSERT INTO sudasi_tmp
    SELECT "(+)",
           "DA",
           FORMAT(COALESCE(varDAThisYear, 0), 1),
           FORMAT(COALESCE(varDALastYear, 0), 1),
           CASE
               WHEN COALESCE(varDALastYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT(((COALESCE(varDAThisYear, 0) / varDALastYear) - 1) * 100, 1)
               END;

    INSERT INTO sudasi_tmp
    SELECT "(=)",
           "TA",
           FORMAT(COALESCE(varCAThisYear, 0) + COALESCE(varFAThisYear, 0) + COALESCE(varDAThisYear, 0), 1),
           FORMAT(COALESCE(varCALastYear, 0) + COALESCE(varFALastYear, 0) + COALESCE(varDALastYear, 0), 1),
           CASE
               WHEN COALESCE(varCALastYear, 0) + COALESCE(varFALastYear, 0) + COALESCE(varDALastYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT((((COALESCE(varCAThisYear, 0) + COALESCE(varFAThisYear, 0) + COALESCE(varDAThisYear, 0)) /
                             (COALESCE(varCALastYear, 0) + COALESCE(varFALastYear, 0) + COALESCE(varDALastYear, 0))) -
                            1) * 100, 1)
               END;

    INSERT INTO sudasi_tmp
    SELECT "",
           "",
           "",
           "",
           "";

    INSERT INTO sudasi_tmp
    SELECT "",
           "CL",
           FORMAT(COALESCE(varCLThisYear, 0), 1),
           FORMAT(COALESCE(varCLLastYear, 0), 1),
           CASE
               WHEN COALESCE(varCLLastYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT(((COALESCE(varCLThisYear, 0) / varCLLastYear) - 1) * 100, 1)
               END;

    INSERT INTO sudasi_tmp
    SELECT "(+)",
           "NL",
           FORMAT(COALESCE(varLLLThisYear, 0), 1),
           FORMAT(COALESCE(varLLLLastYear, 0), 1),
           CASE
               WHEN COALESCE(varLLLLastYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT(((COALESCE(varLLLThisYear, 0) / varLLLLastYear) - 1) * 100, 1)
               END;

    INSERT INTO sudasi_tmp
    SELECT "(+)",
           "DL",
           FORMAT(COALESCE(varDLThisYear, 0), 1),
           FORMAT(COALESCE(varDLLastYear, 0), 1),
           CASE
               WHEN COALESCE(varDLLastYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT(((COALESCE(varDLThisYear, 0) / varDLLastYear) - 1) * 100, 1)
               END;

 INSERT INTO sudasi_tmp
    SELECT "(+)",
           "EQ",
           FORMAT(COALESCE(varEQThisYear, 0), 1),
           FORMAT(COALESCE(varEQLastYear, 0), 1),
           CASE
               WHEN COALESCE(varEQLastYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT(((COALESCE(varEQThisYear, 0) / varEQLastYear) - 1) * 100, 1)
               END;

    INSERT INTO sudasi_tmp
    SELECT "",
           "",
           "",
           "",
           "";


    INSERT INTO sudasi_tmp
    SELECT "(=)",
           "EL",
           FORMAT(COALESCE(varCLThisYear, 0) + COALESCE(varLLLThisYear, 0) + COALESCE(varDLThisYear, 0) + COALESCE(varEQThisYear, 0), 1),
           FORMAT(COALESCE(varCLLastYear, 0) + COALESCE(varLLLLastYear, 0) + COALESCE(varDLLastYear, 0) + COALESCE(varEQLastYear, 0), 1),
           CASE
               WHEN COALESCE(varCLLastYear, 0) + COALESCE(varLLLLastYear, 0) + COALESCE(varDLLastYear, 0) + COALESCE(varEQLastYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT((((COALESCE(varCLThisYear, 0) + COALESCE(varLLLThisYear, 0) + COALESCE(varDLThisYear, 0) + COALESCE(varEQThisYear, 0)) /
                             (COALESCE(varCLLastYear, 0) + COALESCE(varLLLLastYear, 0) + COALESCE(varDLLastYear, 0) + COALESCE(varEQLastYear, 0))) -
                            1) * 100, 1)
               END;


END $$
DELIMITER ;

CALL t4_balsheet_sp(2020);

Select *
from sudasi_tmp;