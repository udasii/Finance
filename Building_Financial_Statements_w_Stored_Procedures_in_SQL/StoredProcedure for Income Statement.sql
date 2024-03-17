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

DROP PROCEDURE IF EXISTS `t4_pnl_sp`;

-- The tpycal delimiter for Stored procedures is a double dollar sign
DELIMITER $$
CREATE PROCEDURE `t4_pnl_sp`(varCalendarYear SMALLINT)
BEGIN
    -- We receive as an argument the year for which we will calculate the revenues
    -- This value is stored as an 'YEAR' type in the variable `varCalendarYear`
    -- It could be confusing which are schema fields from a table vs. which are argument variables
    -- Therefore, a good practice is to adopt a naming convention for all variables
    -- In these lines of code we are naming prefixing every variable as "var"

    DECLARE varRevenueThisYear DOUBLE DEFAULT 0;
    DECLARE varRevenueLastYear DOUBLE DEFAULT 0;
    DECLARE varCOGSThisYear DOUBLE DEFAULT 0;
    DECLARE varCOGSLastYear DOUBLE DEFAULT 0;
    DECLARE varSellingExpThisYear DOUBLE DEFAULT 0;
    DECLARE varSellingExpLastYear DOUBLE DEFAULT 0;
    DECLARE varGeneralExpThisYear DOUBLE DEFAULT 0;
    DECLARE varGeneralExpLastYear DOUBLE DEFAULT 0;
    DECLARE varAdminExpThisYear DOUBLE DEFAULT 0;
    DECLARE varAdminExpLastYear DOUBLE DEFAULT 0;
    DECLARE varReturnRefundDiscountThisYear DOUBLE DEFAULT 0;
    DECLARE varReturnRefundDiscountLastYear DOUBLE DEFAULT 0;
    DECLARE varOtherIncomeThisYear DOUBLE DEFAULT 0;
    DECLARE varOtherIncomeLastYear DOUBLE DEFAULT 0;
    DECLARE varIncomeTaxThisYear DOUBLE DEFAULT 0;
    DECLARE varIncomeTaxLastYear DOUBLE DEFAULT 0;
    DECLARE varOtherTaxThisYear DOUBLE DEFAULT 0;
    DECLARE varOtherTaxLastYear DOUBLE DEFAULT 0;
    DECLARE varNetIncomeThisYear DOUBLE DEFAULT 0;
    DECLARE varNetIncomeLastYear DOUBLE DEFAULT 0;


    SELECT SUM(jeli.credit)
    INTO varRevenueThisYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id
    WHERE ss.statement_section_code = "REV"
      AND YEAR(je.entry_date) = varCalendarYear;

    -- WE NOW CALCULATE THE REVENUE FROM LAST YEAR
    SELECT SUM(jeli.credit)
    INTO varRevenueLastYear
    FROM journal_entry_line_item AS jeli
             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id
    WHERE ss.statement_section_code = "REV"
      AND YEAR(je.entry_date) = varCalendarYear - 1;

    SELECT SUM(jeli.debit)
    INTO varCOGSThisYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "COGS"
      AND YEAR(je.entry_date) = varCalendarYear;

    SELECT SUM(jeli.debit)
    INTO varCOGSLastYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "COGS"
      AND YEAR(je.entry_date) = varCalendarYear - 1;

    SELECT SUM(jeli.debit)
    INTO varSellingExpThisYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "SEXP"
      AND YEAR(je.entry_date) = varCalendarYear;

    SELECT SUM(jeli.debit)
    INTO varSellingExpLastYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "SEXP"
      AND YEAR(je.entry_date) = varCalendarYear - 1;

    SELECT SUM(jeli.debit)
    INTO varGeneralExpThisYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "OEXP"
      AND YEAR(je.entry_date) = varCalendarYear;

    SELECT SUM(jeli.debit)
    INTO varGeneralExpLastYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "OEXP"
      AND YEAR(je.entry_date) = varCalendarYear - 1;

    SELECT SUM(jeli.debit)
    INTO varAdminExpThisYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "GEXP"
      AND YEAR(je.entry_date) = varCalendarYear;

    SELECT SUM(jeli.debit)
    INTO varAdminExpLastYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "GEXP"
      AND YEAR(je.entry_date) = varCalendarYear - 1;


    SELECT SUM(jeli.debit)
    INTO varReturnRefundDiscountThisYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "RET"
      AND YEAR(je.entry_date) = varCalendarYear;

    SELECT SUM(jeli.debit)
    INTO varReturnRefundDiscountLastYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "RET"
      AND YEAR(je.entry_date) = varCalendarYear - 1;

    SELECT SUM(jeli.credit)
    INTO varOtherIncomeThisYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "OI"
      AND YEAR(je.entry_date) = varCalendarYear;

    SELECT SUM(jeli.credit)
    INTO varOtherIncomeLastYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "OI"
      AND YEAR(je.entry_date) = varCalendarYear - 1;

    SELECT SUM(jeli.debit)
    INTO varIncomeTaxThisYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "INCTAX"
      AND YEAR(je.entry_date) = varCalendarYear;

    SELECT SUM(jeli.debit)
    INTO varIncomeTaxLastYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "INCTAX"
      AND YEAR(je.entry_date) = varCalendarYear - 1;

    SELECT SUM(jeli.debit)
    INTO varOtherTaxThisYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "OTHTAX"
      AND YEAR(je.entry_date) = varCalendarYear;

    SELECT SUM(jeli.debit)
    INTO varOtherTaxLastYear

    FROM journal_entry_line_item AS jeli

             INNER JOIN account AS ac ON ac.account_id = jeli.account_id
             INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
             INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id

    WHERE ss.statement_section_code = "OTHTAX"
      AND YEAR(je.entry_date) = varCalendarYear - 1;


    -- WE DROP THE TMP, TO INSERT THE VALUES INTO THE TMP FORMATTED
    DROP TABLE IF EXISTS sudasi_tmp;


    -- WE CALCULATE THE GROWTH ON REVENUE vs. LAST YEAR
    CREATE TABLE sudasi_tmp AS
    SELECT "   "                                                          AS `Op`,
           "Revenue"                                                      AS 'Income Statement Items',
           FORMAT(varRevenueThisYear, 1)                                  AS 'Current Year ($US)',
           FORMAT(varRevenueLastYear, 1)                                  AS 'Previous Year ($US)',
           FORMAT((varRevenueThisYear / varRevenueLastYear - 1) * 100, 1) AS "YoY Change (%)";

    INSERT INTO sudasi_tmp
    SELECT "(-)",
           "COGS",
           FORMAT(varCOGSThisYear, 1),
           FORMAT(varCOGSLastYear, 1),
           FORMAT((varCOGSThisYear / varCOGSLastYear - 1) * 100, 1);

    INSERT INTO sudasi_tmp
    SELECT "", "", "", "", "";

    INSERT INTO sudasi_tmp
    SELECT "=",
           "GP",
           FORMAT(varRevenueThisYear - varCOGSThisYear, 1),
           FORMAT(varRevenueLastYear - varCOGSLastYear, 1),
           FORMAT(((varRevenueThisYear - varCOGSThisYear) / (varRevenueLastYear - varCOGSLastYear) - 1) * 100, 1);

    INSERT INTO sudasi_tmp
    SELECT "%",
           "Margin",
           FORMAT(((varRevenueThisYear - varCOGSThisYear) / (varRevenueThisYear)) * 100, 1),
           FORMAT(((varRevenueLastYear - varCOGSLastYear) / (varRevenueLastYear)) * 100, 1),
           FORMAT((((varRevenueThisYear - varCOGSThisYear) / (varRevenueThisYear)) * 100) -
                  (((varRevenueLastYear - varCOGSLastYear) / (varRevenueLastYear)) * 100), 1);

    INSERT INTO sudasi_tmp
    SELECT "", "", "", "", "";

    INSERT INTO sudasi_tmp
    SELECT "(-)",
           "S.Exp",
           FORMAT(varSellingExpThisYear, 1),
           FORMAT(varSellingExpLastYear, 1),
           FORMAT((varSellingExpThisYear / varSellingExpLastYear - 1) * 100, 1);

    INSERT INTO sudasi_tmp
    SELECT "(-)",
           "G.Exp",
           FORMAT(varGeneralExpThisYear, 1),
           FORMAT(varGeneralExpLastYear, 1),
           FORMAT((varGeneralExpThisYear / varGeneralExpLastYear - 1) * 100, 1);

    INSERT INTO sudasi_tmp
    SELECT "(-)",
           "A.Exp",
           IFNULL(FORMAT(IFNULL(varAdminExpThisYear, 0), 1), '0'),
           IFNULL(FORMAT(IFNULL(varAdminExpLastYear, 0), 1), '0'),
           IFNULL(FORMAT((IFNULL(varAdminExpThisYear, 0) / NULLIF(varAdminExpLastYear, 0) - 1) * 100, 1), '0');


    INSERT INTO sudasi_tmp
    SELECT "(-)",
           "Others",
           IFNULL(FORMAT(IFNULL(varReturnRefundDiscountThisYear, 0), 1), '0'),
           IFNULL(FORMAT(IFNULL(varReturnRefundDiscountLastYear, 0), 1), '0'),
           IFNULL(FORMAT((IFNULL(varReturnRefundDiscountThisYear, 0) / NULLIF(varReturnRefundDiscountLastYear, 0) - 1) *
                         100, 1), '0');


    INSERT INTO sudasi_tmp
    SELECT "=",
           "OPEX",
           FORMAT(IFNULL(varSellingExpThisYear, 0) + IFNULL(varGeneralExpThisYear, 0) + IFNULL(varAdminExpThisYear, 0) +
                  IFNULL(varReturnRefundDiscountThisYear, 0), 1),
           FORMAT(IFNULL(varSellingExpLastYear, 0) + IFNULL(varGeneralExpLastYear, 0) + IFNULL(varAdminExpLastYear, 0) +
                  IFNULL(varReturnRefundDiscountLastYear, 0), 1),
           FORMAT(((IFNULL(varSellingExpThisYear, 0) + IFNULL(varGeneralExpThisYear, 0) +
                    IFNULL(varAdminExpThisYear, 0) + IFNULL(varReturnRefundDiscountThisYear, 0)) / NULLIF(
                           IFNULL(varSellingExpLastYear, 0) + IFNULL(varGeneralExpLastYear, 0) +
                           IFNULL(varAdminExpLastYear, 0) + IFNULL(varReturnRefundDiscountLastYear, 0), 0) - 1) * 100,
                  1);

    INSERT INTO sudasi_tmp
    SELECT "%",
           "Margin",
           FORMAT(IFNULL((varSellingExpThisYear + varGeneralExpThisYear), 0) /
                  NULLIF(IFNULL(varRevenueThisYear, 0), 0) * 100, 1),
           FORMAT(IFNULL((varSellingExpLastYear + varGeneralExpLastYear), 0) /
                  NULLIF(IFNULL(varRevenueLastYear, 0), 0) * 100, 1),
           FORMAT((IFNULL((varSellingExpThisYear + varGeneralExpThisYear), 0) /
                   NULLIF(IFNULL(varRevenueThisYear, 0), 0) * 100) -
                  (IFNULL((varSellingExpLastYear + varGeneralExpLastYear), 0) /
                   NULLIF(IFNULL(varRevenueLastYear, 0), 0) * 100), 1);

    INSERT INTO sudasi_tmp
    SELECT "", "", "", "", "";

    INSERT INTO sudasi_tmp
    SELECT "(+)",
           "Oth Inc",
           FORMAT(IFNULL(varOtherIncomeThisYear, 0), 1),
           FORMAT(IFNULL(varOtherIncomeLastYear, 0), 1),
           FORMAT((IFNULL(varOtherIncomeThisYear, 0) / NULLIF(varOtherIncomeLastYear, 0) - 1) * 100, 1);

    INSERT INTO sudasi_tmp
    SELECT "", "", "", "", "";

    INSERT INTO sudasi_tmp
    SELECT "=",
           "EBIT",
           FORMAT(((varRevenueThisYear - varCOGSThisYear) - (varSellingExpThisYear + varGeneralExpThisYear) +
                   (IFNULL(varOtherIncomeThisYear, 0))), 1),
           FORMAT(((varRevenueLastYear - varCOGSLastYear) - (varSellingExpLastYear + varGeneralExpLastYear) +
                   (IFNULL(varOtherIncomeLastYear, 0))), 1),
           FORMAT((((varRevenueThisYear - varCOGSThisYear) - (varSellingExpThisYear + varGeneralExpThisYear) +
                    IFNULL(varOtherIncomeThisYear, 0))
                       /
                   NULLIF((varRevenueLastYear - varCOGSLastYear) - (varSellingExpLastYear + varGeneralExpLastYear) +
                          IFNULL(varOtherIncomeLastYear, 0), 0) - 1) * 100, 1);


    INSERT INTO sudasi_tmp
    SELECT "%",
           "Margin",
           FORMAT(((varRevenueThisYear - varCOGSThisYear) - (varSellingExpThisYear + varGeneralExpThisYear) +
                   (IFNULL(varOtherIncomeThisYear, 0))) / NULLIF(IFNULL(varRevenueThisYear, 0), 0) * 100, 1),
           FORMAT(((varRevenueLastYear - varCOGSLastYear) - (varSellingExpLastYear + varGeneralExpLastYear) +
                   (IFNULL(varOtherIncomeLastYear, 0))) / NULLIF(IFNULL(varRevenueLastYear, 0), 0) * 100, 1),
           FORMAT(((((varRevenueThisYear - varCOGSThisYear) - (varSellingExpThisYear + varGeneralExpThisYear) +
                     IFNULL(varOtherIncomeThisYear, 0)) / NULLIF(IFNULL(varRevenueThisYear, 0), 0))
               -
                   (((varRevenueLastYear - varCOGSLastYear) - (varSellingExpLastYear + varGeneralExpLastYear) +
                     IFNULL(varOtherIncomeLastYear, 0)) / NULLIF(IFNULL(varRevenueLastYear, 0), 0)))
                      * 100, 1);

    INSERT INTO sudasi_tmp
    SELECT "", "", "", "", "";

    INSERT INTO sudasi_tmp
    SELECT "(-)",
           "IncTax",
           FORMAT(IFNULL(varIncomeTaxThisYear, 0), 1),
           FORMAT(IFNULL(varIncomeTaxLastYear, 0), 1),
           FORMAT(IFNULL((IFNULL(varIncomeTaxThisYear, 0) / NULLIF(IFNULL(varIncomeTaxLastYear, 0), 0) - 1) * 100, 0),
                  1);


    INSERT INTO sudasi_tmp
    SELECT "(-)",
           "OthTax",
           FORMAT(IFNULL(varOtherTaxThisYear, 0), 1),
           FORMAT(IFNULL(varOtherTaxLastYear, 0), 1),
           FORMAT(IFNULL((IFNULL(varOtherTaxThisYear, 0) / NULLIF(IFNULL(varOtherTaxLastYear, 0), 0) - 1) * 100, 0), 1);

    INSERT INTO sudasi_tmp
    SELECT "", "", "", "", "";

    SET varNetIncomeThisYear =
            IFNULL(varRevenueThisYear, 0) - IFNULL(varCOGSThisYear, 0) - IFNULL(varSellingExpThisYear, 0) -
            IFNULL(varGeneralExpThisYear, 0) - IFNULL(varAdminExpThisYear, 0) -
            IFNULL(varReturnRefundDiscountThisYear, 0) + IFNULL(varOtherIncomeThisYear, 0) -
            IFNULL(varIncomeTaxThisYear, 0) - IFNULL(varOtherTaxThisYear, 0);
    SET varNetIncomeLastYear =
            IFNULL(varRevenueLastYear, 0) - IFNULL(varCOGSLastYear, 0) - IFNULL(varSellingExpLastYear, 0) -
            IFNULL(varGeneralExpLastYear, 0) - IFNULL(varAdminExpLastYear, 0) -
            IFNULL(varReturnRefundDiscountLastYear, 0) + IFNULL(varOtherIncomeLastYear, 0) -
            IFNULL(varIncomeTaxLastYear, 0) - IFNULL(varOtherTaxLastYear, 0);

    INSERT INTO sudasi_tmp
    SELECT "(=)",
           "NetInc",
           FORMAT(varNetIncomeThisYear, 1),
           FORMAT(varNetIncomeLastYear, 1),
           FORMAT(((varNetIncomeThisYear - varNetIncomeLastYear) / NULLIF(IFNULL(varNetIncomeLastYear, 0), 0)) * 100,
                  1);

    INSERT INTO sudasi_tmp
    SELECT "%",
           "Margin",
           CASE
               WHEN COALESCE(varRevenueThisYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT((varNetIncomeThisYear / varRevenueThisYear) * 100, 1)
               END,
           CASE
               WHEN COALESCE(varRevenueLastYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT((varNetIncomeLastYear / varRevenueLastYear) * 100, 1)
               END,
           CASE
               WHEN COALESCE(varRevenueThisYear, 0) = 0 OR COALESCE(varRevenueLastYear, 0) = 0 THEN 'N/A'
               ELSE FORMAT(((varNetIncomeThisYear / varRevenueThisYear) * 100) -
                           ((varNetIncomeLastYear / varRevenueLastYear) * 100), 1)
               END;

END $$
DELIMITER ;

CALL t4_pnl_sp(2019);

SELECT *
FROM sudasi_tmp;