#install.packages("corrplot")
library(quantmod)
library(tseries)
library(timeSeries)
library(fPortfolio)
library(caTools)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")



eq_1 <- getSymbols("IXN", auto.assign = FALSE)[,6]
eq_2 <- getSymbols("VIG", auto.assign = FALSE)[,6]
fixed_income <- getSymbols("AGG", auto.assign = FALSE)[,6]
real_estate <- getSymbols("SRS", auto.assign = FALSE)[,6]
commod <- getSymbols("PDBC", auto.assign = FALSE)[,6]

joined_dailyprices <- merge.xts(eq_1, eq_2, fixed_income, real_estate, commod)

colnames(joined_dailyprices) <- c("eq1", "eq2", "fixed_income","real_estate",
                                  "commod")

eq1_dreturns <- dailyReturn(eq_1)
eq2_dreturns <- dailyReturn(eq_2)
fixed_income_dreturns <- dailyReturn(fixed_income)
real_estate_dreturns <- dailyReturn(real_estate)
commod_dreturns <- dailyReturn(commod)

joined_dailyreturns <- merge.xts(eq1_dreturns,
                                 eq2_dreturns, 
                                 fixed_income_dreturns,
                                 real_estate_dreturns,
                                 commod_dreturns)

colnames(joined_dailyreturns) <- c("eq1", "eq2", "fixed_income","real_estate",
                                   "commod")

joined_dailyreturns <- na.omit(joined_dailyreturns, "extend")

eq1_returns <- monthlyReturn(eq_1)
eq2_returns <- monthlyReturn(eq_2)
fixed_income_returns <- monthlyReturn(fixed_income)
real_estate_returns <- monthlyReturn(real_estate)
commod_returns <- monthlyReturn(commod)

joined_monthlyreturns <- merge.xts(eq1_returns,
                                   eq2_returns, 
                                   fixed_income_returns,
                                   real_estate_returns,
                                   commod_returns)

benchmark_returns <- monthlyReturn(getSymbols("^GSPC", auto.assign = FALSE))
joined_monthlyreturns <- merge.xts(joined_monthlyreturns, benchmark_returns)

colnames(joined_monthlyreturns) <- c("eq1", "eq2", "fixed_income","real_estate",
                                     "commod","SP500")

IXN_alloc <- 0.18
VIG_alloc <- 0.22
AGG_alloc <- 0.30
SRS_alloc <- 0.10
PDBC_alloc <- 0.20

#creating our portfolio returns viable: 
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns) %>%
  mutate(portfolio = IXN_alloc*eq1 + 
           VIG_alloc*eq2 +
           AGG_alloc*fixed_income +
           SRS_alloc*real_estate +
           PDBC_alloc*commod)

time_index <- nrow(joined_monthlyreturns)

timeindex_12M <- (time_index-11) : time_index
timeindex_18M <- (time_index-17) : time_index
timeindex_24M <- (time_index-23) : time_index


Returns_12M <- Return.annualized(joined_monthlyreturns[timeindex_12M,])
Returns_18M <- Return.annualized(joined_monthlyreturns[timeindex_18M,]) 
Returns_24M <- Return.annualized(joined_monthlyreturns[timeindex_24M,])

#Sigma

eq1_sigma <- sd(joined_monthlyreturns$eq1[timeindex_12M]) * sqrt(12)
eq2_sigma <- sd(joined_monthlyreturns$eq2[timeindex_12M]) * sqrt(12)
fixed_income_sigma <- sd(joined_monthlyreturns$fixed_income[timeindex_12M]) * sqrt(12)
real_estate_sigma <- sd(joined_monthlyreturns$real_estate[timeindex_12M]) * sqrt(12)
commod_sigma <- sd(joined_monthlyreturns$commod[timeindex_12M]) * sqrt(12)
portfolio_sigma <- sd(joined_monthlyreturns$portfolio[timeindex_12M]) * sqrt(12)
SP500_sigma <- sd(joined_monthlyreturns$SP500[timeindex_12M]) * sqrt(12)

tickers <- c("eq1", "eq2", "fixed_income", "real_estate", "commod", "portfolio",
             "SP500")
sigmas <- c(eq1_sigma, eq2_sigma, fixed_income_sigma, real_estate_sigma, commod_sigma, portfolio_sigma,
            SP500_sigma)

sigma_summary <- cbind(tickers, sigmas)


# Tracking Error
eq1_te <- sd(joined_monthlyreturns$eq1[timeindex_12M] - joined_monthlyreturns$SP500[timeindex_12M]) * sqrt(12)
eq2_te <- sd(joined_monthlyreturns$eq2[timeindex_12M] - joined_monthlyreturns$SP500[timeindex_12M]) * sqrt(12)
fixed_income_te <- sd(joined_monthlyreturns$fixed_income[timeindex_12M] - joined_monthlyreturns$SP500[timeindex_12M]) * sqrt(12)
real_estate_te <- sd(joined_monthlyreturns$real_estate[timeindex_12M] - joined_monthlyreturns$SP500[timeindex_12M]) * sqrt(12)
commod_te <- sd(joined_monthlyreturns$commod[timeindex_12M] - joined_monthlyreturns$SP500[timeindex_12M]) * sqrt(12)
portfolio_te <- sd(joined_monthlyreturns$portfolio[timeindex_12M] - joined_monthlyreturns$SP500[timeindex_12M]) * sqrt(12)
SP500_te <- sd(joined_monthlyreturns$SP500[timeindex_12M] - joined_monthlyreturns$SP500[timeindex_12M]) * sqrt(12)

tracking_errors <- c(eq1_te, eq2_te, fixed_income_te, real_estate_te, commod_te, portfolio_te,
                     SP500_te)

te_summary <- cbind(tickers, tracking_errors)

# Sharpe Ratio

riskfree <- 0.003

eq1_sharpe <- (mean(joined_monthlyreturns$eq1[timeindex_12M]) - riskfree ) / eq1_sigma
eq2_sharpe <- (mean(joined_monthlyreturns$eq2[timeindex_12M]) - riskfree ) / eq2_sigma
fixed_income_sharpe <- (mean(joined_monthlyreturns$fixed_income[timeindex_12M]) - riskfree ) / fixed_income_sigma
real_estate_sharpe <- (mean(joined_monthlyreturns$real_estate[timeindex_12M]) - riskfree ) / real_estate_sigma
commod_sharpe <- (mean(joined_monthlyreturns$commod[timeindex_12M]) - riskfree ) / commod_sigma
portfolio_sharpe <- (mean(joined_monthlyreturns$portfolio[timeindex_12M]) - riskfree ) / portfolio_sigma
SP500_sharpe <- (mean(joined_monthlyreturns$SP500[timeindex_12M]) - riskfree ) / SP500_sigma

sharpe_ratios <- c(eq1_sharpe, eq2_sharpe, fixed_income_sharpe, real_estate_sharpe, commod_sharpe, portfolio_sharpe,
                   SP500_sharpe)

SR_summary <- cbind(tickers, sharpe_ratios)

# Covariance matrix and Correlation  

cov(joined_monthlyreturns, use='complete.obs')
portfolio_cor <- cor(joined_monthlyreturns, use='complete.obs')
portfolio_cor_12M <- cor(joined_monthlyreturns[timeindex_12M,], use='complete.obs')

rquery.cormat(joined_monthlyreturns[,1:5])
rquery.cormat(joined_monthlyreturns[timeindex_12M,1:5])

# CAPM for Beta
#let's assume that the risk free rate is 0 (zero)

last_12_months <- joined_monthlyreturns[(time_index-11) : time_index, ]

eq1_reg <- lm(eq1 ~ SP500 ,data=last_12_months)  
summary(eq1_reg)

eq2_reg <- lm(eq2 ~ SP500 ,data=last_12_months)  
summary(eq2_reg)

fixed_income_reg <- lm(fixed_income ~ SP500 ,data=last_12_months)  
summary(fixed_income_reg)

real_estate_reg <- lm(real_estate ~ SP500 ,data=last_12_months)  
summary(real_estate_reg)

commod_reg <- lm(commod ~ SP500 ,data=last_12_months)  
summary(commod_reg)

portfolio_reg <- lm(portfolio ~ SP500 ,data=last_12_months)  
summary(portfolio_reg)

#create a random sample out of the entire data:
testing_sample_indx <- sample(1:nrow(joined_monthlyreturns), size=5) #I'll be taking 5 dates
#subset the data from just those observations from the random sample
testing_sample_data <- joined_monthlyreturns[testing_sample_indx,]
#predict() function to estimate the mu using the CAPM model
predict(eq1_reg, testing_sample_data)

# Treynor's ratio: this will give us the beta
eq1_treynor <- (mean(joined_monthlyreturns$eq1[timeindex_12M])-riskfree ) / eq1_reg$coefficients[2] 
eq2_treynor <- (mean(joined_monthlyreturns$eq2[timeindex_12M])-riskfree ) / eq2_reg$coefficients[2]
fixed_income_treynor <- (mean(joined_monthlyreturns$fixed_income[timeindex_12M])-riskfree ) / fixed_income_reg$coefficients[2]
real_estate_treynor <- (mean(joined_monthlyreturns$real_estate[timeindex_12M])-riskfree ) / real_estate_reg$coefficients[2]
commod_treynor <- (mean(joined_monthlyreturns$commod[timeindex_12M])-riskfree ) / commod_reg$coefficients[2]
portfolio_treynor <- (mean(joined_monthlyreturns$portfolio[timeindex_12M])-riskfree ) / portfolio_reg$coefficients[2]

TRs <- c("eq1", "eq2", "fixed_income", "real_estate", "commod", "portfolio")
T_ratios <- c(eq1_treynor, eq2_treynor, fixed_income_treynor, real_estate_treynor, commod_treynor, portfolio_treynor)

TR_summary <- cbind(TRs, T_ratios)

####################################################
#### ACF and pACF - daily timeseries analysis #######
####################################################

acf(joined_dailyreturns$eq1) #eq1 - we might see 2 lags here MA(2)
acf(joined_dailyreturns$eq2) #eq2 - only 2 lags MA(2)
acf(joined_dailyreturns$fixed_income) #fixed_income - see 2 lags MA(2)
acf(joined_dailyreturns$real_estate) #real_estate - see 2 lags MA(2)
acf(joined_dailyreturns$commod) #commod - see 0 lags MA(0)



#pACF - will show us the lag - "if today depends on yesterday" - autocorrelation - AR
pacf(joined_dailyreturns$eq1) #eq1 - we might see 1 lags here AR(1)
pacf(joined_dailyreturns$eq2) #eq2 - only 2 lags AR(2)
pacf(joined_dailyreturns$fixed_income) #fixed_income - see 0 lags AR(0)
pacf(joined_dailyreturns$real_estate) #real_estate - see 2 lags AR(2)
pacf(joined_dailyreturns$commod) #commod - see 0 lags AR(0)


#######################################################################
#### Fitting ARMA on daily ROR stationary time series      ############
#### NOTE : if you wanted to forecast non-stationary, use ARIMA #######
#######################################################################
#fitting an ARMA(1,2) on IXN:
eq1_arma <- arma(joined_dailyreturns$eq1, order=c(1,2)) 
summary(eq1_arma)
#however, to use the predict() function we need to use the arima function
eq1_arima <- arima(joined_dailyreturns$eq1, 
                   order=c(1,0,2)) 
predict(eq1_arima, n.ahead =3) #want to get forecasted values for 3 days out

#fitting an ARMA(2,2) on VIG:
eq2_arma <- arma(joined_dailyreturns$eq2, order=c(2,2)) 
summary(eq2_arma)
#however, to use the predict() function we need to use the arima function
eq2_arima <- arima(joined_dailyreturns$eq2, 
                   order=c(2,0,2)) 
predict(eq2_arima, n.ahead =3) #want to get forecasted values for 3 days out

#fitting an ARMA(0,2) on AGG:
fixed_income_arma <- arma(joined_dailyreturns$fixed_income, order=c(0,2)) 
summary(fixed_income_arma)
#however, to use the predict() function we need to use the arima function
fixed_income_arima <- arima(joined_dailyreturns$fixed_income, 
                            order=c(0,0,2)) 
predict(fixed_income_arima, n.ahead =3) #want to get forecasted values for 3 days out

#fitting an ARMA(2,2) on SRS:
real_estate_arma <- arma(joined_dailyreturns$real_estate, order=c(2,2)) 
summary(real_estate_arma)

real_estate_arima <- arima(joined_dailyreturns$real_estate, 
                           order=c(2,0,2)) 
predict(real_estate_arima, n.ahead =3) #want to get forecasted values for 3 days out

#fitting an ARMA(0,0) on PDBC:
commod_arma <- arma(joined_dailyreturns$commod, order=c(0,0)) 
summary(commod_arma)

commod_arima <- arima(joined_dailyreturns$commod, 
                      order=c(0,0,0)) 
predict(commod_arima, n.ahead =3)

