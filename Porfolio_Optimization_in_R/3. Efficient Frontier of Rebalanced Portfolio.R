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

#How do our residuals look like in this model? are these models good?
#we want to see residuals(standardized) that are linear 
plot(eq1_reg, which=2, col=c("red"))
plot(eq2_reg, which=2, col=c("blue"))
plot(fixed_income_reg, which=2, col=c("green4"))
plot(real_estate_reg, which=2, col=c("black"))
plot(commod_reg, which=2, col=c("orange"))
plot(portfolio_reg, which=2, col=c("pink"))

#we can first create a random sample out of the entire data:
testing_sample_indx <- sample(1:nrow(joined_monthlyreturns), size=5) #I'll be taking 5 dates
#we will now subset the data from just those observations from the random sample
testing_sample_data <- joined_monthlyreturns[testing_sample_indx,]
#now we'll use the predict() function to estimate the mu using the CAPM model
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

## Efficient Frontier using fportfolio

eq1_returns <- monthlyReturn(eq_1)
eq2_returns <- monthlyReturn(eq_2)
fixed_income_returns <- monthlyReturn(fixed_income)
real_estate_returns <- monthlyReturn(real_estate)
commod_returns <- monthlyReturn(commod)

monthlyret <- merge.xts(eq1_returns,
                        eq2_returns, 
                        fixed_income_returns,
                        real_estate_returns,
                        commod_returns)

monthlyret <- na.omit(monthlyret, "extend")

asset_tickers <- c("eq1", "eq2", "fixed income", "real estate", "commod")

colnames(monthlyret) <- asset_tickers 

monthlyret_ts <- as.timeSeries(monthlyret)


effFrontier <- portfolioFrontier(
  monthlyret_ts, `setRiskFreeRate<-`(portfolioSpec(), 0.001),
  constraints = "LongOnly"
)



#1: Efficient Frontier
#2: Global Minimum Vairnace Portfolio 
#3: Tangent (Optimal) Porfolio
#4: Risk/Return for each Asset
#5: Equal Weights Portfolio
#6: Two Assets Frontier
#7: Monte Carlo Portfolio
#8: Sharpe Ratio

plot(effFrontier, c(1,2,3,4))

frontierweights <- getWeights(effFrontier)
colnames(frontierweights) <- c("eq1", "eq2", "fixed_income","real_estate",
                               "commod")
risk_return <- frontierPoints(effFrontier)

annualisedpoints <- data.frame(targetRisk=risk_return[, "targetRisk"] * sqrt(12),
                               targetRetrun=risk_return[,"targetReturn"] *12)

plot(annualisedpoints)

#frotnier weights
barplot(t(frontierweights), main="Frontier Weights", col=cm.colors(ncol(frontierweights)+2), legend=colnames(frontierweights))


#minimum variance portfolio
mvp <- minvariancePortfolio(monthlyret_ts, spec = portfolioSpec(), constraints = "LongOnly")
mvp_weights <- getWeights(mvp)


tangencyport <- tangencyPortfolio(monthlyret_ts, spec = portfolioSpec(), constraints = "LongOnly")
tangencyport_weights <- getWeights(tangencyport)

barplot(tangencyport_weights, main="Tangency Portfolio", xlab="Assets", ylab = "Weights (%)", col=cm.colors(ncol(frontierweights)))

df <- data.frame(tangencyport_weights)
assets <- colnames(frontierweights)
ggplot(data=df, aes(x=assets, y=tangencyport_weights, fill=assets))+
  geom_bar(stat="identity", position=position_dodge(), color="black")+
  geom_text(aes(label=sprintf("%.02f %%", tangencyport_weights*100)),
            position = position_dodge(width=0.9), vjust=-0.25, check_overlap = T) +
  ggtitle("Tangency Portfolio Optimal Weights") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Assets", y="Weights (%)")

# Highest Sharpe portfolio
effFrontier_sameSR <- portfolioFrontier(
  monthlyret_ts[,c(4,5)], `setRiskFreeRate<-`(portfolioSpec(), 0.001),
  constraints = "LongOnly"
)

plot(effFrontier_sameSR, c(1,2,3,4,8))
