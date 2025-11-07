rm(list=ls())
setwd("~/Library/CloudStorage/Dropbox/_TSAF/Week3/Tutorial3")
library(forecast)

dt <- read.csv("RetailSales.csv")
Retail_m <- ts(dt$Total, frequency=12, start=c(1982,4), end=c(2024,9))
Retail_q <- aggregate(Retail_m, nfrequency=4)
Y <- window(log(Retail_q), start=c(2000,1), end=c(2019,4))
Time <- time(Y)
gfc <- 2008.5
Time_postgfc <- 1*(Time>gfc)*(Time-gfc)
QD <- seasonaldummy(Y)
X <- cbind(Time, Time_postgfc, QD)


## "Recursive" regression

Time_1 <- 2000.0
Time_n <- 2017.75
h <- 8

pmax <- 4

Forecasts <- matrix(nrow=h, ncol=pmax+1)
colnames(Forecasts) <- paste0("AR",0:pmax)
rownames(Forecasts) <- Time_n+0.25*(1:h)

ForecastErrors <- matrix(nrow=h, ncol=pmax+1)
colnames(ForecastErrors) <- paste0("AR",0:pmax)
rownames(ForecastErrors) <- Time_n+0.25*(1:h)

for (k in 1:h){
  t_est <- which(Time>= Time_1 & Time<=Time_n)
  t_for <- which(Time==(Time_n+0.25))
  for (p in 0:pmax){
    ARp <- Arima(Y[t_est], order=c(p,0,0), xreg=X[t_est,])
    ARp_for <- forecast(ARp, xreg=X[t_for,,drop=FALSE])
    Forecasts[k,p+1] <- ARp_for$mean
    ForecastErrors[k,p+1] <- Y[t_for]-ARp_for$mean
  }
  Time_n <- Time_n+0.25
}
cat("Recursive regressions\n")
cat("=====================\n")
cat("Forecasts:\n")
print(Forecasts)
cat("\nForecast Errors:\n")
print(ForecastErrors)
cat("\nRMSE:\n")
RMSE <- sqrt(apply(ForecastErrors^2, MARGIN=2, FUN=mean))
print(RMSE)
cat(paste0("\nBest model: ", names(RMSE)[which.min(RMSE)]))



## "Rolling" regression

Time_1 <- 2000.0
Time_n <- 2017.75
h <- 8

pmax <- 4

Forecasts <- matrix(nrow=h, ncol=pmax+1)
colnames(Forecasts) <- paste0("AR",0:pmax)
rownames(Forecasts) <- Time_n+0.25*(1:h)

ForecastErrors <- matrix(nrow=h, ncol=pmax+1)
colnames(ForecastErrors) <- paste0("AR",0:pmax)
rownames(ForecastErrors) <- Time_n+0.25*(1:h)

for (k in 1:h){
  t_est <- which(Time>= Time_1 & Time<=Time_n)
  t_for <- which(Time==(Time_n+0.25))
  for (p in 0:pmax){
    ARp <- Arima(Y[t_est], order=c(p,0,0), xreg=X[t_est,])
    ARp_for <- forecast(ARp, xreg=X[t_for,,drop=FALSE])
    Forecasts[k,p+1] <- ARp_for$mean
    ForecastErrors[k,p+1] <- Y[t_for]-ARp_for$mean
  }
  Time_1 <- Time_1+0.25
  Time_n <- Time_n+0.25
}

cat("\n\nRecursive regressions\n")
cat("=====================\n")
cat("Forecasts:\n")
print(Forecasts)
cat("\nForecast Errors:\n")
print(ForecastErrors)
cat("\nRMSE:\n")
RMSE <- sqrt(apply(ForecastErrors^2, MARGIN=2, FUN=mean))
print(RMSE)
cat(paste0("\nBest model: ", names(RMSE)[which.min(RMSE)]))


