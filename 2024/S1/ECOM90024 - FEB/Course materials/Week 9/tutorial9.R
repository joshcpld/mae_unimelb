rm(list= ls())
library(tseries)

data <- read.csv("btc.csv")

p <- data$btc

date <- as.Date(data$date, format = "%d/%m/%y")

T <- length(p)

lp <- log(p)

ret <- lp[2:T] - lp[1:(T-1)] 
ret.2 = diff(lp)

plot(date[2:T], ret, main = "BTC Daily Returns", col = "darkorchid", type = "l", lwd = 2)

acfret <- acf(ret, plot = FALSE)
pacfret <- pacf(ret, plot = FALSE)


plot(acfret[1:20], main = "Sample Autocorrelations for daily Bitcoin returns")



plot(pacfret[1:20], main = "Sample Partial Autocorrelations for daily Bitcoin returns")


m <- ceiling(sqrt(T))

Box.test(ret, lag = m, type = "Box-Pierce")
Box.test(ret, lag = m, type = "Ljung-Box")

retsq <- ret^2

acfretsq <- acf(retsq, plot = FALSE)
pacfretsq <- pacf(retsq, plot = FALSE)


plot(acfretsq[1:20], main = "Sample Autocorrelations for daily Bitcoin squared returns")



plot(pacfretsq[1:20], main = "Sample Partial Autocorrelations for daily Bitcoin squared returns")


Box.test(retsq, lag = m, type = "Box-Pierce")
Box.test(retsq, lag = m, type = "Ljung-Box")
T_r <- length(retsq)

arch.lm <- lm(formula = retsq[6:T_r] ~ retsq[5:(T_r-1)] + retsq[4:(T_r-2)] + retsq[3:(T_r-3)] + retsq[2:(T_r-4)] + retsq[1:(T_r-5)])

z = 25

archaic <- numeric(length = z)
archbic <- numeric(length = z)

for(i in 1:z){
archmod <- garch(ret, order = c(0,i))
archaic[i] <- AIC(archmod)
archbic[i] <- AIC(archmod, k = log(T_r - i))
}

arch06 <- garch(ret, order = c(0,6))

archr <- resid(arch06)

archr <- archr[7:(T-1)]

plot(archr, main = "Residuals from the ARCH(6) Estimation", type = "l", col = "red")


archrsq <- archr^2

acfarchrsq <- acf(archrsq, plot = FALSE)
pacfarchrsq <- pacf(archrsq, plot = FALSE)


plot(acfarchrsq[1:20], main = "Sample Autocorrelations for squared standardised ARCH residuals")



plot(pacfarchrsq[1:20], main = "Sample Partial Autocorrelations for squared standardised ARCH residuals")


Box.test(archrsq, lag = m, type = "Box-Pierce")
Box.test(archrsq, lag = m, type = "Ljung-Box")



