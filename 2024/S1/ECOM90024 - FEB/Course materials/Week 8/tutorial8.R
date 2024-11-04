rm(list = ls())

library(DREGAR)
library(urca)
library(forecast)

# Question 2--------------------------------------------------------------------
ar.order <- 4

ar.coef <- generateAR(n = ar.order)

ar.coef

T = 1000

y <- arima.sim(n = T, list(ar = ar.coef))

plot(ar.sim1,
     main = "Plot of Stationary Simulated AR(4) Process",
     type = "l",
     col = "darkorange")

df.1 <- ur.df(y, type = "none", lags = 3)

summary(df.1)

z <- cumsum(y)

plot(z,
     main = "Plot of Non-Stationary Simulated AR(4) Process",
     type = "l",
     col = "darkorchid")

df.2 <- ur.df(z, type = "none", lags = 3)

summary(df.2)

# Question 3 -------------------------------------------------------------------

spdata <- read.csv("sp500.csv")
sp <- spdata[,2]
plot(sp, 
     main = "Plot of the S&P500",
     ylab = "Index Value",
     xlab = "Time",
     type = "l",
     col = "green3",
     lwd = 2.0)

acf(sp)
pacf(sp)

T = length(sp)

kmax = ceiling(12*(T/100)^(1/4))

df.sp = ur.df(sp, type = "drift", lags = kmax-2)

summary(df.sp)

spret = 100*diff(log(sp))

plot(spret, 
     main = "Plot of the S&P500 Continuously Compounded Returns",
     ylab = "Percentage",
     xlab = "Time",
     type = "l",
     col = "blue3",
     lwd = 2.0)

acf(spret)
pacf(spret)

df.spret = ur.df(spret, type = "none", lags = kmax-3)
summary(df.spret)
