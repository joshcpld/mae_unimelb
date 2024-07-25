rm(list = ls())

# Question 1 ------------------------------------------------------------------

y <- read.csv("ushstarts.csv")

date <- seq(as.Date("1946/1/1"), as.Date("1994/11/1"), by = "month")

T <- length(date)

s = 12

# Defining seasonal dummy variables 

nyears = ceiling(T/s) 

spring <- rep(c(0,0,1,1,1,0,0,0,0,0,0,0), nyears)
summer <- rep(c(0,0,0,0,0,1,1,1,0,0,0,0), nyears)
fall <- rep(c(0,0,0,0,0,0,0,0,1,1,1,0), nyears)
winter <- rep(c(1,1,0,0,0,0,0,0,0,0,0,1), nyears)

spring <- spring[1:T]
summer <- summer[1:T]
fall <- fall[1:T]
winter <- winter[1:T]

# Estimating the model 

seasmod <- lm(formula = y$hstarts ~ 0 + spring + summer + fall + winter)

summary(seasmod)

# Plotting the estimated seasonal factors 

seasonal.f <- coef(seasmod)

seasons <- c("Spring", "Summer", "Fall", "Winter")

barplot(seasonal.f,
        main = "Seasonal Factors for Meteorological Seasons",
        xlab = "Season",
        ylab = "Seasonal Average",
        names.arg=seasons,
        col = "mediumpurple1")

# Generating the values of the predictor variables at the forecasting horizon

h = 24
nyears.f = h/s + 1

spring.h <- rep(c(0,0,1,1,1,0,0,0,0,0,0,0), nyears)
summer.h <- rep(c(0,0,0,0,0,1,1,1,0,0,0,0), nyears)
fall.h <- rep(c(0,0,0,0,0,0,0,0,1,1,1,0), nyears)
winter.h <- rep(c(1,1,0,0,0,0,0,0,0,0,0,1), nyears)

spring.h <- spring.h[12:36]
summer.h <- summer.h[12:36]
fall.h <- fall.h[12:36]
winter.h <- winter.h[12:36]

xh = data.frame(spring.h,summer.h,fall.h,winter.h)
colnames(xh) = c('spring','summer','fall','winter')

# Generating point and interval forecasts 

point.forecast <- predict(seasmod, newdata = xh)
point.forecast <- c(rep(NA,T),point.forecast)

ci.forecast <- data.frame(predict(seasmod, newdata = xh, interval = 'confidence'))
pi.forecast <- data.frame(predict(seasmod, newdata = xh, interval = 'prediction'))

ci.forecast.lwr <- c(rep(NA,T),ci.forecast$lwr)
ci.forecast.upr <- c(rep(NA,T),ci.forecast$upr)

pi.forecast.lwr <- c(rep(NA,T),pi.forecast$lwr)
pi.forecast.upr <- c(rep(NA,T),pi.forecast$upr)

# Plotting point and interval forecasts

datenew <- seq(as.Date("1946/1/1"), as.Date("1996/12/1"), by = "month")
hstartsnew <- c(y$hstarts, rep(NA,25))

plot(datenew[500:612],hstartsnew[500:612], 
     main = " 25 Step Ahead Forecast of Monthly US Housing Starts", 
     xlab = "Date",
     ylab = "(0,000's of New Constructions)",
     ylim = c(20,250),
     type = "l",
     lwd = 1,
     col = "black") 
lines(datenew,point.forecast, type = 'l', lty = 'dashed', lwd = 2.0, col = "red")
lines(datenew,ci.forecast.lwr, type = 'l', lty = 'dotted', lwd =2.0, col = "darkorange")
lines(datenew,ci.forecast.upr, type = 'l', lty = 'dotted', lwd =2.0, col = "darkorange")
lines(datenew,pi.forecast.lwr, type = 'l', lty = 'dotted', lwd =2.0, col = "darkorchid")
lines(datenew,pi.forecast.upr, type = 'l', lty = 'dotted', lwd =2.0, col = "darkorchid")
abline (v = datenew[T])

legend(x = "topleft",
       legend = c("Point Forecast", "95% Confidence Interval", "95% Prediction Interval"),
       lty = c("dashed", "dotted", "dotted"),
       lwd = 2.0,
       col = c("red", "darkorange", "darkorchid"))

# Question 2 -------------------------------------------------------------------

rm(list = ls())

# Importing and plotting the data

install.packages("readxl")
library(readxl)

y = read_excel("5206002_expenditure_volume_measures.xls", sheet = "Data1", col_names = "cons", range = "FM11:FM248")

cons <- ts(y$cons, start = c(1959,3), end = c(2018,4), frequency = 4)

plot.ts(cons, 
        main = "Quarterly Household Final Consumption Expenditure from Q1 1959 to Q4 2018",
        ylab = "Millions of $AUD",
        xlab = "Date",
        col = "seagreen",
        lwd = 2.0)

# Computing decomposition and generating the seasonally adjusted series

cons.decomp <- decompose(cons, type = "multiplicative")

cons.sa <- cons/cons.decomp$seasonal

# Comparing series with the seasonally adjusted series from the excel spreadsheet

z = read_excel("5206002_expenditure_volume_measures.xls", sheet = "Data1", col_names = "cons.sa2", range = "CI11:CI248")

cons.sa2 <- ts(z$cons.sa2, start = c(1959,3), end = c(2018,4), frequency = 4)

par(mfrow=c(2,1))
plot.ts(cons.sa, 
        main = "Seasonally Adjusted Series from decompose()",
        ylab = "Millions of $AUD",
        xlab = "Date",
        col = "seagreen",
        lwd = 2.0)
plot.ts(cons.sa2, 
        main = "Seasonally Adjusted Series from ABS Spreadsheet",
        ylab = "Millions of $AUD",
        xlab = "Date",
        col = "deepskyblue",
        lwd = 2.0)
dev.off()
# Estimating quadratic trend model

T = length(cons.sa)
time = seq(1,T)
timesq = time^2

quadtrend <- lm(formula = cons.sa ~ time + timesq)

summary(quadtrend)

# Plotting fitted quadratic trend

plot(date,cons.sa, 
     main = " Fitted Quadratic Trend of Quarterly Household Consumption", 
     xlab = "Date",
     ylab = "Millions of US Dollars", 
     type = "l",
     lwd = 2.0,
     col = "deepskyblue") 
lines(date, predict(quadtrend), type = 'l', lty = "dashed", lwd = 2.0)

# Generating point and interval forecasts 

h <- 8
xh <- data.frame(time = seq(from = T+1, to = T+h), timesq = seq(from = T+1, to = T+h)^2)

cons.sa.new <- c(cons.sa,rep(NA,h))

date <- seq(as.Date("1959/9/1"), as.Date("2018/12/1"), by = "quarter")
date.for <- seq(date[T], by = "quarter", length.out = h+1)
date.for <- date.for[1:h+1]

datenew <- c(date,date.for) 

point.forecast <- predict(quadtrend, newdata = xh)
point.forecast <- c(rep(NA,T),point.forecast)

ci.forecast <- data.frame(predict(quadtrend, newdata = xh, interval = 'confidence', level = 0.80))
pi.forecast <- data.frame(predict(quadtrend, newdata = xh, interval = 'prediction', level = 0.80))

ci.forecast.lwr <- c(rep(NA,T),ci.forecast$lwr)
ci.forecast.upr <- c(rep(NA,T),ci.forecast$upr)

pi.forecast.lwr <- c(rep(NA,T),pi.forecast$lwr)
pi.forecast.upr <- c(rep(NA,T),pi.forecast$upr)

# Plotting point and interval forecasts

plot(datenew[200:T+h],cons.sa.new[200:T+h], 
     main = " Quadratic Trend 8 Step Ahead Forecast of Quarterly Household Consumption", 
     xlab = "Date",
     ylab = "Millions of US Dollars", 
     ylim = c(200000,300000),
     type = "l",
     lwd = 2.0,
     col = "deepskyblue") 
lines(datenew,point.forecast, type = 'l', lty = 'dashed', lwd = 2.0, col = "red")
lines(datenew,ci.forecast.lwr, type = 'l', lty = 'dotted', lwd =2.0, col = "darkorange")
lines(datenew,ci.forecast.upr, type = 'l', lty = 'dotted', lwd =2.0, col = "darkorange")
lines(datenew,pi.forecast.lwr, type = 'l', lty = 'dotted', lwd =2.0, col = "darkorchid")
lines(datenew,pi.forecast.upr, type = 'l', lty = 'dotted', lwd =2.0, col = "darkorchid")
lines(date, predict(quadtrend), type = 'l', lty = "dashed", lwd = 2.0)
abline (v = datenew[T])

legend(x = "topleft",
       legend = c("Point Forecast", "80% Confidence Interval", "80% Prediction Interval"),
       lty = c("dashed", "dotted", "dotted"),
       lwd = 2.0,
       col = c("red", "darkorange", "darkorchid"))

