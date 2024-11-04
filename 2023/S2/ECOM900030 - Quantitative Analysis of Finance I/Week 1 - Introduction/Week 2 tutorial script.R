library(tidyverse)
library(readxl)
library(forecast)
library(tsbox)



# options(scipen=999) #Get's rid of scientific notation

################################################################################
################################Exercise 1######################################
################################################################################


#A) Graph the indices

dfts.prices <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 2/t2e1.xlsx",
                  sheet = "capm") %>% 
  select(-Date) %>% 
  ts(start = c(1990,4), end = c(2004,7), frequency = 12)

plot.ts(dfts.prices)


#B) Calculate growth and approximate (log growth)


glimpse(exxon)


Exxon <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 2/t2e1.xlsx",
                    sheet = "capm") %>% 
  select(Exxon) %>%
  mutate(R_Exxon = (Exxon - lag(Exxon)) / lag(Exxon)) %>%
  mutate(r_Exxon = log(Exxon) - lag(log(Exxon))) %>% 
  select(-Exxon) %>% 
  ts(start = c(1990,4), end = c(2004,7), frequency = 12) %>% 
  plot.ts()


#C) Assume we have an equal weighted portfolio, compute portfolio return for the first 7 months of 2004

#Note: the price of a portfolio is the sum of the number of stock in a portfolio, multiplied by each stock's weight and price

portfolio <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 2/t2e1.xlsx",
                  sheet = "capm") %>% 
  mutate(R_Exxon = (Exxon - lag(Exxon)) / lag(Exxon),
         R_GE = (GE - lag(GE)) / lag(GE),
         R_IBM = (IBM - lag(IBM)) / lag(IBM),
         R_Msoft = (Msoft - lag(Msoft)) / lag(Msoft),
         R_Wmart = (Wmart - lag(Wmart)) / lag(Wmart),
         R_Gold = (Gold - lag(Gold)) / lag(Gold)) %>% 
  mutate(R_p = (R_Exxon + R_GE + R_IBM + R_Msoft + R_Wmart + R_Gold) / 6) %>% 
  mutate(r_p = log((exp(R_Exxon) + exp(R_GE) + exp(R_IBM) + exp(R_Msoft) + exp(R_Wmart) + exp(R_Gold)) / 6)) %>% 
  select(Date, R_p,r_p) %>% 
  filter(Date >= "2004-01-01", Date < "2004-08-01") %>% 
  print()
  

#Multi-period returns

#The log return of the portfolio for the first 7 months of 2004 is just the sum of the seven
#Simple log returns  (R_p)

portfolio_return_log <- portfolio %>% 
  summarise(r_pk = sum(r_p)) %>% 
  print()

#To get the simple return of the portfolio, we need to use the product function

portfolio_return <- portfolio %>% 
  summarise(R_pk = prod( 1+ R_p) -1 ) %>% 
  print()



################################################################################
################################Exercise 2######################################
################################################################################


#A) plot equity price over tiem and interpret its time series properties

price <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 2/t2e2.xlsx") %>% 
  select(PRICE) %>% 
  ts(start = c(1900,1), end = c(2016,9), frequency = 12)


#B) Plot the natural logarithm of the equity price over time and interpret its properties


plot.ts(price)

lnprice <- price %>% 
  log()

plot.ts(lnprice)


#These two graphs show the price trend is roughly exponential, whereas the log series fluctuates around a linear trend



#C) Calculate the simple and log return on equities over time and interpret properties

PRICE <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 2/t2e2.xlsx") %>% 
  select(PRICE) %>%  
  mutate(simple_return = (PRICE/lag(PRICE,1) -1)) %>% 
  mutate(log_return = log(PRICE) - lag(log(PRICE)))
  

simple_return <- returns %>% 
  select(simple_return) %>% 
  ts(start = c(1900,1), end = c(2016,9), frequency = 12)

plot.ts(simple_return, col = "blue")

log_return <- returns %>% 
  select(log_return) %>% 
  ts(start = c(1900,1), end = c(2016,9), frequency = 12)

plot.ts(log_return, col = "red")



#D) plot PRICE and DIVIDEND series and compare their properties

PRICE <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 2/t2e2.xlsx") %>% 
  select(PRICE) %>% 
  ts(start = c(1900,1), end = c(2016,9), frequency = 12)

plot.ts(PRICE, ylab = "Price", main = "Equity Price", col = "blue")


DIVIDEND <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 2/t2e2.xlsx") %>% 
  select(DIVIDEND) %>% 
  ts(start = c(1900,1), end = c(2016,9), frequency = 12)

plot.ts(DIVIDEND, ylab = "Dividend", main = "Dividend Payment", col = "red")

#Both are exponential, but price is much more volatile



#E) compute the dividencd yield and comment
  
DIV_YIELD <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 2/t2e2.xlsx") %>% 
  select(DIVIDEND,PRICE) %>% 
  mutate(DIV_YIELD = DIVIDEND / PRICE) %>% 
  select(DIV_YIELD) %>% 
  ts(start = c(1900,1), end = c(2016,9), frequency = 12)

ts.plot(DIV_YIELD)


#F) verify the prediction that there's a linear relationship between the log of equity prices and the log of dividends.
# as per the present value model.

DIV_YIELD <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 2/t2e2.xlsx") %>% 
  select(DIVIDEND,PRICE) %>% 
  mutate(DIVIDEND_log = log(DIVIDEND), 
         PRICE_log = log(PRICE))

plot(DIV_YIELD$DIVIDEND_log,DIV_YIELD$PRICE_log, main = "Scatterplot of log price and log dividend",
     col = "red", pch = 19)



#G) Calculate the sample mean, standard deviation, skewness and kurtosis of the returns on US equities

library(pastecs)

stat.desc(cbind(simple_return, log_return), basic = FALSE, desc = TRUE, norm = TRUE)


## Follow up with Laszlo





################################################################################
################################Exercise 3######################################
################################################################################


E3 <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 2/t2e3.xlsx",
                 sheet = "Data")

#A) graph a histogram of the duration times between sales

hist(E3$d, breaks = 100, freq = FALSE, main = "Empirical distribution of duration")
lines(density(E3$d), col = 4, lwd = 2)

#this series seems to have an exponential distribution. Note the that expected value and
#standard deviation of an exponential random variable X are equal

#Therefore, to check for exponential distribution we can check for equality between the mean and SD

mean(E3$d)
sd(E3$d)

#The difference between them is large, so probably not exponentially distributed


#To be sure, we can use a specific statistical test: Lillefors test for exponentiality.

#H_o = d is distributued exponentially against the alternative hypothesis
#H_a = d is not distributed exponentially


library(EnvStats)

test_exp_lillie <- gofTest(E3$d, test = "lillie", distribution = "exp")
test_exp_lillie$statistic
test_exp_lillie$p.value


#the p-value is ~0, therefore we reject the null hypothesis

