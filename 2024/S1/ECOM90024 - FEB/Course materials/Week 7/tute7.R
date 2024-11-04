
rm(list =ls())

# Question 1 

x = rpois(500,2)

lambda = rep(NA,500)

for (i in 1:500){
  y = rpois(500,2)
  lambda[i] = mean(y)
}

hist(lambda, breaks = 20)
mean(lambda)
var(lambda)

# Question 2

y = read.csv("tute7.csv")
y = y[,1]

plot.ts(y)

# To estimate an ARMA(1,1) using the mle function, we first need to define the conditional likelihood. 

T = length(y)

cll.arma <-  function(phi,theta,sigma){
  
  res = numeric(T+1)
  
  res[1] = y[1] # here we are assuming that y_0 = epsilon_0 = 0 
  
  for (i in 2:T){
    res[i] = y[i] - phi*y[i-1] - theta*res[i-1]         
  }
  return(-sum(dnorm(res, 0, sigma, log = TRUE)))
}

# Then we use the mle function with a reasonable set of starting values

arma.mle <- mle(cll.arma, start = list(phi = 0.5, theta = 0.5, sigma = 0.6), method = "L-BFGS-B")
arma.mle

# Comparing our results with those obtained using the Arima() function, we can see
# that we get very similar results! 

armamod = Arima(y, order = c(1,0,1), include.mean = FALSE)
armamod

phi = coef(armamod)[1]
theta = coef(armamod)[2]
res = armamod$residuals

# To compute the point forecasts, we can use a loop 

h = 10

j = seq(1:h)

yhat = numeric(h)

for (i in 1:h){
    yhat[i] = phi^(i)*y[T] + phi^(i-1)*theta*res[T]
}

# Having computed our point forecasts, we can proceed to compute the forecast error variances

sigmah = numeric(h)

sigmah[1] = armamod$sigma2

for (i in 2:h){
    sigmah[i] = sigmah[i-1] + (phi^(i-1) + phi^(i-2)*theta)^2*armamod$sigma2 
}

alpha = 0.20

lwrh.arma <- yhat - qnorm(alpha/2, lower.tail = FALSE)*sqrt(sigmah)
uprh.arma <- yhat + qnorm(alpha/2, lower.tail = FALSE)*sqrt(sigmah)

arma.for <- data.frame(j,yhat, lwrh.arma, uprh.arma)
colnames(arma.for) = c('hstep','what', 'lower', 'upper')

arma.for

# We can compare our results with the point and interval forecasts produced by the forecast() function
# To check that we are correct

forecast(armamod, h = 10)

# We can see that our point forecast will converge to the long run mean of the process (i.e., zero) while
# the width of the prediction/forecast interval increases with the forecasting horizon.

