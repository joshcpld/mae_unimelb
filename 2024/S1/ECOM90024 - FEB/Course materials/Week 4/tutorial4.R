rm(list=ls())
# Generating a white noise series 

e <- rnorm(200, mean = 0, sd = 1)
T <- length(e)
time <- seq(1,T)

plot(time, e, main = "Gaussian White Noise Series", type = "l")

acfwn <- acf(e, plot = FALSE)

plot(acfwn[1:20], main = "Sample ACF for White Noise Series")

pacfwn <- pacf(e, plot = FALSE) 

plot(pacfwn[1:20], main = "Sample PACF for White Noise Series")

# Generating a first order moving average series 

elag <- e[1:T-1]
ma <- e[2:T] + 0.9*elag

plot(time[2:T], ma, main = "First Order Moving Average Series", type = "l")

acfma <- acf(ma, plot = FALSE)

plot(acfma[1:20], main = "Sample ACF for MA(1) Series")

pacfma <- pacf(ma, plot = FALSE)

plot(pacfma[1:20], main = "Sample PACF for MA(1) Series")

# Generating a first order autoregressive series 

ar1 <- 1 # Here we are initialising the first observation in our autoregressive series

for (i in 2:T) # This is a loop that generates the autoregressive series 
{ar1[i] <- 0.9*ar1[i-1] + e[i]}

plot(time, ar1, main = "First Order Autoregressive Series", type = "l")

acfar <- acf(ar1, plot = FALSE)

plot(acfar[1:20], main = "Sample ACF for AR(1) Series")

pacfar <- pacf(ar1, plot = FALSE)

plot(pacfar[1:20], main = "Sample PACF for AR(1) Series")

# Generating a first order autoregressive series with alternate coefficient 

ar1alt <- 1   # Here we are initialising the first observation in our autoregressive series

for (i in 2:T) # This is a loop that generates the autoregressive series
{ar1alt[i] <- -0.9*ar1alt[i-1] + e[i]}

plot(time, ar1alt, main = "First Order Autoregressive Series", type = "l")

acfaralt <- acf(ar1alt, plot = FALSE)

plot(acfaralt[1:20], main = "Sample ACF for AR(1) Series")

pacfaralt <- pacf(ar1alt, plot = FALSE)

plot(pacfaralt[1:20], main = "Sample PACF for AR(1) Series")

