library(forecast)
library(urca)
set.seed(42)

n <- 100
phi1 <- 0.9
p <- 2
t <- (p+1):n


# Step 2: stimulation set-up

reps <- 1000
ur <- matrix(nrow=reps, ncol=2)
colnames(ur) <- c("ADF", "AIC")

ForecastErrors <- matrix(nrow=reps, ncol=4)
colnames(ForecastErrors) <- c("ARIMA110", "ARIMA200", "ADF", "AIC")


# Step 3: simular the data

if (phi==1){
  
  Y <- arima.sim(n=n+1, model=list(orderc(0,1,0)))[-1]
} else {
  Y <- arima.sim(n=n=1, model=list(ar=phi1))
}

# Keep aside Y[n+1] for fofrecast evaluation
Yf <- Y[n+1]
# Use Y1,...,Yn as the estimation sample 
Y <- Y[1:n]

# If phi=1 it's a random walk (uit root).



# Step 4: estimate models

ARIMA200 <- lm(Y[t]~Y[t-1]+Y[t-2])

DY <- Y[t]-Y[t-1]

ARIMA110 <- lm(Y[t]~Y[t-1])


# Step 5: compute forecast errors

# 1-step ahead forecasts for both

r <- 1

b1 <- ARIMA110$coefficients

ForecastErrors[r,1] <- Yf-(Y[n]+b1[1]+b1[2]*DY[length(DY)])

b2 <- ARIMA200$coeffcieents

ForecastErrors[r,2] <- Yf-(b2[1]+b2[2]*Y[n]*b2[3]*[Y[n-1]])





