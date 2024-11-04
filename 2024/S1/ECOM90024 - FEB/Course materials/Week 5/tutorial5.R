rm(list = ls())

ar2 <- read.csv("ar2.csv")

acfar2 <- acf(ar2[,2], plot = FALSE)


plot(acfar2[1:20], main = "Sample ACF for AR(2) Series")


pacfar2 <- pacf(ar2[,2], plot = FALSE)


plot(pacfar2[1:20], main = "Sample PACF for AR(2) Series")


rho <- acfar2$acf

phi2 <- (rho[2]^2 - rho[3])/(rho[2]^2-1)
phi1 <- rho[2]*(1-phi2)

T <- length(ar2[,2])

ar2 <- ar2$x

ar2l0 <- ar2[3:T]
ar2l1 <- ar2[2:(T-1)]
ar2l2 <- ar2[1:(T-2)]

linreg1 <- lm(formula = ar2l0 ~ 0 + ar2l1 + ar2l2)
summary(linreg1)
