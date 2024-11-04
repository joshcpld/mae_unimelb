library(tidyverse)
library(readxl)
library(forecast)
library(tsbox)
library(car)

options(scipen=999) #Get's rid of scientific notation

################################################################################
################################Exercise 1######################################
################################################################################

# A) calculate returns and excess returns for the CAPM model

e1 <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 3/t3e1.xlsx") 

e1_analysis <- e1 %>% 
  select(Date, Exxon, SP500, Tbill) %>% 
  mutate(Exxon_r = Exxon / lag(Exxon) - 1)  %>% 
  mutate(SP500_r = SP500 / lag(SP500) - 1) %>%
  mutate(Tbill_r = Tbill / 12) %>% 
  mutate(Exxon_ER = Exxon_r - Tbill_r) %>% 
  mutate(SP500_ER = SP500_r - Tbill_r)

Exxon_ER <- e1_analysis %>% 
  select(Exxon_ER) %>% 
  ts(start = c(1990, 4), end = c(2004, 7), frequency = 12)

SP500_ER <- e1_analysis %>% 
  select(SP500_ER) %>% 
  ts(start = c(1990, 4), end = c(2004, 7), frequency = 12)
  

ts.plot(SP500_ER)

#B) Estimate the CAPM simple linear regression for Exxon

CAPM_Exxon = lm(Exxon_ER ~ SP500_ER)
summary(CAPM_Exxon)


#What are the key points from this output?

#R^2 is 0.23, suggesting that this regression accounts for only 23% of the variations 
#of the excess returns on the Exxon stocks


#There is A LOT  more detail, refer back here when doing assignment.





#-------------------------------------------------------------------------------
#B) Test whether the beta-risk if Exxon is significantly different from 1, implying
#That is does not track the market one-to-one

#You need to construct a one-sided F-test. To do this manually you need to construct
#a t-value and then compared this to teh t-critical value.

#However, there is an easy way to do it in R

library(car)
linearHypothesis(model = CAPM_Exxon, c("SP500_ER = 1"))
#What this test does it create a a general F-test that you can specify yourself

#In this instance we are generating an F-test where the null-hypothesis is that 
#Beta_1 from our CAPM_Exxon model is equal to 1.

#In the output, the p-value is practically zero, therefore we can reject the null hypothesis
#And conclude that Exxon does not track the stock market one-to-one

#Re-do the previous exercise using log returns

e1_analysis <- e1_analysis %>% 
  mutate(Exxon_ER_log = (log(Exxon) - lag(log(Exxon))) - Tbill_r) %>% 
  mutate(SP500_ER_log = (log(SP500) - lag(log(SP500))) - Tbill_r)


Exxon_ER_log <- e1_analysis %>% 
  select(Exxon_ER_log) %>% 
  ts(start = c(1990, 4), end = c(2004, 7), frequency = 12)

SP500_ER_log <- e1_analysis %>% 
  select(SP500_ER_log) %>% 
  ts(start = c(1990, 4), end = c(2004, 7), frequency = 12)

 
CAPM_Exxon_log <- lm(Exxon_ER_log ~ SP500_ER_log)
summary(CAPM_Exxon_log)



#-------------------------------------------------------------------------------
#C)


#Consider the original CAPM and conduct residual analysis by:

#i) plotting the residuals


CAPM_Exxon_residuals <- ts(residuals(CAPM_Exxon),
                           start = c(1990, 4), end = c(2004, 7), frequency = 12)

plot.ts(CAPM_Exxon_residuals, xlab = "Date", ylab = "Residuals", main = "Residuals - Exxon", col = "violetred4") %>% 
abline(h=0)

#The residuals fluctuate around zero and show no obvious pattern, except the variance seems large in second half of series


#ii) conduct the White test for heteroskedasticity

#Note: in this test H_0 = homoskedasticity & H_A = heteroskedasticity

library(lmtest)
bptest(CAPM_Exxon, ~ SP500_ER + I(SP500_ER^2))

#In this test the p-value is 0.5, therefore we cannot reject the null hypothesis.

#Therefore, this result does not support the observation made above, that variance appears larger in the second half


#iii) Perform the BG test for (H_0) no autocorrelation up to order 12 in the error term against
#(H_A) some 1-12 autocorrelation

bgtest(CAPM_Exxon, order = 12, type = "Chisq")

#Again, we cannot reject the null hypothesis. This implies the stochastic error terms might be serially uncorrelated

#iv) test for the normality of the error term. (H_0) normal distribution is tested against (H_A) non-normal distribution.

library(tseries)
jarque.bera.test(CAPM_Exxon_residuals)

#P-value is 0.073, therefore we maintain H_0 at the 5% level and stochastic error terms might be normally distributed


#v) LM test for ARCH error, (H_0) no first-order autocorrelation in the squared stochastic error terms is tested
#against (H_A) first-order autocorrelation in the squared stochastic error terms

library(FinTS)
ArchTest(CAPM_Exxon_residuals, lags = 1)

ArchTest(CAPM_Exxon_residuals, lags = 12)

#The p-value is about 0.70, too large again to reject H0 at the usual significance levels, so there is no evidence of ARCH errors.
#this is the same for ARCH errors 1-12.


#v) Now do the reset test for (H_0) correct functional form and (H-A) incorrect funtional form


resettest(CAPM_Exxon, power = 3, type = "fitted")


# The p-value is about 0.77, too large again to reject H0 at the usual significance levels, so the functional form of the model might adequate.






################################################################################
################################Exercise 2######################################
################################################################################


e2 <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 3/t3e2.xlsx") 



e2_analysis <- e2 %>% 
  mutate(ENERGY_ER = ENERGY - RF) %>% 
  mutate(MKT_ER = MKT - RF)

  
  
  
ENERGY_ER <- e2_analysis %>% 
  select(ENERGY_ER) %>% 
  ts(start = c(1927, 1), end = c(2013, 12), frequency = 12)


MKT_ER <- e2_analysis %>% 
  select(MKT_ER) %>% 
  ts(start = c(1927, 1), end = c(2013, 12), frequency = 12)

SMB <- e2_analysis %>% 
  select(SMB) %>% 
  ts(start = c(1927, 1), end = c(2013, 12), frequency = 12)

HML <- e2_analysis %>% 
  select(HML) %>% 
  ts(start = c(1927, 1), end = c(2013, 12), frequency = 12)


FF3 <- lm(ENERGY_ER ~ MKT_ER + SMB + HML)
summary(FF3)


#------------------------------------------------------------------------------
#B)
#If Beta_2 and Beta_3 = 0 then FF3 model collapses to the basic. Test these restrictions

linearHypothesis(model = FF3, c("SMB = 0", "HML = 0"))

#We can reject the null hypothesis, and conclude this three-factor model better explains
#The movements than the basic CAP



#C) This three-factor model have similar sized Betas for B2 & B3 in absolute terms, but opposite signs.
#Test the restriction B2 + B3 = 0


linearHypothesis(model = FF3, c("SMB + HML = 0"))

#remember H_0: B2 + B3 = 0, H_a the inverse

#We cannot reject the null hypothesis, therefore we can replace the 3-factor model with a restrictire version.

#Estimate this new model

FF3_r <- lm(ENERGY_ER ~ MKT_ER + I(SMB - HML))
summary(FF3_r)

# Comparing both printouts, ther are practically the same in terms of goodness of fit and slope estimate. Therefore it is not binding.





################################################################################
################################Exercise 2######################################
################################################################################

e3 <- read_excel("C:/Users/joshc/Documents/2023S2/ECON90033/Tutorials/Week 3/t2e2.xlsx")


# A) Estimate the present value model

PRICE <- e3 %>% 
  select(PRICE) %>% 
  ts(start = c(1900, 1), end = c(2016, 9), frequency = 12)

DIVIDEND <- e3 %>% 
  select(DIVIDEND) %>% 
  ts(start = c(1900, 1), end = c(2016, 9), frequency = 12)


pvm <- lm(log(PzRICE) ~ log(DIVIDEND))
summary(pvm)


#B) Examine the properties of the estimated model by performing the following tests:

# i. Plot the ordinary least squares residuals and interpret their time series patterns.

pvm_residuals <- ts(residuals(pvm),
                    start = c(1900, 1), end = c(2016, 9), frequency = 12)

plot.ts(pvm_residuals,
        xlab = "Date", ylab = "Residuals",
        main = "Residuals - Present value model", col = "red")
abline(h = 0)

#There is clearly positive first order autocorrelation in the residuals
#Test for autocorrelation to make sure this is actually the case


# ii. Test for autocorrelation of orders 1 to 6.


bgtest(pvm, order = 6, type = "Chisq")

#P-value is basically zero, therefore we reject the null hypothesis. There is evidence of autocorrelation.


# iii. Test for second order ARCH errors.


ArchTest(pvm_residuals, lags = 2)

#Reject null hypothesis, there is evidence of first/second order ARCH errors



# iv. Test for normality of the residuals.


jarque.bera.test(pvm_residuals)

#P-value is too high to reject null, therefore the stochastic error terms might be normally distributed.




#C)

#In part a we estimated the PV model

#In tutorial 2, this specification is taken from the alleged lin r/ship b/wn the log of Price and dividends:

#   lnP_t = -lngamma + lnD

#Where gamma is the discount factor at time 1 and the slop parameter is beta = 1

#Test this restriction and test the result and retest if merited using a generalized F-test


linearHypothesis(model = FF3, c("SMB + HML = 0"))

#The p-value is zero, so we reject the null: the slope parameter is different from one.

#therefore stick to the unrestricted form.

