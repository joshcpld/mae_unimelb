## 
 # This file contains commands and results, so for reading not running.
 # If you want source code that you can just run, it is in code_Ex10Q2.R
 ##
R version 3.5.0 (2018-04-23) -- "Joy in Playing"
Copyright (C) 2018 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin15.6.0 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]
## 
 # 
 # Load data and generate useful working names
 # You will need to provide paths that work for you for the two commands immediately below
 # 
 ##
> setwd("~/Dropbox/Teaching/Econometrics 3/2025/Exercises/Week 10/Week_10_Solutions")
> wages <- read.csv("~/Dropbox/Teaching/Econometrics 3/2025/Exercises/Week 10/Week_10_Solutions/wages.csv")
>   View(wages)
> logsal=wages$LOGSAL
> educ=wages$EDUC
> gender=wages$GENDER
> minority=wages$MINORITY
> jobcat=wages$JOBCAT
## 
 # 
 # Fit the unrestricted model.  For no obvious reason but will give us an 
 # idea of what to expect (Coefficient on educ looks pretty significant, 
 # so LM tests should reject H_0.)
 # 
 ##
> mdl_unr=lm(logsal~educ+gender+minority+jobcat)
> mdl_unstats=summary.lm(mdl_unr)
> mdl_unr

Call:
lm(formula = logsal ~ educ + gender + minority + jobcat)

Coefficients:
(Intercept)         educ       gender     minority       jobcat  
    9.26350      0.04869      0.16595     -0.08135      0.25786  

> mdl_unstats

Call:
lm(formula = logsal ~ educ + gender + minority + jobcat)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.49591 -0.13065 -0.00745  0.12182  0.87216 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  9.263500   0.045055 205.605  < 2e-16 ***
educ         0.048688   0.003741  13.015  < 2e-16 ***
gender       0.165947   0.020211   8.211 2.15e-15 ***
minority    -0.081347   0.022334  -3.642    3e-04 ***
jobcat       0.257863   0.014126  18.255  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1961 on 469 degrees of freedom
Multiple R-squared:  0.7585,	Adjusted R-squared:  0.7564 
F-statistic: 368.2 on 4 and 469 DF,  p-value: < 2.2e-16
## 
 # 
 # Fit restricted model so that we can create e0
 # 
 ##
> mdl_r=lm(logsal~gender+minority+jobcat)
> mdl_rstats=summary.lm(mdl_r)
> mdl_rstats

Call:
lm(formula = logsal ~ gender + minority + jobcat)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.47081 -0.14843 -0.02354  0.13570  0.94495 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  9.78689    0.02368 413.373  < 2e-16 ***
gender       0.22287    0.02300   9.691  < 2e-16 ***
minority    -0.11087    0.02589  -4.281 2.25e-05 ***
jobcat       0.33507    0.01494  22.426  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2286 on 470 degrees of freedom
Multiple R-squared:  0.6712,	Adjusted R-squared:  0.6691 
F-statistic: 319.9 on 3 and 470 DF,  p-value: < 2.2e-16

> e0=mdl_rstats$residuals
## 
 # 
 # Fit equation (2) from Exercise
 # 
 ##
> mdl_2=lm(e0~educ+gender+minority+jobcat)
> mdl2_stats=summary.lm(mdl_2)
> mdl2_stats

Call:
lm(formula = e0 ~ educ + gender + minority + jobcat)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.49591 -0.13065 -0.00745  0.12182  0.87216 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.523392   0.045055 -11.617  < 2e-16 ***
educ         0.048688   0.003741  13.015  < 2e-16 ***
gender      -0.056925   0.020211  -2.817  0.00506 ** 
minority     0.029519   0.022334   1.322  0.18691    
jobcat      -0.077208   0.014126  -5.466  7.5e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1961 on 469 degrees of freedom
Multiple R-squared:  0.2653,	Adjusted R-squared:  0.2591 
F-statistic: 42.35 on 4 and 469 DF,  p-value: < 2.2e-16
## 
 # 
 # Create n as sample size 
 # 
 ##
> n=length(e0)
> n
[1] 474
## 
 # 
 # Now construct LM1 statistic as described immediately above Equation (2) of 
 # the Exercise.
 # 
 ##
> LM1=length(e0)*mdl2_stats$r.squared
> LM1
[1] 125.7683
## 
 # 
 # Next construct residuals from regression of educ on the remaining 
 # explanatory variables.    This is the variable (M_X_2)x_1 in the Exercise.
 # 
 ##
> mdl_3=lm(educ~gender+minority+jobcat)
> MX2x1=mdl_3$residuals
## 
 # 
 # Fit Equation (3) from Exercise.  Note: No intercept!
 # 
 ##
> mdl_LM2=lm(e0~MX2x1-1)
> mdl_LM2stats=summary.lm(mdl_LM2)
## 
 # 
 # Construct LM2 as described immediately above Equation (3) in the Exercise.
 # 
 ##
> LM2=n*mdl_LM2stats$r.squared
> LM2
[1] 125.7683
## 
 # 
 # Fantastic! LM1=LM2.  Now for LM3.  Equation above equation (4) on page 5 suggests 
 # that it should equal sqare root of LM2, so let's calculate that first.  
 # Will call it LM3a.
 # 
 ##
> mdl_LM2stats

Call:
lm(formula = e0 ~ MX2x1 - 1)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.49591 -0.13065 -0.00745  0.12182  0.87216 

Coefficients:
      Estimate Std. Error t value Pr(>|t|)    
MX2x1 0.048688   0.003725   13.07   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1953 on 473 degrees of freedom
Multiple R-squared:  0.2653,	Adjusted R-squared:  0.2638 
F-statistic: 170.8 on 1 and 473 DF,  p-value: < 2.2e-16

> LM3a=sqrt(LM2)
> LM3a
[1] 11.21465
## 
 # 
 # Construct sums of squared residuals
 # 
 ##
> sse0=t(e0)%*%e0
> sse0
        [,1]
[1,] 24.5508
> sse1=t(MX2x1)%*%MX2x1
> sse1
         [,1]
[1,] 2748.043
> sigma.tilde.sq=t(e0)%*%e0/n
> sigma.tilde.sq
           [,1]
[1,] 0.05179494
## 
 # 
 #  Gather bits and pieces to construct LM3 manually
 #  
 ##
> phi=mdl_LM2$coefficients
> phi
     MX2x1 
0.04868751 
> LM3b=phi*sqrt(sse1/sigma.tilde.sq)
> LM3b
         [,1]
[1,] 11.21465
## 
 # 
 #  LM3a=LM3b, so both versions of LM3 are equal, as they should be.  Yeah!
 #  Now for LM4.  Run reverse regression (Equation (4) from Exercise).  Again no intercept
 #  
 ##
> mdl_LM4=lm(MX2x1~e0-1)
> mdl_LM4stats=summary.lm(mdl_LM4)
> mdl_LM4stats

Call:
lm(formula = MX2x1 ~ e0 - 1)

Residuals:
    Min      1Q  Median      3Q     Max 
-5.8288 -1.0738  0.1767  1.4768  4.6969 

Coefficients:
   Estimate Std. Error t value Pr(>|t|)    
e0    5.450      0.417   13.07   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.066 on 473 degrees of freedom
Multiple R-squared:  0.2653,	Adjusted R-squared:  0.2638 
F-statistic: 170.8 on 1 and 473 DF,  p-value: < 2.2e-16
## 
 # 
 #  Gather bits and pieces to construct LM4.  
 #  Notes: 
 #  1. Standard error for \hat{psi}=sqrt((hat{\sigma}^2)*(sse0)^(-1))
 #  2. hat{\sigma}^2=sse1/n
 #  3. t=\hat(psi)/sqrt((hat{\sigma}^2)*(sse0)^(-1))
 #      =\hat(psi)*sqrt(n*sse0/sse1)
 #  4. Contrast expression from 3 with formula for LM3:
 #     t=\hat{phi)*sqrt(n*sse1/sse0)
 #  
 ##
> psi=mdl_LM4$coefficients
> psi
      e0 
5.449735 
> LM4=psi*sqrt(n*sse0/sse1)
> LM4
         [,1]
[1,] 11.21465
## 
 # 
 #  LM4=LM3.  Yeah!!
 #  
 ##

## 
 # Next, Part (b)
 # 
 # First find critical value for 5% upper tail test
 ##

> cv2=qchisq(0.95,df=2)
> cv2
[1] 5.991465

 ## 
  # Next, run restricted regression and save residuals
  ##
> mdl_Q2=lm(logsal~educ+gender)
> mdl_Q2.stat=summary.lm(mdl_Q2)
> mdl_Q2.stat

Call:
lm(formula = logsal ~ educ + gender)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.62421 -0.18000 -0.01467  0.15515  0.89894 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 9.132275   0.057991 157.476   <2e-16 ***
educ        0.080853   0.004462  18.122   <2e-16 ***
gender      0.245606   0.025817   9.513   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2616 on 471 degrees of freedom
Multiple R-squared:  0.5684,	Adjusted R-squared:  0.5666 
F-statistic: 310.1 on 2 and 471 DF,  p-value: < 2.2e-16

> Q2_resid=mdl_Q2.stat$residuals
> ## 
> # Next run auxiliary regrssion, save R^2, and construct LM test
> ##
> aux_Q2=lm(Q2_resid~educ+gender+minority+jobcat) 
> aux_Q2.stat=summary.lm(aux_Q2)
> aux_Q2.stat

Call:
lm(formula = Q2_resid ~ educ + gender + minority + jobcat)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.49591 -0.13065 -0.00745  0.12182  0.87216 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.131224   0.045055   2.913  0.00376 ** 
educ        -0.032165   0.003741  -8.598  < 2e-16 ***
gender      -0.079659   0.020211  -3.941 9.34e-05 ***
minority    -0.081347   0.022334  -3.642  0.00030 ***
jobcat       0.257863   0.014126  18.255  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1961 on 469 degrees of freedom
Multiple R-squared:  0.4404,	Adjusted R-squared:  0.4356 
F-statistic: 92.27 on 4 and 469 DF,  p-value: < 2.2e-16

> aux_Q2.rsquare=aux_Q2.stat$r.squared
> Q2_LM1=n*aux_Q2.rsquare
> Q2_LM1
[1] 208.745
## 
 # Finally, Part (c)
 # 
 # First, find the critical value that we will need.
 ##
> cv3=qchisq(0.95,df=1)
> cv3
[1] 3.841459
## 
 # Create new variable Z
 ##
> z=jobcat-minority
## 
 # Next, generate residuals from restricted model: mdl_Q3_resid
 # (regression of logsal on intercept, educ, gender, z)
 ##
> mdl_Q3=lm(logsal~educ+gender+z)
> mdl_Q3_resid=mdl_Q3$residuals
## 
 # Generate residuals from regression of minority on intercept, educ, gender, z
 # Call them mdl_Q3_restricted$residuals
 ##
> mdl_Q3_restricted=lm(minority~educ+gender+z)
> mdl_Q3_restricted_resid=mdl_Q3_restricted$residuals
## 
 # Regress mdl_Q3_resid on mdl_Q3_restricted$residuals without intercept
 # Save the coefficient of determination: final.stat$r.squared
 # Then calculate LM2
 ##
> final=lm(mdl_Q3_resid~mdl_Q3_restricted_resid-1)
> final.stat=summary.lm(final)
> final.stat

Call:
lm(formula = mdl_Q3_resid ~ mdl_Q3_restricted_resid - 1)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.49591 -0.13065 -0.00745  0.12182  0.87216 

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
mdl_Q3_restricted_resid  0.17652    0.02777   6.356 4.87e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1953 on 473 degrees of freedom
Multiple R-squared:  0.07869,	Adjusted R-squared:  0.07675 
F-statistic:  40.4 on 1 and 473 DF,  p-value: 4.872e-10
## 
 # calculate LM2
 ##
> LM2_Q3=n*final.stat$r.squared
> LM2_Q3
[1] 37.30097





