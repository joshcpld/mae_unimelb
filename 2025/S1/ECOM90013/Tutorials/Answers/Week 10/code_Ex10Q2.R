## 
 # This file contains the raw source code for the Week 10 Exercise.  
 # It can be loaded directly into R and just run.
 # For something containing results as well, see Ex10Q2_Rcode.R
 ##

## 
 # 
 # Load data and generate useful working names
 # You will need to provide paths that work for you for the two commands immediately below
 # 
 ##
setwd("~/Dropbox/Teaching/Econometrics 3/2025/Exercises/Week 10/Week_10_Solutions")
wages <- read.csv("~/Dropbox/Teaching/Econometrics 3/2025/Exercises/Week 10/Week_10_Solutions/wages.csv")
logsal=wages$LOGSAL
educ=wages$EDUC
gender=wages$GENDER
minority=wages$MINORITY
jobcat=wages$JOBCAT
## 
 # 
 # Fit the unrestricted model.  For no obvious reason but will give us an 
 # idea of what to expect (Coefficient on educ looks pretty significant, 
 # so LM tests should reject H_0.)
 # 
 ##
mdl_unr=lm(logsal~educ+gender+minority+jobcat)
mdl_unstats=summary.lm(mdl_unr)
## 
 # 
 # Fit restricted model so that we can create e0
 # 
 ##
mdl_r=lm(logsal~gender+minority+jobcat)
mdl_rstats=summary.lm(mdl_r)
e0=mdl_rstats$residuals
## 
 # 
 # Fit equation (2) from Exercise
 # 
 ##
mdl_2=lm(e0~educ+gender+minority+jobcat)
mdl2_stats=summary.lm(mdl_2)
## 
 # 
 # Create n as sample size 
 # 
 ##
n=length(e0)
## 
 # 
 # Now construct LM1 statistic as described immediately above Equation (2) of 
 # the Exercise.
 # 
 ##
LM1=length(e0)*mdl2_stats$r.squared
## 
 # 
 # Next construct residuals from regression of educ on the remaining 
 # explanatory variables.    This is the variable (M_X_2)x_1 in the Exercise.
 # 
 ##
mdl_3=lm(educ~gender+minority+jobcat)
MX2x1=mdl_3$residuals
## 
 # 
 # Fit Equation (3) from Exercise.  Note: No intercept!
 # 
 ##
mdl_LM2=lm(e0~MX2x1-1)
mdl_LM2stats=summary.lm(mdl_LM2)
## 
 # 
 # Construct LM2 as described immediately above Equation (3) in the Exercise.
 # 
 ##
LM2=n*mdl_LM2stats$r.squared
## 
 # 
 # Fantastic! LM1=LM2.  Now for LM3.  Equation above equation (4) on page 5 suggests 
 # that it should equal sqare root of LM2, so let's calculate that first.  
 # Will call it LM3a.
 # 
 ##
LM3a=sqrt(LM2)
## 
 # 
 # Construct sums of squared residuals
 # 
 ##
sse0=t(e0)%*%e0
sse1=t(MX2x1)%*%MX2x1
sigma.tilde.sq=t(e0)%*%e0/n
## 
 # 
 #  Gather bits and pieces to construct LM3 manually
 #  
 ##
phi=mdl_LM2$coefficients
LM3b=phi*sqrt(sse1/sigma.tilde.sq)
## 
 # 
 #  LM3a=LM3b, so both versions of LM3 are equal, as they should be.  Yeah!
 #  Now for LM4.  Run reverse regression (Equation (4) from Exercise).  Again no intercept
 #  
 ##
mdl_LM4=lm(MX2x1~e0-1)
mdl_LM4stats=summary.lm(mdl_LM4)
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
psi=mdl_LM4$coefficients
LM4=psi*sqrt(n*sse0/sse1)
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
cv2=qchisq(0.95,df=2)
## 
 # Next, run restricted regression and save residuals
 ##
mdl_Q2=lm(logsal~educ+gender)
mdl_Q2.stat=summary.lm(mdl_Q2)
Q2_resid=mdl_Q2.stat$residuals
## 
 # Next run auxiliary regrssion, save R^2, and construct LM test
 ##
aux_Q2=lm(Q2_resid~educ+gender+minority+jobcat) 
aux_Q2.stat=summary.lm(aux_Q2)
aux_Q2.rsquare=aux_Q2.stat$r.squared
Q2_LM1=n*aux_Q2.rsquare
## 
 # Finally, Part (c)
 # 
 # First, find the critical value that we will need.
 ##
cv3=qchisq(0.95,df=1)
## 
 # Create new variable Z
 ##
z=jobcat-minority
## 
 # Next, generate residuals from restricted model: mdl_Q3_resid
 # (regression of logsal on intercept, educ, gender, z)
 ##
mdl_Q3=lm(logsal~educ+gender+z)
mdl_Q3_resid=mdl_Q3$residuals
## 
 # Generate residuals from regression of minority on intercept, educ, gender, z
 # Call them mdl_Q3_restricted$residuals
 ##
mdl_Q3_restricted=lm(minority~educ+gender+z)
mdl_Q3_restricted_resid=mdl_Q3_restricted$residuals
## 
 # Regress mdl_Q3_resid on mdl_Q3_restricted$residuals without intercept
 # Save the coefficient of determination: final.stat$r.squared
 # Then calculate LM2
 ##
final=lm(mdl_Q3_resid~mdl_Q3_restricted_resid-1)
final.stat=summary.lm(final)
## 
 # calculate LM2
 ##
LM2_Q3=n*final.stat$r.squared


