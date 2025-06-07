# Question 1
# As a preliminary step, let's initialse the workspace
rm(list=ls())
# setwd("Insert your own path here and then uncomment")

# We need the following package, and its dependencies, to read in a Stata file
install.packages("haven")
library(haven)
# Read in Data
loanapp <- read_dta("my_loanapp.dta")
#The attach command allows us to just use variable names without the data= 
#option on the glm command.
attach(loanapp)
# (a) Fit a probit model
probit=glm(approve~hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+pubrec+mortlat1+mortlat2+vr+white, family=binomial(link="probit"))
probit.summary=summary(probit)
probit.logLik=logLik(probit)
probit.summary
probit.logLik

# (b) construct interaction term and see what impact it has
wobrat=white*obrat
probit2=glm(approve~hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+pubrec+mortlat1+mortlat2+vr+white+wobrat, family=binomial(link="probit"))
probit2.summary=summary(probit2)
probit2.logLik=logLik(probit2)
probit2.summary
probit2.logLik
probit2.rd=probit2$deviance

probit3=glm(approve~hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+pubrec+mortlat1+mortlat2+vr, family=binomial(link="probit"))
probit3.summary=summary(probit3)
probit3.logLik=logLik(probit3)
probit3.rd=probit3$deviance
probit3.summary
probit3.logLik

#Can use LR tests which are chi-sq(2) under the null that coeficients on white and wobrat are jointly zero
#The relevant critical value for a 5% test can be otained as
qchisq(0.95, 2)

#Construct LR test based on maximized log-likelihoods
LRtest=-2*(probit3.logLik-probit2.logLik)
LRtest
#Construct LR test based on residual Deviances
LR2test=probit3$deviance-probit2$deviance
LR2test

# Question 2# As a preliminary step, let's initialse the workspace
rm(list=ls())
# setwd("Insert your own path here and then uncomment")

# Load Data and then subset it
DebTrivedi = read.csv("DebTrivedi.csv")
dt=DebTrivedi[, c(1, 6:9, 16, 18, 21)]

# Plot frequency of physician visits
plot(table(dt$ofp), main="Frequency distribution for number of physician office visits.", 
   xlab="Number of physician office visits", ylab="Frequency")

# Fit Poisson Regression Model
PReg <- glm(ofp ~ ., data = dt, family = poisson)
summary(PReg)

