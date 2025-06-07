## 
 # Set the working directory. You will need to adjust this to meet your own needs.
 ##
rm(list=ls())
setwd("~/Dropbox/Teaching/Econometrics 3/2025/Exercises/Week 10/Week_8_Solutions")
## 
 # Import data. Note that we first need to install the readxl package, 
 # which can generate a warning if your R installation is older than 
 # 3.5.2, like mine was, but otherwise seems to work okay.
 ##
install.packages("readxl")
library(readxl)
Stocks <- read_excel("Stocks.xls")
## 
 # Initialize some variables that get used below.
 ##
x=Stocks$RENDMARK
y=Stocks$RENDNCCO
n=length(y)
one=rep(1,n)
opg=matrix(rep(0,4),nrow=2)
## 
 # Part (b)
 # ----------
 ##
## 
 # Install the nleqslv package
 # If you already have it installed and loaded then you can comment out 
 # the next two lines
 ##
install.packages("nleqslv")
library(nleqslv)
## 
 # Create a function to construct the score.  This is needed by nleqslv.
 # - theta are values for the parameters
 # - x and y are your regression variables (in an obvious notation)
 ##
cauchy.score<-function(theta,x,y){
   alpha<-theta[1]
   beta<-theta[2]   
   n<-length(y)
   e<-rep(0,n)
   for(i in 1:n)
      {
         temp=y[i]-alpha-beta*x[i]
         e[i]=temp/(1+temp*temp)
      }
   score.alpha<-sum(e) 
   score.beta<-t(x)%*%e
   score=c(score.alpha,score.beta)
   return(score)
}
## 
 # Let it rip!  Parameter estimates will be stored in solution$x.
 ##
solution=nleqslv(c(1,1),cauchy.score,jac=NULL,x,y)
solution$x
## 
 # We need the residuals again to estimate the information matrix.
 ##
e=y-one*solution$x[1]-x*solution$x[2]
## 
 # Now for the information matrix.
 # Note that the information matrix is of dimension 2 x 2 and so I 
 # simply work out the values of those 4 elements at each sample point.  
 # Note that the order in whicj R populates the matrix is to fill the 
 # first column, from top to bottom, and then head to the top of the 
 # second column.
 ##
for(i in 1:n){
   temp=2*opg
   e[i]/(1+e[i]*e[i])
   temp2=temp*temp
   xtemp2=x[i]*temp2
   opg=opg+matrix(c(temp2,xtemp2,xtemp2,xtemp2*x[i]),nrow=2)
}
## 
 # Now obtain the standard errors of the mle
 ##
sd=sqrt(diag(solve(opg)))
sd
## 
 # Part (c)
 # ----------
 ##
## 
 # Obtain OLS/MLE estimates of coefficients.  
 # This will also yield OLS standard errors and t-statistics.
 ##
mdl_Q3=lm(y~x)
mdl_Q3.stat=summary.lm(mdl_Q3)
ols=mdl_Q3$coefficients
ols
se_ols=mdl_Q3.stat$coefficients[,2]
se_ols
## 
 # Obtain MLE standard errors by scaling OLS standard errors by a factor 
 # of sqrt((n-2)/n)
 ##
se_mle=se_ols*sqrt(1-2/n)
se_mle
## 
 # Part (d)
 # ----------
 ##
## 
 # Find standard Normal critical value for 0.025 upper tail probability
 ##
cv=qnorm(0.975)
cv
## 
 # Various t-stats, including 2 ways of calculating that for the Normal mle
 ##
t_cauchy=solution$x[1]/sd[1]
t_ols=mdl_Q3.stat$coefficients[1,3]
t_norm1=ols[1]/se_mle[1]
t_norm2=sqrt(n/(n-2))*t_ols
c(t_cauchy,t_ols,t_norm1)
## 
 # Part (e)
 # ----------
 # Note that the residuals for the Normal mle and OLS are identical
 ##
e_ols=mdl_Q3$residuals
## 
 # Now to make a pretty couple of histograms
 # First, find rgb values for some colours.
 # Second, create transparent versions of these colours (0\leq alpha \leq 255)
 # Third, create histograms but don't print them
 # Finally, use the plot command to print the histograms one atop the 
 # other].  This gives us the power to add legends, etc.
 # Conclude that this is beautiful enough for us. :-)
 ##
col2rgb(c("lightgreen", "pink"))
c1 <- rgb(144,238,144,max = 255, alpha = 80, names = "lt.green")
c2 <- rgb(255,192,203,max = 255, alpha = 80, names = "lt.pink")
hg_cauchy=hist(e,plot=FALSE)
hg_ols=hist(e_ols,plot=FALSE)
plot(hg_cauchy,col=c1,main="Residuals from Cauchy and Normal Models",xlab="Residuals")
legend("topright", c("Cauchy", "Normal"), fill=c(c1, c2))
plot(hg_ols,add=TRUE,col=c2)




