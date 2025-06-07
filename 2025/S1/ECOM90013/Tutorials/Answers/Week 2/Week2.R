# --------------------------------------------------------------------
# ECOM40006/90013 Econometrics 3
# Semester 1, 2025
# Author: Daniel Tiong
# Week 2: Creating Q-Q plots in R
# --------------------------------------------------------------------

# Clear the workspace
rm(list=ls())

# Fix the random seed for replicablity (not like it matters much here)
set.seed(42)

# Generate random draws from our desired distributions
std.N = rnorm(250)
t.20  = rt(250, df = 20)
t.10  = rt(250, df = 10)
t.5   = rt(250, df = 5)
t.2   = rt(250, df = 2)
t.1   = rt(250, df = 1)

# --------------------------------------------------------------------
# Create a picture with 2 rows, 3 columns
# --------------------------------------------------------------------
par(mfrow = c(2,3))
    qqnorm(std.N, main = "Standard Normal")
    qqline(std.N)
    
    qqnorm(t.20, main = "t (20 degrees of freedom)")
    qqline(t.20)

# Main title for graph
# (yes, it needs to go here due to how R works)
mtext("Quantile-Quantile Plots Against Normal", 
      line = 2.75, # Vertical placement of the title
      cex = 1)     # Font size of title

    qqnorm(t.10, main = "t (10 degrees of freedom)")
    qqline(t.10)
    
    qqnorm(t.5, main = "t (5 degrees of freedom)")
    qqline(t.5)
    
    qqnorm(t.2, main = "t (2 degrees of freedom)")
    qqline(t.2)
    
    qqnorm(t.1, main = "t (1 degree of freedom)")
    qqline(t.1)

# --------------------------------------------------------------------
# For those who are curious, we can plot the theoretical
# densities against each other rather than using simulated
# draws. The syntax is more complicated so if you don't understand
# what the code is doing, check the picture instead -- 
# they're more important.
# --------------------------------------------------------------------
# The same plots as above, but this time against theoretical quantiles
par(mfrow=c(2,3))
qqnorm(qnorm(ppoints(250)), main = "Standard Normal")
qqline(qnorm(ppoints(250)))

qqnorm(qt(ppoints(250), df = 20), main = "t (20 degrees of freedom)")
qqline(qt(ppoints(250), df = 20))

# Main title for graph
# (yes, it needs to go here due to how R works)
mtext("Theoretical Quantile-Quantile Plots against Normal", 
      line = 2.75, # Vertical placement of the title
      cex = 1)     # Font size of title

qqnorm(qt(ppoints(250), df = 10), main = "t (10 degrees of freedom)")
qqline(qt(ppoints(250), df = 10))

qqnorm(qt(ppoints(250), df = 5), main = "t (5 degrees of freedom)")
qqline(qt(ppoints(250), df = 5))

qqnorm(qt(ppoints(250), df = 2), main = "t (2 degrees of freedom)")
qqline(qt(ppoints(250), df = 2))

qqnorm(qt(ppoints(250), df = 1), main = "t (1 degree of freedom)")
qqline(qt(ppoints(250), df = 1))
