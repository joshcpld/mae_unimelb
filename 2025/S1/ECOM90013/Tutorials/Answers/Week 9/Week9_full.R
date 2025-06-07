# --------------------------------------------------------------------
# ECOM40006/90013 Econometrics 3
# Semester 1, 2021
# Author: Daniel Tiong
# Week 9: Monte Carlo simulations in R
# --------------------------------------------------------------------

rm(list=ls())

# Start the clock!
# ptm = proc.time()

# Load in libraries
library(AER)
library(stargazer)
library(MASS)

# Set random seed
set.seed(42)

# Number of repeat samples
nreps = 1000

# Single iteration
# Number of observations per sample
n = 100

# Establish the critical values for the problem
#    - To explain what the qt command does: it's the quantile function.
#    Basically we're looking to see *what* value of the t-distribution
#    has 97.5% of the probability mass before it. The reason for using
#    2.5% rather than 5% is because we're using a two-sided test:
#    for a 5% significance level we double everything.)
#
#    - For the F-statistic on the other hand: this is a distribution
#    that is defined on the positive real numbers (like the chi-squared).
#    It accepts two degrees of freedom, F_{q,n-k-1}.
#        - q    : the number of restrictions tested
#        - n-k-1: number of obs minus the number of regressors
#                 INCLUDING THE INTERCEPT.
#    For this one we use the 95th percentile; it's one sided.
cv.t    = qt(0.975, df = n-3) # crit value, t-dist
cv.F    = qf(0.95, df1 = 3, df2 = n-4)

# Relevant critical value for the chi-squared distribution
cv.chisq = qchisq(0.95, df = 3)
    
# Storage matrices
t.stats = matrix(nrow = nreps, ncol = 3)
F.stats = matrix(nrow = nreps, ncol = 1)

for (i in 1:nreps) {
    # Generate observations on our variables
    x1 = rnorm(n,  mean = 0,  sd  = 3)
    x2 = runif(n,  min  = -1, max = 1)
    x3 = rchisq(n, df   = 2)
    
    # Generate a dependent variable
    # (which is amusingly not dependent on the others)
    y  = rnorm(n) # automatically defaults to N(0,1)
    
    # Conduct a regression
    eq = lm(y ~ x1 + x2 + x3)  
    
    # Pull out the t-statistics and get absolute values for testing
    # (All the t-stats are located in column 3.
    # The [2,4] index only pulls the t-stats on coefficients.
    # Each row provides the t-stats for a single sample.)
    t.stats[i,] = abs(coeftest(eq)[,3])[2:4]
    
    # Store the F statistic of regression
    F.stats[i] = summary(eq)$fstatistic[1]
}

# Rejection frequencies under the prescribed rejection rule in the Week 9 questions
# ---------------------------------------------------------------------------------
# For each of the t-statistics, calculate whether it is above
# the designated critical value (and hence a rejection of H_0)
t = t.stats > cv.t

# Sum up the number of rejections across each of the rows
# (i.e. this will help us decide the final rejection)
t.rej = apply(t,1,sum) # the "1" means "do it over the rows" -- 2 would be for columns

# Reject the null hypothesis if any coefficient is individually significant
overall.rej = t.rej > 0

print("The standard t critical value for this experiment is")
print(cv.t)
print("...and the size using the standard critical value is")
print(mean(overall.rej))
print("If we used the F-statistic, we get a rejection frequency of")
print(mean(F.stats > cv.F))
print("and if we compared it to the asymptotic chi-squared (3df) we get")
print(mean(3*F.stats > cv.chisq))
# as a final comment: so basically individual t-tests suck for joint significance

# PART 2. BISECTION METHOD for a 5% size
# ---------------------------------------------------------------------------------
# Set the maximum number of iterations (if convergence is not achieved, increase it)
niter     = 100

# set initial value for rejection of critical value
t.upper   = 100 # Upper bound for grid search
t.lower   = 0   # Lower bound for grid search
midpoint  = matrix(nrow = niter, ncol = 1)

# Set tolerance level for bisection
# (i.e. how close do we get to the sig. level before we stop?)
tol       = 0.001 # can widen this later based on what we find

# Significance level (desired size)
sig.level = 0.05

# ReLeaSe tHe biSecTiOn mEthOd
# ---------------------------------------------------------------------------------
for (j in 1:niter) {
    # Calculate the midpoint
    t.mid   = (t.upper + t.lower)/2
    
    # Calculate the rejection frequencies
    # Step 1. Which t-stats are rejected?
    rej.upper = t.stats > t.upper
    rej.lower = t.stats > t.lower
    rej.mid   = t.stats > t.mid
    
    # Step 2. Add up the number of rejections by sample
    rej.num.upper = apply(rej.upper,1,sum)
    rej.num.lower = apply(rej.lower,1,sum)
    rej.num.mid   = apply(rej.mid,1,sum)
    
    # Step 3. Grab the rejection frequency
    size.upper = mean(rej.num.upper > 0)
    size.lower = mean(rej.num.lower > 0)
    size.mid   = mean(rej.num.mid   > 0)
    
    # Step 4. Calculate the objective function at each point
    obj.upper = size.upper - sig.level
    obj.lower = size.lower - sig.level
    obj.mid   = size.mid   - sig.level
    
    # The intervals that we will continue with depend on the signs
    # of the functions. For example, what we are computing is a function
    # of the critical value t, namely of the form
    #   F(t) = size - 0.05 = 0.
    # So if the value of t satisfying this equation between the lower and 
    # middle bounds, f(lower) and f(mid) will be of opposite signs. 
    #   - That means that we should continue with that interval.
    # Now if f(mid) and f(upper) are of opposite signs instead, then the root
    # should be there and we'll continue with that interval instead.
    
    # If the midpoint satisfies the stopping rule, then terminate early.
    if (abs(obj.mid) < tol) {
        t.crit = t.mid
        break # Stop the -for- loop early
    } else if (obj.lower * obj.mid < 0) {
        t.upper = t.mid   
    } else {
        t.lower = t.mid   
    }
    
    # Invoke the stopping rule before proceeding to next iteration.
    # (don't worry about this. it just works.)
    if ( t.upper - t.lower < tol ) {
        if ( (abs(obj.lower) < abs (obj.upper)) & (abs(obj.lower) < tol) ) {
            t.crit = t.lower
            break # Terminate loop
        } else if ( abs(obj.upper) < tol ) {
            t.crit = t.upper
            break
        }
    }
}

# Comparison of critical values that achieve sizes
print("The critical value that achieves a 5% size")
print(t.crit)

print("But the standard critical value is")
print(cv.t)
print("...and the size using the standard critical value is")
print(mean(overall.rej))