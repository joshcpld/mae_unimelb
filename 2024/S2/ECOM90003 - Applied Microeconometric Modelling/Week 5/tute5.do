************************************************************************
************** ECOM90003 - Applied Microeconometric Modelling  *********
**************************** Tutorial: Week 5 **************************
************************************************************************

// note: di command stands for "display" - use it to conduct arithmetic!

************************************************************************
******************************* SET-UP *********************************
************************************************************************

clear // clear all data from memory
log close

set more off // pause more message 
  
cd "C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\tutorial_data"

log using "week5.log", replace // if you get log file open error message, insert: "log close" into command

* Import Data

use airfare-1, clear


************************************************************************
********************************** Q1A *********************************
************************************************************************

reg lfare concen ldist ldistsq y98 y99 y00

// there is a shortcut to include variable with similar naming conventions in stata

// It is: "y*". This will include all vaiables taht start with "y". But, we need to change it so the "year" variable doesn't get impacted by this call.

foreach i in 98 99 00 {
	rename y`i' yr`i'
}

reg lfar concen ldist ldistsq yr*


di 2.718^(0.1*_b[concen]) - 1

// A 3.7%~ increase in fare




************************************************************************
********************************** Q1B *********************************
************************************************************************

reg lfar concen ldist ldistsq yr*, r

// Rule of using robust standard errors

// 1. Robust stnadrd errors performs poorly under small sample sizes.
// 2. When sample size is sufficiently large (i.e. robust s.e. are reliable), expect robust s.e.s to be larger than original s.e.s. This is because they explicitly account for heteroskedasticity.

// This makes the denominator for t-stats larger, and therefore smaller relative to the non-robust s.e.s







************************************************************************
********************************** Q1C *********************************
************************************************************************

di -_b[ldist]/(2*_b[ldistsq])

// Refer to answers for more detail


************************************************************************
********************************** Q1D *********************************
************************************************************************

xtset id year

xtreg lfar concen ldist ldistsq yr*, re // random effect model

// You can use the re model when rho in the output is close to one (rho = sigma_u / sigma_u + sigma_e). Thhis is because when its close to one, the estimates of RE and FE are approximately constant.

// When rho ~ 1, the variance of epsilon is small compared to the variance of alpha


xtreg lfar concen ldist ldistsq yr*, fe // fixed effect model




************************************************************************
********************************** Q1D *********************************
************************************************************************