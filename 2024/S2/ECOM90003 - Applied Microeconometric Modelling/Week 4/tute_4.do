************************************************************************
************** ECOM90003 - Applied Microeconometric Modelling  *********
**************************** Tutorial: Week 4 **************************
************************************************************************

// note: di command stands for "display" - use it to conduct arithmetic!

************************************************************************
******************************* SET-UP *********************************
************************************************************************

clear // clear all data from memory
log close

set more off // pause more message 
  
cd "C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\tutorial_data\"

log using "week4.log", replace // if you get log file open error message, insert: "log close" into command

* Import Data

use MURDER_new-1, clear





************************************************************************
********************************** Q3 **********************************
************************************************************************

// A)

reg mrdrte d93 exec unem if year != 87, r

// No, you don't estimate a deterrent effect of capital punishment. All the values are insignificant.

// Perhaps accounting for alpha_i using first difference model could be the solution.









// B)

// To compute the FD estimates we need to do some data maniuplation and reshape it properly.

//First, you need to make sure youre data is ordered in the correct order: by states first and then by year to make sure state obs are in order.

sort id year

//Now we can actually start develop our FD model.


// This is how state retrieves the previous observation:

by id: gen mrdrte_pst = mrdrte[_n-1]

// Now we can generate the first_differences manually for all the variables

by id: gen d_mrdrte = mrdrte - mrdrte[_n-1] 

by id: gen d_exec = exec - exec[_n-1] 
by id: gen d_unem = unem- unem[_n-1] 


reg d_mrdrte d_exec d_unem if year==93
reg d_mrdrte d_exec d_unem if year==93, r
 
//From this we see that yes, increasing the number of executions does have a negative impact on the the murder rate (-.1 beta) and is statistically significant.

// If there is no obvious choice for what kind of standard errors to use than choose the method which generates the largest p-value. If is still rejects the null, then your results are unambiguous.


// to make your life easier, note the method for generating the differences semi-automatically:

	// First, you need to make a time variable with increments of 1 (else stata will freak out and can't handle it)
	
gen time=1

replace time=2 if year==90
replace time=3 if year==93

	// Then, use the xtset command to tell Stata this is a panel data set.

xtset id time


reg D.mrdrte D.exec D.unem if year==93, r // "D." operator tells Stata you want the first difference automatically

gen d2_mrdrte=D.mrdrte


// If you want to get really tech, end of lecture 3 slides show you automate the generation of the differenced variables using a for-loop.
