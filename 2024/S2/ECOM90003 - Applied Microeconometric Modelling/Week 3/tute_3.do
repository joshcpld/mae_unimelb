************************************************************************
************** ECOM90003 - Applied Microeconometric Modelling  *********
**************************** Tutorial: Week 3 *************************
************************************************************************

// note: di command stands for "display" - use it to conduct arithmetic!

************************************************************************
******************************* SET-UP *********************************
************************************************************************

clear // clear all data from memory
capture log close 

set more off // pause more message 
  
cd "C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\Week 3\"

log using "week3.log", replace // if you get log file open error message, insert: "log close" into command

* Import Data

use CardKruegerTute3-Data, clear


************************************************************************
********************************** Q6 **********************************
************************************************************************


//Necessary dummies


gen post=(time==2)
lab var post "Dummy: Post treatment"

gen NJ=(stater==1)
lab var NJ "Dummy: New Jersey"

gen NJ_post=NJ*post
label var NJ_post "Interaction"


//Regressions

* Need to use robust standard error, and use firms which are open in both periods. For robust standard errors just use the ",r" or ",robust" command when generating the regression

*Conditionally selecting observations is a bit more complicated.

*Use egen: calculated values for a variable across rows in a specific group.

*bys = group_by and arrange_by function together

bys id: egen x1 = count(wage_st)  //Indicator variable for missing wages

// x1 = 2 if no missing wages, x1 = 1 if missing wage

bys id: egen x2=total(closed) // Indicator for restaurant being closed
//x2=0 if no closure, x=1 if closed in second wave


*Now generate the regressions

reg wage_st NJ post NJ_post if x1==2 & x2 ==0, robust
reg emptot NJ post NJ_post if (x1==2 & x2 ==0) | x2==1, robust


* Generating chain dummies
label list

gen BK=(chainr==1)
gen KFC=(chainr==2)
gen RR=(chainr==3)


* Now put in dummy variables


reg emptot NJ post NJ_post co_owned BK KFC RR if (x1==2 & x2 ==0) | x2==1, robust


save Tute_answer_w3
