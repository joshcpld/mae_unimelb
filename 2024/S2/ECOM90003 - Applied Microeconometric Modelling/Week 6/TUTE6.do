************************************************************************
************** ECOM90003 - Applied Microeconometric Modelling  *********
**************************** Tutorial: Week X **************************
************************************************************************

// note: di command stands for "display" - use it to conduct arithmetic!

************************************************************************
******************************* SET-UP *********************************
************************************************************************

clear // clear all data from memory
log close

set more off // pause more message 
  
cd "C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\tutorial_data\"

log using "weekX.log", replace // if you get log file open error message, insert: "log close" into command

* Import Data

use WAGE2-3, clear


************************************************************************
********************************** Q3 **********************************
************************************************************************

//a) 

reg lwage educ sibs




//b)

reg educ brthord

// This is implicitly the first step of 2SLS if you use brthord as your instrument.





//c) 


// don't use "ivreg" - this is a retired state command.

ivregress 2sls lwage (educ=brthord)



//d) 



ivregress 2sls lwage sibs (educ=brthord)


reg educ brthord sibs
predict educhat

corr educhat sibs