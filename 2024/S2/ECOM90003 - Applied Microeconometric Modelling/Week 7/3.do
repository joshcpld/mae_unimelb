************************************************************************
************** ECOM90003 - Applied Microeconometric Modelling  *********
**************************** Tutorial: Week 7 **************************
************************************************************************

// note: di command stands for "display" - use it to conduct arithmetic!

************************************************************************
******************************* SET-UP *********************************
************************************************************************

clear // clear all data from memory
log close

set more off // pause more message 
  
cd "C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\tutorial_data\"

log using "week7_3.log", replace // if you get log file open error message, insert: "log close" into command

* Import Data

use HTV-2, clear


************************************************************************
********************************** Q3 **********************************
************************************************************************

