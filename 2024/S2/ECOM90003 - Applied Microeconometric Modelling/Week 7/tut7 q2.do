************************************************************************
************** ECOM90003 - Applied Microeconometric Modelling  *********
**************************** Tutorial: Week 7 **************************
************************************************************************

// note: di command stands for "display" - use it to conduct arithmetic!

************************************************************************
******************************* SET-UP *********************************
************************************************************************

clear // clear all data from memory
//log close

set more off // pause more message 
  
cd "C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\tutorial_data\"

log using "week7.log", replace // if you get log file open error message, insert: "log close" into command



************************************************************************
********************************** Q2 **********************************
************************************************************************

// These two questions investigate two scenarios where the IV doesn't work without controls:


// 1. IV is correlated with u_i unless we include controls (exogeneity x)

//2. IV is not correlated with X unless we include controls (relevance x)
		// The cost of the second is large se and ci



* Import Data

use CARD-2, clear


// a) 

reg IQ nearc4



reg IQ nearc4 smsa66 reg6*



************************************************************************
********************************** Q3 **********************************
************************************************************************

use HTV-2, clear

reg lwage educ

reg educ ctuit