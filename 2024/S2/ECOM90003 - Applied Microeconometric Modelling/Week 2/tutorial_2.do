************************************************************************
************** ECOM90003 - Applied Microeconometric Modelling  *********
**************************** Tutorial: Week 2 *************************
************************************************************************

// note: di command stands for "display" - use it to conduct arithmetic!

************************************************************************
******************************* SET-UP *********************************
************************************************************************

clear // clear all data from memory
log close

set more off // pause more message 
  
cd "C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\tutorial_data\"

log using "week2.log", replace // if you get log file open error message, insert: "log close" into command

* Import Data

use beauty, clear
 
 

************************************************************************
********************************** Q2 **********************************
************************************************************************

// a) Find the proportion of men and women classified as having above and below average looks. For which gender, who has more either side of average looks?

tab female abvavg // This gives you counts
tab female abvavg, row // This gives you counts and percentages

* Men have a higher proportion of people above average looks compared to women

tab female belavg, row

* There are more women below average looks compared to men






// b) Test the null hypothesis that the  population fractions of above averge looking men and women are the same (assume the male/female variance is the same). Report the one-sided p-value that the fraction is higher for women.

*There are two ways to do this:

* 1 - two same t-test

ttest abvavg, by(female) // p-value == 0.07. Therefore, you can only reject the null hypothesis of 

reg abvavg female


// CLARIFY ANSWER WITH XINRAN FOR INTERPRETATION





// c) Estimate the model separately for men and women and report the results in the usual from (as set out in the textbook in Chapter 4). In each case, interpret the coefficient on belavg. Explain 

reg lwage belavg abvavg if female == 0 // For men, being below average reduce wages by around 20%

reg lwage belavg abvavg if female == 1 // For women, being below average reduces wages by around 13%


di (2.718^_b[belavg]-1)*100

