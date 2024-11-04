************************************************************************
************** ECOM90003 - Applied Microeconometric Modelling  *********
**************************** Tutorial: Week 9 **************************
************************************************************************

// note: di command stands for "display" - use it to conduct arithmetic!

************************************************************************
******************************* SET-UP *********************************
************************************************************************

clear // clear all data from memory
log close

set more off // pause more message 
  
cd "C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\tutorial_data\"

log using "week9.log", replace // if you get log file open error message, insert: "log close" into command

* Import Data

use loanapp-1, clear


************************************************************************
********************************** Q1 **********************************
************************************************************************

// Prob of graduating if study = 10

di 2.718^(-1.17+0.24*3+0.00058*1200+0.073*10) / (1+2.718^(-1.17+0.24*3+0.00058*1200+0.073*10))

// Prob of graduating if study = 5

di 2.718^(-1.17+0.24*3+0.00058*1200+0.073*5) / (1+2.718^(-1.17+0.24*3+0.00058*1200+0.073*5))

// The difference? 7.8ppts.

di 2.718^(-1.17+0.24*3+0.00058*1200+0.073*10) / (1+2.718^(-1.17+0.24*3+0.00058*1200+0.073*10)) - 2.718^(-1.17+0.24*3+0.00058*1200+0.073*5) / (1+2.718^(-1.17+0.24*3+0.00058*1200+0.073*5))


************************************************************************
********************************** Q2 **********************************
************************************************************************

//b)

reg approve white, r

//c) 

reg approve white hrat obrat loanprc unem male married dep sch chist pubrec mortlat1 mortlat2 vr, r

//d) 

gen white_obr = white*obrat

reg approve white white_obr hrat obrat loanprc unem male married dep sch chist pubrec mortlat1 mortlat2 vr, r



************************************************************************
********************************** Q3 **********************************
************************************************************************

//a)

probit approve white


//b) 

probit approve white hrat obrat loanprc unem male married dep sch chist pubrec mortlat1 mortlat2 vr, r

//d) Refer to cheat sheet for explanation of margins function

probit approve white hrat obrat loanprc unem male married dep sch chist pubrec mortlat1 mortlat2 vr, r

margins, dydx(white)


logit approve white hrat obrat loanprc unem male married dep sch chist pubrec mortlat1 mortlat2 vr, r

margins, dydx(white)