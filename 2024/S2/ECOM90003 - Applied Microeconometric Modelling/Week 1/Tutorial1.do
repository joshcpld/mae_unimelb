/* 
Introduction to stata
*/

/*This program runs a regression of lnwages and plots predictions*/

/*This is a complete version of the session */

// Use Ctrl + D to run a highlighted section

clear // clear all data from memory
set more off // pause more message 

// Change directory (folder to import data from + save data/plots/tables to)
// Change this path to the folder for tutorial materials on your computer
cd"C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\Week 1\"

// Create log file to save all commands ran in the session
log using "StataLab.log", replace

// Import data: make sure the files are in the directory folder. If not, then specify the correct pathway in the name, i.e. "Tutorial 1/Wage.xlsx"
	// Excel spreadsheet
	import excel using WAGE, firstrow clear
	import excel using "WAGE.xlsx", firstrow clear
	
	// Text file, csv files,...
	import delimited using "WAGE.txt", clear
	
	// Dta files (Stata's format)
	use "~/Dropbox/Tutoring/Semester 2 2022/AMM/Tutorials/Tutorial 1/WAGE.dta", clear
	use WAGE, clear	

////////////////////////////////
// Question 1	
	
	// Preliminary checks 
	list in 1/10
	sort educ // Ascending sort of education
	sort educ wage // Ascending sort of wage within category of education
	list in 1/10	
	
	sum wage 
	sum _all, detail // Summary statistics 
	help summarize // Open help document
	count if missing(wage)
	misstable sum, all // Report on missing values
	codebook wage // Properties of variables
	tab educ // Tabulate number of unique values
	tab educ gender 
	tab educ, sum(wage)
	
	// Generate necessary variables 
	gen lnwage=ln(wage)
	label var lnwage "Ln average hourly earnings"
	
	gen expersq=exper^2
	label var  expersq "years of potential experience squared"

	tab gender
	tab gender, nolabel // remove label to show underlying numeric values

	gen female=1 if gender==2
	replace female=0 if gender==1 // if the variable has already been created need to use the replace command. i.e. replace is for editing.
	label var female "Female dummy"
	label define female 1"female" 0"male"
	label values female female
	
	gen femmarried=female*married
	label var femmarried "Female X Married"
	
	// Regression
	regress lnwage educ exper expersq female married nonwhite femmarried
		// Predict log wage using estimated model
		predict lnwagehat
		label var lnwagehat "Predicted values ln wage"
		twoway (scatter lnwagehat lnwage) || (line lnwage lnwage) // true value against predicted value & 45o line
	
	// Part b
		// Single female - single male
		di (2.718^_b[female]-1)*100 // calculate percentage change
		test female // test if coefficient = 0 
		
		// Married male - single male
		di (2.718^_b[married]-1)*100 
		test married 
		
		// Married female - single male 	
		di (2.718^(_b[female] + _b[married] + _b[femmarried])-1)*100
		test _b[female] + _b[married] + _b[femmarried] = 0
		
	// Part c 
		// Married female - married male 
		di (2.718^(_b[female] + _b[femmarried])-1)*100
		test _b[female] + _b[femmarried] = 0
				
		// Single female - married male
		di (2.718^(_b[female] - _b[married])-1)*100
		test _b[female] - _b[married] = 0
		
		// Single male - married male
		di (2.718^(-_b[married])-1)*100
		test -_b[married] = 0
	
////////////////////////////////
// Question 2	

	// Generate necessary variables 
	gen male = 1-female
	gen manmarried = male*married
	gen femsing = female*(1-married)
	gen mansing = male*(1-married)
	label var manmarried "Male X Married"
	label var femsing "Female X Single"
	label var mansing "Male X Single"

	// Regression
	reg lnwage educ exper expersq femmarried manmarried femsing mansing nonwhite, nocons
	
	// Part b 
		// Single female - single male 
		di (2.718^(_b[femsing] - _b[mansing])-1)*100 
		test _b[femsing] - _b[mansing] = 0
		
		// Married male - single male 
		di (2.718^(_b[manmarried] - _b[mansing])-1)*100 
		test _b[manmarried] - _b[mansing] = 0
		
		// Married female - single male
		di (2.718^(_b[femmarried] - _b[mansing])-1)*100 
		test _b[femmarried] - _b[mansing] = 0

// End 		
save WAGE_new, replace // save data currently in memory - i.e. with the newly generated variables
log close // Create the log file for this session
	
