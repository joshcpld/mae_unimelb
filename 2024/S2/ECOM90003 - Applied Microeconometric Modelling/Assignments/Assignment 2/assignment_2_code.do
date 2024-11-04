************************************************************************
************** ECOM90003 - Applied Microeconometric Modelling  *********
****************************** ASSIGNMENT 2 ****************************
************************************************************************

************************************************************************
******************************* SET-UP *********************************
************************************************************************

clear // clear all data from memory
log close

set more off // pause more message 
  
cd "C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\Assignments\Assignment 2\"

log using "assignment2.log", replace // if you get log file open error message, insert: "log close" into command


frame create temp1
frame create temp2

frame change temp1
use grade5_final, clear

frame change temp2
use grade4_final, clear

frame rename temp1 grade5
frame rename temp2 grade4


************************************************************************
******************************* GRADE 5 ********************************
************************************************************************

use grade4_final

******************************* TABLE 1 ********************************

// Using the data provided, replicate Panel A (columns 1-2 and 7-8) of Table 3 and label your replication "Table 1". 


// Generating means

frame change grade5

summarize classize

frame change grade4

summarize classize



// Generating actual table content


// Clear previous estimates
eststo clear

// Run regressions for grade5
frame change grade5
eststo: reg classize func1 tipuach, cl(schlcode)
estimates store model_grade5_1  // Store first model for grade5

eststo: reg classize func1 tipuach c_size, cl(schlcode)
estimates store model_grade5_2  // Store second model for grade5

// Run regressions for grade4
frame change grade4
eststo: reg classize func1 tipuach, r
estimates store model_grade4_1  // Store first model for grade4

eststo: reg classize func1 tipuach c_size, cl(schlcode)
estimates store model_grade4_2  // Store second model for grade4

// Switch back to the default frame to produce the combined table
frame change default

// Output the table with all four models
esttab model_grade5_1 model_grade5_2 model_grade4_1 model_grade4_2 using "table_1.csv", ///
    b(%5.3f) se obslast r2 star(* 0.1 ** 0.05 *** 0.01) replace ///
    title("Model 1 (Grade 5)" "Model 2 (Grade 5)" "Model 1 (Grade 4)" "Model 2 (Grade 4)") ///
	stats(rmse r2 N)


******************************* TABLE 2 ********************************

// Replicate Columns 1 to 12 of Table 2 in the paper. Include it in your assignment appendix, labelled as "Table 2".


// Generating means

frame change grade5

summarize avgverb
summarize avgmath

frame change grade4

summarize avgverb
summarize avgmath



//Generating actual table output

local outcome_var avgverb avgmath

// Clear previous estimates
eststo clear

// List of dataframes to loop through
local dataframes grade5 grade4

// Loop through each dataframe
foreach df in `dataframes' {
    // Change to the current dataframe
    frame change `df'
    
    // Loop through each outcome variable
    foreach y in `outcome_var' {
        // Run regressions and store results
        eststo: reg `y' classize, cl(schlcode)
        eststo: reg `y' classize tipuach, cl(schlcode)
		eststo: reg `y' classize tipuach c_size, cl(schlcode)
    }
}

// Switch back to the default frame to produce the combined table
frame change default

// Output the table with all models into "table_2.csv"
esttab using "table_2.csv", ///
    b(%5.3f) se obslast replace ///
    title("Regression Results for Grade 5 and Grade 4") ///
    stats(rmse r2 N) ///
	
	
	
******************************** TABLE 3 *******************************

// Replicate Columns 3 - 6 and 9 – 12 in Panel A of Table 3 in the paper. Include it in your assignment appendix, labelled as "Table 3".

//Generating actual table output

local outcome_var avgverb avgmath

// Clear previous estimates
eststo clear

// List of dataframes to loop through
local dataframes grade5 grade4

// Loop through each dataframe
foreach df in `dataframes' {
    // Change to the current dataframe
    frame change `df'
    
    // Loop through each outcome variable
    foreach y in `outcome_var' {
        // Run regressions and store results
        eststo: reg `y' func1 tipuach, cl(schlcode)
        eststo: reg `y' func1 tipuach c_size, cl(schlcode)
    }
}

// Switch back to the default frame to produce the combined table
frame change default

// Output the table with all models 
esttab using "table_3.csv", ///
    b(%5.3f) se obslast replace ///
    title("Regression Results for Grade 5 and Grade 4") ///
    stats(rmse r2 N) ///





******************************** TABLE 4 *******************************

// Replicate Columns 1-3 and 7-9 of Table 4 in the paper. Include it in your assignment appendix, labelled as "Table 4".


local outcome_var avgverb avgmath

// Clear previous estimates
eststo clear

// List of dataframes to loop through
local dataframes grade5

// Loop through each dataframe
foreach df in `dataframes' {
    // Change to the current dataframe
    frame change `df'
    
    // Loop through each outcome variable
    foreach y in `outcome_var' {
        // Run 2SLS regressions and store results
        eststo: ivregress 2sls `y' (classize = func1) tipuach, cl(schlcode)
        eststo: ivregress 2sls `y' (classize = func1) tipuach c_size, cl(schlcode)
        eststo: ivregress 2sls `y' (classize = func1) tipuach c_size c_size2, cl(schlcode)
    }
}

// Switch back to the default frame to produce the combined table
frame change default

// Output the table with all models 
esttab using "table_4.csv", /// 
    b(%5.3f) se obslast replace /// 
    title("Regression Results for Grade 5") /// 
    stats(rmse r2 N) 






















