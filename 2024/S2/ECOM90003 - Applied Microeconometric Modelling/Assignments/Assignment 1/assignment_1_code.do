************************************************************************
************** ECOM90003 - Applied Microeconometric Modelling  *********
****************************** ASSIGNMENT 1 ****************************
************************************************************************


************************************************************************
******************************* SET-UP *********************************
************************************************************************

clear // clear all data from memory
log close

set more off // pause more message 
  
cd "C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\Assignments\Assignment 1\"

global tablefile "C:\Users\joshc\OneDrive\Desktop\git\mae_unimelb\2024\S2\ECOM90003 - Applied Microeconometric Modelling\Assignments\Assignment 1\"


log using "assignment_1.log", replace // if you get log file open error message, insert: "log close" into command

* Import Data

use Educ_PolEcon_Data, clear


************************************************************************
********************************** Q8 **********************************
************************************************************************


//a) # of unique years and villages?

levelsof year // 6 different years

distinct v_id // There are 9855 unique villages





//b) 

// # schools constructed b/w 1974 & 1978?

summarize num_PSINPRES1980 if year == 1986, detail // 8810 schools were constructed


// Average number of schools on the villages?

mean num_PSINPRES1980 if year == 1986 // 0.894 is the average




//c) What prop. of villages had 0, 1 or 2 schools?

tabulate num_PSINPRES1980 // 0 == 32.14, 1 == 46.33, 2 == 21.53.



************************************************************************
********************************** Q9 **********************************
************************************************************************

// Creating table 1


preserve

duplicates drop v_id, force

tabulate ele1v_post92

restore


************************************************************************
******************************** TABLE 2 *******************************
************************************************************************

// Creating table 2


// Step 1

generate post92 = 0

replace post92 = 1 if year >= ele1v_post92

// Step 2

generate interaction = post92 * num_dev


// Creating actual table

local outcome_var dum doc safe

eststo clear

foreach y in `outcome_var' {
	
	eststo: quietly reg `y' post92 num_dev interaction, cl(idkab_num)
	eststo: quietly areg `y' post92 num_dev interaction, absorb(year) cl(idkab_num)
	eststo: quietly areg `y' post92 num_dev interaction, absorb(year v_id) cl(idkab_num)
	
}

esttab using "table_2.csv", b(%5.3f) se obslast r2 star(* 0.1 ** 0.05 *** 0.01) label replace keep(post92 interaction) title(Table 2. The effect of school construction on public good provision)



************************************************************************
******************************** TABLE 3 *******************************
************************************************************************

eststo clear

foreach y in `outcome_var' {
	
	eststo: quietly areg `y' post92 i.post92##i.num_PSINPRES1980, absorb(year v_id) cl(idkab_num)
	
}

esttab using "table_3.csv", b(%5.3f) se obslast r2 star(* 0.1 ** 0.05 *** 0.01) label replace title (Table 3. Intensity of School Construction) keep(post92 1.post92#1.num_PSINPRES1980 1.post92#2.num_PSINPRES1980)

