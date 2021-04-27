/* ******************************************** */ 
/* Study: XXX                        */ 
/* Author: Alan Haynes                          */
/* Purpose: program to get labels prior to collapsing         */  
/* Date created: 24.07.2018                     */ 
/* Last update:                       */ 
/* External routines: None                      */ 
/* ******************************************** */ 

cap program drop get_labs

program def get_labs 
	syntax [varlist], Saving(string) [ds(string)]
	
	if "`varlist'" == "" {
		ds 
		local varlist = r(varlist)
	}
	
	cap file close labls
	file open labls using "`saving'.do", write replace
	
	foreach var of local varlist {
		*di "z"
		local lab : var label `var'
		file write labls `"cap label var `var' "`lab'""' _n //"
	}
	
	file write labls "" _n
	
	label save using "`saving'_values", replace 
	
	file write labls "do `saving'_values " _n
	file write labls "" _n
	
	foreach var of local varlist {
		local vallab : value label `var'
		if "`vallab'" != " " {
			file write labls "cap label value `var' `vallab'" _n
		}
	}
	
	file write labls "" _n
	
	file close labls
	
end


* SUGGESTED USE :
* get_labs, s("labels")
* collapse (mean) ..., by(whatever)
* do "labels"









