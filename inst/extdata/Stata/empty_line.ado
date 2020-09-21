****************************
*add empty line before or after a pre-specified position
**************************
*Lukas Buetikofer
*03.09.2015
***************************

/*
*syntax
empty_line [if] [in] [, generate(name) position(string)]

position: "before" or "after"
returns line with new entry in r(insert_line)
optional new variabl that is 2 for the new entry
*/

cap program drop empty_line
program empty_line, rclass 
version 13.0
syntax [if] [in] [, GENerate(name) position(string)]

marksample touse	
tempvar seq
tempvar expanded

if !inlist("`position'" ,"after","before","") {
	dis "use after or before as position arguments"
}
else {
	qui gen `seq'=_n
	qui expand 2 if `touse'==1, gen(`expanded')

	if "`position'"=="" {
		qui replace `seq'=`seq'+0.1 if `expanded'==1 
	}
	else {
		if "`position'"=="after" {
			qui replace `seq'=`seq'+0.1 if `expanded'==1
		}
		if "`position'"=="before" {
			qui replace `seq'=`seq'-0.1 if `expanded'==1
		}
	}
	sort `seq'
	qui sum `seq' if `expanded'==1
	cap local insert_line=ceil(`r(max)')

	if "`generate'" != "" {
		confirm new variable `generate'
		qui gen `generate'=`expanded'
	}

	foreach var of varlist _all {
		local vartype: type `var'
		if strpos("`vartype'","str")>0 {
			qui replace `var'="" if `expanded'==1
		}
		else {
			qui replace `var'=. if `expanded'==1
		}
	}		
	cap return scalar insert_line=`insert_line'
}

end			
