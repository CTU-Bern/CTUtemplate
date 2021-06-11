/* ******************************************** */
/* Study: name */
/* Purpose: Export from REDCap  */
/* Author: Lukas Buetikofer, adapted from API EXPORT FROM REDCAP, Alan Haynes, 12th May 2016 */
/* Date created:  */
/* Last update:  */
/* External routines: None */
/* ******************************************** */


****************
*export from database to raw data
**************

*curl
//path to curl has to be defined in master file, global curl
dis "$curl"

*Token
//has to be defined in the master file, global mytoken
//disp "$mytoken"

*API URL
//has to be defined in the master file, global url
dis "$url"



*use curl to export the tables to CSVs and then dta 
foreach tab in "project" "metadata" "instrument" "arm" "event" "formEventMapping" "record" "user" {
	shell $curl --output "$rd/`tab'.csv" --form token=$mytoken --form content=`tab' --form format=csv --form exportDataAccessGroups=TRUE $url
	import delim "$rd/`tab'.csv", clear bindquotes(strict)  encoding(utf-8)
	save "$rd/`tab'", replace
}


*************
*drop  info from meta_data (if applicable)
***************
//use "$rd/metadata", clear
//drop if form_name=="contact_info"
//save "$rd/metadata", replace


***********************
* prepare files for xlabel and xvarlabel  from metadata
********************************

*variable labels
use "$rd/metadata", clear
drop if field_type == "descriptive"
keep field_name field_label 
rename field_name var
rename field_label text
save "$ld/varlabels", replace

*variable types
use "$rd/metadata", clear
tab field_type
//checkboxes and dropdown/radios have to be handled seperately, checkboxes have one variable per level


*a) radio/dropdown 
use "$rd/metadata", clear

count if field_type == "radio" | field_type=="dropdown"
local radios = r(N)
if `radios' > 0 {

	keep if field_type == "radio" | field_type=="dropdown"
	gen cat = field_name + "_l"

	preserve
		rename field_name var 
		keep var cat
		order var cat
		save "$ld/varvallabels", replace
	restore
	
	split select_choices_or_calculations, gen(opt) parse("|")
	reshape long opt, i(cat) j(opti)

	keep opt cat
	drop if opt == ""
	
	//replace commas if there are more than 1:
	egen noc=nss(opt) , find(",")
	assert strpos(opt,"*")==0
	replace opt = reverse(subinstr(reverse(opt),",","*",noc-1))
	drop noc
	egen noc=nss(opt) , find(",")
	assert noc==1
	drop noc
	
	split opt, parse(",")
	replace opt2 = trim(opt2)
	drop opt
	
	//change back to comma
	replace opt2 = subinstr(opt2,"*",",",.)

	rename opt1 keynr
	rename opt2 keytext
	destring keynr, replace
	order cat keynr keytext
	save "$ld/vallabels", replace
}


*b) yesno
use "$rd/metadata", clear
count if field_type == "yesno"
local yesno = r(N)
if `yesno' > 0 {
	
	keep if field_type == "yesno"
	rename field_name var 
	keep var
	gen cat="yesno_l"
	
	preserve
	append using "$ld/varvallabels"
	save "$ld/varvallabels", replace
	restore
	
	expand 2, gen(keynr)
	gen keytext="Yes" if keynr==1
	replace keytext="No" if keynr==0
	keep cat keynr keytext
	append using "$ld/vallabels"
	save "$ld/vallabels", replace
	
}


*c) check box 
use "$rd/metadata", clear

//var labels
preserve
count if field_type == "checkbox"
local boxes = r(N)
if `boxes' > 0 {
	keep if field_type == "checkbox"
	split select_choices_or_calculations, gen(opt) parse("|")
	reshape long opt, i(field_name) j(opti)
	drop if opt == ""

	drop select_choices_or_calculations
	
	split opt, parse(",")
	replace opt1 = trim(opt1)
	replace opt2 = trim(opt2)

	gen var = field_name + "___" + opt1
	gen text = field_label + " = " + opt2
	keep var text 
	save "$ld/checklabels", replace
	
}
restore

// val labels
preserve
count if field_type == "checkbox"
local boxes = r(N)
if `boxes' > 0 {
	keep if field_type == "checkbox"
	split select_choices_or_calculations, gen(opt) parse("|")
	reshape long opt, i(field_name) j(opti)
	drop if opt == ""

	drop select_choices_or_calculations
	
	split opt, parse(",")
	replace opt1 = trim(opt1)
		
	gen var = field_name + "___" + opt1
	gen cat = "checkuncheck"
	keep var cat 
	save "$ld/checkvarvallabels", replace
	
	keep in 1/2
	keep cat
	gen byte keynr = _n - 1
	gen keytext = "unchecked" if keynr == 0
	replace keytext = "checked" if keynr == 1
	
	save "$ld/checkvallabels", replace

}
restore


*identify date variables
use "$rd/metadata", clear
keep if strpos(text_validation_type_or_show_sli,"date")>0
keep field_name
levelsof field_name, local(datevars)
save "$ld/datvars",replace


****************
* labeling
****************

use "$rd/record", clear

//destring date variables
foreach var of local datevars {
	tostring `var', replace
	gen _date_ = date(`var',"YMD")
	order _date, after(`var')
	drop `var'
	rename _date_ `var'
	format `var' %td
}

//label variables
xvarlabel , varinfo("$ld/varlabels")

//label values
if `radios' > 0 {
	xlabel , varinfo("$ld/varvallabels") labinfo("$ld/vallabels")
}

//checkboxes, special case
if `boxes' > 0 {
	xvarlabel , varinfo("$ld/checklabels")
	xlabel , varinfo("$ld/checkvarvallabels") labinfo("$ld/checkvallabels")
}

save "$ld/record", replace


*****************
*generate single forms
***************
use "$rd/formEventMapping", clear
rename form form_name
mmerge form_name using "$rd/metadata", type(n:n)
assert _merge==3
drop if field_type == "descriptive"
save "$ld/varforms", replace


use "$ld/varforms", clear
tab form_name
levelsof form_name, local(fname)

foreach f of local fname {
	
	use "$ld/varforms", clear
	keep if form_name=="`f'"
	
	preserve
	keep unique_event_name
	duplicates drop
	rename unique_event_name redcap_event_name
	save "$temp/keepforms", replace
	restore
	
	local keeplist
	
	//non-checkbox variables
	levelsof field_name if field_type!="checkbox", local(vars) 
	foreach l of local vars {
			local keeplist `keeplist' `l'
	}	
	//checkbox variables: several entries
	levelsof field_name if field_type=="checkbox", local(vars)
	foreach l of local vars {
			local keeplist `keeplist' `l'*
	}
	

	use "$ld/record", clear	
	mmerge redcap_event_name using  "$temp/keepforms", type(n:1)
	keep if _merge==3
	sort record_id
	local fs=substr("`f'_complete",1,32)
	label define formstat_l 0 "Incomplete" 1 "Unverified" 2 "Complete", replace	
	label val `fs' formstat_l
	
	//repeating form indicator
	cap confirm variable redcap_repeat_instrument redcap_repeat_instance
	if _rc {
		keep record_id redcap_event_name `keeplist' `fs'
	}
	else {
		keep record_id redcap_event_name  redcap_repeat_instrument redcap_repeat_instance `keeplist' `fs'
	}
	
	save "$fd/`f'", replace
}








