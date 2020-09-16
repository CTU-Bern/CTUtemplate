*!1.1.4 22Jan2008
capture program drop xlabel

program define xlabel

 	syntax [varlist], [VARinfo(str) LABinfo(str) UNASSign]  /*
     	*/	 [VFIELDnames(str)] [LFIELDnames(str)] [NOMODify] /*
     	*/ 	 [omit(str)] [savedo(str)] [replace]
	
	********************************************
	local includelist `"`varlist'"'

	* if varlist not specified: _all will be automatically assumed (by STATA)
	
	* check if varlist is not specified
	gettoken piece : 0,parse(",")
	if `"`piece'"' == "" | `"`piece'"' == "," local hasvarlist = 0
	else local hasvarlist = 1
	local piece ""

	*expected names: varname file: "key" = varname, "name" = valuelabel name
	local vvn_var "var"
	local vvn_cat "cat"
	*expected names: labelfile: "key" = labelname, "nr" = valuelabel value, "text" = valuelabel label
	local lvn_cat "cat"
	local lvn_nr "keynr"
	local lvn_text "keytext"

	********************************************
	*syntax / option checks
	
	* remark: omit(labellist) means that neither label define with labels on list
	* nor assigning with label values will be performed 
	
	* remark: savedo(do-filename) means that no changes will be performed in
	* STATA working dataset (no label drop, label define, label values). Only
	* writing label define commands in specified do-file.
	
	*options varinfo and labinfo compulsory unless option unassign speified
	if `"`varinfo'"' == "" & `"`unassign'"' == "" {
		di as err "option varinfo() required"
		exit 198
	}

	if `"`labinfo'"' == "" & `"`unassign'"' == "" {
		di as err "option labinfo() required"
		exit 198
	}
	* unassign may not be combined with other options
	if `"`unassign'"' != "" & ///
		(`"`vfieldnames'"' != "" | `"`lfieldnames'"' != "" | `"`nomodify'"' != "" ///
		| `"`omit'"' != "" | `"`varinfo'"' != "" | `"`labinfo'"' != "" | `"`savedo'"' != "") {
		
		di as err "option unassign may not be combined with other options"
		exit 198
	}

	* savedo may not be combined with ...
	if `"`savedo'"' != "" { 
		if `"`nomodify'"' != "" {
			di as err "option savedo() may not be combined with option nomodify"
			exit 198
		}
		if `"`omit'"' != "" {
			di as err "option savedo() may not be combined with option omit"
			exit 198
		}
		if `"`unassign'"' != "" {
			di as err "option savedo() may not be combined with option unassign"
			exit 198
		}
		if `hasvarlist' == 1 {
			di as err "varlist (list of to include variables) outside the options"
			di as err "not allowed in combination with option savedo()"
			exit 198
		}
	}

	*replace only possible with savedo
	if `"`replace'"' != "" & `"`savedo'"' == "" {
		di as err "option replace only possible in combination with option savedo()"
		exit 198
	}

	*********************************************
	if `"`vfieldnames'"' != "" {
		*must contain 1 ";"
		local vfield `"`vfieldnames'"'
		local part = 1  //before 1st ";"

		while `"`vfield'"' != "" {  //get token before and after ; (if present)
			gettoken piece vfield : vfield,parse(";,") //qed(hadquotes)
			if `"`piece'"' == ";" | `"`piece'"' == "," {
				local part = `part' + 1
			}
			else {
				local v`part' `"`piece'"'
			}
		}
		if `part' != 2 {
			di as err "syntax error in option vfieldnames()"
			di as err "must contain 1 semikolon"
			exit 198
		}
		if `"`v1'"' != "" {
			local vvn_var `"`v1'"'
		}
		if `"`v2'"' != "" {
			local vvn_cat `"`v2'"'
		}
	}
	********************************************
	if `"`lfieldnames'"' != "" {
		*must contain 2 ";"
		local lfield `"`lfieldnames'"'
		local part = 1  //before 1st ";"

		while `"`lfield'"' != "" {  //get token before and after ; (if present)
			gettoken piece lfield : lfield,parse(";,") //qed(hadquotes)
			if `"`piece'"' == ";" | `"`piece'"' == "," {
				local part = `part' + 1
			}
			else {
				local l`part' `"`piece'"'
			}
		}
		if `part' != 3 {
			di as err "syntax error in option lfieldnames()"
			di as err "must contain 2 semikolon"
			exit 198
		}
		if `"`l1'"' != "" {
			local lvn_cat `"`l1'"'
		}
		if `"`l2'"' != "" {
			local lvn_nr `"`l2'"'
		}
		if `"`l3'"' != "" {
			local lvn_text `"`l3'"'
		}
	}
	*******************************************
	* if unassign: "unassign" value labels from variables on includelist
	if `"`unassign'"' != "" {
		foreach var of varlist `includelist' {
			capture label values `var'  //without labelname = unassign
		}
		exit 0  // exit program
	}
	********************************************
	* save names of existing value labels of current dataset
	qui label dir
	local existlab `"`r(names)'"'
	
	********************************************
	* save dataset
	preserve

	*******************************************
	*** Load table with varnames and relevant value labels
	capture use `"`varinfo'"',clear
        if _rc ! = 0 {
		di as err `"Error reading file `varinfo'"'
		exit 603
        }

	*names of fields in vn_var and vn_cat
	capture confirm variable `vvn_var', exact
	if _rc != 0 {
		di as err "variable `vvn_var' in varinfo file not found"
		di as err "use option vfieldnames() if variable has a different name"
		exit 111
	}
	
	local tp:type `vvn_var'
	if substr("`tp'",1,3) != "str" {
		di as err "`vvn_var' must be string variable"
		exit 109
	}
	
	capture confirm variable `vvn_cat', exact
	if _rc != 0 {
		di as err "variable `vvn_cat' in varinfo file not found"
		di as err "use option vfieldnames() if variable has a different name"
		exit 111
	}

	local tp:type `vvn_cat'
	if substr("`tp'",1,3) != "str" {
		di as err "`vvn_cat' must be string variable"
		exit 109
	}

	*** save varnames and value label names in local macros
	local k = 1

	qui count
	forval i=1/`r(N)' {
		*colum names are varname and labelname
		if `vvn_var'[`i'] != "" & `vvn_cat'[`i'] != "" {
			local vname`k'=`vvn_var'[`i']
			local lab`k'=`vvn_cat'[`i']
			local k = `k' + 1
		}	
	}
	local totallabs = `k' - 1
	*macro list


	********************************************
	* load datafile with value labels
	capture use `"`labinfo'"',clear
        if _rc ! = 0 {
		di as err `"Error reading file `labinfo'"'
		exit 603
        }

	*names in vn_cat, vn_nr and vn_text
	capture confirm variable `lvn_cat'
	if _rc != 0 {
		di as err "variable `lvn_cat' in labinfo file not found"
		di as err "use option lfieldnames() if variable has a different name"
		exit 111
	}

	local tp:type `lvn_cat'
	if substr("`tp'",1,3) != "str" {
		di as err "`lvn_cat' must be string variable"
		exit 109
	}

	capture confirm variable `lvn_nr'
	if _rc != 0 {
		di as err "variable `lvn_nr' in labinfo file not found"
		di as err "use option lfieldnames() if variable has a different name"
		exit 111
	}

	local tp:type `lvn_nr'
	if `"`tp'"' != "int" & `"`tp'"' != "byte" & `"`tp'"' != "long" {
		di as err "variable `lvn_nr' must be numeric (byte, int or long)"
		exit 109
	}

	capture confirm variable `lvn_text'
	if _rc != 0 {
		di as err "variable `lvn_text' in labinfo file not found"
		di as err "use option lfieldnames() if variable has a different name"
		exit 111
	}

	local tp:type `lvn_text'
	if substr("`tp'",1,3) != "str" {
		di as err "`lvn_text' must be string variable"
		exit 109
	}

	*** if savedo: open (create or erase) do-file for write
	if `"`savedo'"' != "" {
	
		*sort datset by label name
		qui sort `lvn_cat'
	
		tempname WBT

		*open do_file for write
		cap file open `WBT' using `"`savedo'"' , write text `replace' // not append
		
		if _rc ! = 0 {
			qui capture confirm file `"`savedo'"'
			if _rc == 0 & `"`replace'"' == "" {
				di as err "error writing label define commands in do-file"
				di as err "not able to write file `savedo'" 
				di as err "file already exists - use option replace"
				exit 602
			}
			else {
				di as err "error writing label define commands in do-file"
				di as err "not able to write file `savedo'" 
				exit 603
			}
		}

		* write header
		local line "*****************************************"
		file write `WBT' `"`line'"'
		file write `WBT' _newline(1) 
		local line "* do-file for defining and assigning labels"
		file write `WBT' `"`line'"'	
		file write `WBT' _newline(1)
		local line "* built according to information in datafiles:"
		file write `WBT' `"`line'"'	
		file write `WBT' _newline(1)
		local line "* `varinfo'"
		file write `WBT' `"`line'"'	
		file write `WBT' _newline(1)
		local line "* and"
		file write `WBT' `"`line'"'	
		file write `WBT' _newline(1)
		local line "* `labinfo'"
		file write `WBT' `"`line'"'	
		file write `WBT' _newline(1)
		local line "* do-file created date: `c(current_date)'  time: `c(current_time)'"
		file write `WBT' `"`line'"'	
		file write `WBT' _newline(1)
		local line "*****************************************"
		file write `WBT' `"`line'"'
		file write `WBT' _newline(2) 
		local line "*define value labels"
		file write `WBT' `"`line'"'
		file write `WBT' _newline(2) 
	}

	*** define value labels
	local lbname_old = ""
	local labdroplist ""  //labels have to be dropped before modified (evt. no more used values)

	qui count
	forval i=1/`r(N)' {

		if `lvn_cat'[`i'] != "" & `lvn_nr'[`i'] != . {
			local lbname=`lvn_cat'[`i']
			local lbnr=`lvn_nr'[`i']
			local lbtext=`lvn_text'[`i']

			if `"`lbname'"' != `"`lbname_old'"' {  // create new dodef flag only if different valuelabel
				local dodef = 1
				* if option nomodify: is value label already defined?
				if `"`nomodify'"' != "" {
					foreach l of local existlab {
						if `"`l'"' == `"`lbname'"' {
							local dodef = 0  //already defined
							continue, break
						}
					}
				}
				* is label on "omit-list"?
				if `"`omit'"' != "" & `dodef' == 1 {
					foreach o of local omit {
						if `"`o'"' == `"`lbname'"' {
							local dodef = 0  // omit
							continue, break
						}
					}
				}
				* is label to be assigned to variable on includelist?
				if `dodef' == 1 & `"`savedo'"' == "" {
					local dodef = 0
					local k = 1
					while `dodef' != 1 &  `k' <= `totallabs' {  //search until found or end of list
						*search for valuelabel in locals and look for relevent varname
						if `"`lab`k''"' == `"`lbname'"' {
							foreach v of local includelist {
								if `"`v'"' == `"`vname`k''"' local dodef = 1  //relevant var found
							}
						}
						local k = `k' + 1
					}
				}
				if `"`savedo'"' != "" & `dodef' == 1 {
					*if not already on droplist
					local alr = 0
					foreach item of local labdroplist {
						if `"`item'"' == `"`lbname'"' local alr = 1
					}
					if `alr' != 1 {
						local line "capture label drop `lbname'"
						file write `WBT' `"`line'"'	
						file write `WBT' _newline(1)
					}
				}
				if `dodef' == 1 local labdroplist `"`labdroplist' `lbname'"'

			}
			*** define label, but only if dodef flag
			if `dodef' == 1 {
				label define `lbname' `lbnr' `"`lbtext'"', modify
				if `"`savedo'"' != "" {
					local line `"label define `lbname' `lbnr' `"`lbtext'"', modify"'
					file write `WBT' `"`line'"'	
					file write `WBT' _newline(1)
				}
			}

		}
		local lbname_old `"`lbname'"'
	}

	*** save value labels in temporary file (do-file format)
	* if not savedo
	if `"`savedo'"' == "" {
		tempfile labels
		qui label save using "`labels'",replace
	}
	else { // label defines already written into do-file
		// now label values commands

		file write `WBT' _newline(2)
		local line "*****************************************"
		file write `WBT' `"`line'"'
		file write `WBT' _newline(1) 
		local line "* assign value labels to variables"
		file write `WBT' `"`line'"'	
		file write `WBT' _newline(2)

		forval x=1/`totallabs' {
			local line `"capture label values `vname`x'' `lab`x''"'
			file write `WBT' `"`line'"'	
			file write `WBT' _newline(1)
		}

		file close `WBT'
	
		disp ""
		disp "{col 1}{txt} no changes in current STATA dataset"
		disp "{col 1}{txt} label drop, label define and label values commands only saved in do-file"
		disp "{col 1}{txt} file name: {res}`savedo'{txt}"
		disp ""
	}

	*************************************************
	*** restore Stata File with required data
	restore

	*************************************************
	if `"`savedo'"' != "" exit 0
	
	*************************************************
	*** run do file to rebuild saved value labels
	*but before that, drop relevant labels
	
	foreach lb of local labdroplist {
		capture label drop `lb'
	}
	qui do `labels'
	*********
	*** assign value labels to present variables (only specified variables)
	foreach var of varlist `includelist' {
		forval x=1/`totallabs' {
			local doval = 1
			if "`var'" == "`vname`x''" & `"`lab`x''"' != "" {
				* is label on "omit-list"?
				if `"`omit'"' != "" {
					foreach o of local omit {
						if `"`o'"' == `"`lab`x''"' {
							local doval = 0  // omit
							continue, break
						}
					}
				}
				if `doval' == 1 label values `var' `lab`x''
			}
		}
	}
end

exit
