*!1.0.3 22Jan2008
capture program drop xvarlabel

program define xvarlabel

 	syntax [varlist], [VARinfo(str)  erase]  /*
     	*/	 [VFIELDnames(str)] [NOMODify] [MAXLENgth(str)]/*
     	*/ 	 [omit(varlist)] [savedo(str)] [replace]

	********************************************
	local includelist `"`varlist'"'

	* if varlist not specified: _all will be automatically assumed (by STATA)
	
	* check if varlist is not specified
	gettoken piece : 0,parse(",")
	if `"`piece'"' == "" | `"`piece'"' == "," local hasvarlist = 0
	else local hasvarlist = 1
	local piece ""

	*expected names: varname file: "var" = varname, "text" = var label
	local vvn_var "var"
	local vvn_cat "text"

	********************************************
	*syntax / option checks
	
	* remark: omit(varlist) means that varlabel(s) will be deleted
	
	* remark: savedo(do-filename) means that no changes will be performed in
	* STATA working dataset (no label var). Only
	* writing label define commands in specified do-file.
	
	*option varinfo compulsory unless option erase speified
	if `"`varinfo'"' == "" & `"`erase'"' == "" {
		di as err "option varinfo() required"
		exit 198
	}

	* erase may not be combined with other options
	if `"`erase'"' != "" & ///
		(`"`vfieldnames'"' != ""  | `"`nomodify'"' != "" | `"`maxlength'"' != "" ///
		| `"`omit'"' != "" | `"`varinfo'"' != ""  | `"`savedo'"' != "") {
		
		di as err "option erase may not be combined with other options"
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
		if `"`erase'"' != "" {
			di as err "option savedo() may not be combined with option erase"
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
	* maxlength >0 <81
	if `"`maxlength'"' != "" {
		local maxl = real("`maxlength'")
		if `maxl' < 1 | `maxl' > 80 | `maxl' == . {
			di as err "value out of range in option maxlength()"
			di as err "must be between 1 and 80"
			exit 198
		}
	}
	else local maxl = 80  //up to 80 chars are allowed for variable labels
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
	*******************************************
	* if erase: "erase" variable labels from variables on includelist
	if `"`erase'"' != "" {
		foreach var of varlist `includelist' {
			capture label var `var' ""
		}
		exit 0  // exit program
	}
	
	********************************************
	* save dataset
	preserve

	*******************************************
	*** Load table with varnames and variable labels
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

	*** save varnames and variable labels in local macros
	local k = 1

	qui count
	forval i=1/`r(N)' {
		*colum names are varname and labelname
		if `vvn_var'[`i'] != ""  {
			local vname`k'=`vvn_var'[`i']
			local lab`k'=`vvn_cat'[`i']
			local k = `k' + 1
		}	
	}
	local totallabs = `k' - 1
	*macro list


	*** if savedo: open (create or erase) do-file for write
	if `"`savedo'"' != "" {
	

		*qui sort `vvn_var'
	
		tempname WBT

		*open do_file for write
		cap file open `WBT' using `"`savedo'"' , write text `replace' // not append
		
		if _rc ! = 0 {
			qui capture confirm file `"`savedo'"'
			if _rc == 0 & `"`replace'"' == "" {
				di as err "error writing label variable commands in do-file"
				di as err "not able to write file `savedo'" 
				di as err "file already exists - use option replace"
				exit 602
			}
			else {
				di as err "error writing label variable commands in do-file"
				di as err "not able to write file `savedo'" 
				exit 603
			}
		}

		* write header
		local line "*****************************************"
		file write `WBT' `"`line'"'
		file write `WBT' _newline(1) 
		local line "* do-file for defining variable labels"
		file write `WBT' `"`line'"'	
		file write `WBT' _newline(1)
		local line "* built according to information in datafile:"
		file write `WBT' `"`line'"'	
		file write `WBT' _newline(1)
		local line "* `varinfo'"
		file write `WBT' `"`line'"'	
		file write `WBT' _newline(1)
		local line "* do-file created date: `c(current_date)'  time: `c(current_time)'"
		file write `WBT' `"`line'"'	
		file write `WBT' _newline(1)
		local line "*****************************************"
		file write `WBT' `"`line'"'
		file write `WBT' _newline(2) 
	}


	if `"`savedo'"' != "" {

		forval x=1/`totallabs' {
			local lb=substr(`"`lab`x''"',1,`maxl')
			local line `"capture label var `vname`x'' `"`lb'"'"'
			*local line `"capture label var `vname`x'' `"`lab`x''"'"'
			file write `WBT' `"`line'"'	
			file write `WBT' _newline(1)
		}

		file close `WBT'
	
		disp ""
		disp "{col 1}{txt} no changes in current STATA dataset"
		disp "{col 1}{txt} label variable commands only saved in do-file"
		disp "{col 1}{txt} file name: {res}`savedo'{txt}"
		disp ""
	}

	*************************************************
	*** restore Stata File with required data
	restore

	*************************************************
	if `"`savedo'"' != "" exit 0
	
	*************************************************
	*** define variable labels
	
	foreach var of varlist `includelist' {
		local olb:variable label `var'
		if `"`nomodify'"' == "" | `"`olb'"' == "" {
			forval x=1/`totallabs' {
				local doval = 1
				if "`var'" == "`vname`x''" {
					* is variable on "omit-list"?
					if `"`omit'"' != "" {
						foreach o of local omit {
							if `"`o'"' == `"`vname`x''"' {
								local doval = 0  // omit
								continue, break
							}
						}
					}
					local lb=substr(`"`lab`x''"',1,`maxl')

					if `doval' == 1 capture label variable `var' `"`lb'"'
				}
			}
		}
	}
end

exit
