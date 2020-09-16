******************************************************************************************************************************************************************************************************************************************
	
	
	***!!! TABLES SUMMARISING VARIABLES - EACH ROW SHOULD IDEALLY CONTAIN THE SAME NUMBERS !!!***

*!	PROGRAM SYNTAX	!*
	
program define tdescribe
version 11.2
syntax varlist (numeric) [if] [in] [, SUBGroups(str) PARTition(str asis) drop(str) ///	main options
NONMetric(str) CATEgorical(str) CONTinuous(str) LOWErbin(str) ///						variable classification options
VALUecounts(str asis) DECImals(str) PUNCtuation(str asis) HEADers(str asis) TABLename(str) ///	table strucrure and appearance options
CENTiles(str asis) CCENtiles(str asis) CONFidence(str) ///
chi2(str) FISHer(str) ANOVa(str) KWALlis(str)] //										centile, confidence interval,and p-vale options
marksample genfil, novarlist //															the tempvar `genfil' is replaced by 1 according to the content of `if'
loc order "`varlist'"
dis " "

foreach a in subgroups drop nonmetric categorical continuous lowerbin decimals tablename confidence chi2 fisher anova kwallis{
	loc `a': subinstr loc `a' `"""' `""', all
	loc `a': subinstr loc `a' "  " " ", all count(loc b)
	while `b'>0{ //																		itrim does only work for short strings
		loc `a': subinstr loc `a' "  " " ", count(loc b)
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h1"
*!	CHECK THE SUBGROUPS OPTION AND GENERATE A TEMPORARY SUBGROUP DEFINING VARIABLE  !*

loc subgroups: subinstr loc subgroups " " "", all //									tokenise subgroups to one token per subgroup
loc subgroups: subinstr loc subgroups ";" " ", all
loc grouplist "1==1 `subgroups'"

tempvar groupdefiner
qui gen `groupdefiner' = .
loc a = 1

foreach b of local subgroups{

	loc a = `a' + 1
	
	capt count if (`b') & `genfil' & 11111111==11111111
	if _rc!=0 loc errorcolumns "`errorcolumns',`a'" //									THE ERROR MESSAGES OF THE SUBGROUPS OPTION ARE GENERATED INTO THE RESPECTIVE COLUMNS
	else{
		qui replace `groupdefiner' = -99 if `groupdefiner'<. & (`b') & `genfil' //		if a value was entered before, it means that the groups are not independent
		qui replace `groupdefiner' = `a' if `groupdefiner'==. & (`b') & `genfil' //		1 if the first condition is fulfilled, 2 f the first condition is fulfilled, etc.
	}
}
if "`errorcolumns'"=="" loc errorcolumns ",0"
*
																						if "$rafaeldebug"=="debug" dis as result "h2"
*!	CHECK THE PARTITION OPTION	!*

loc b = 1
loc c = 0
loc d: word count `varlist'

foreach a of local partition{

	if `b'!=3{
		capt conf integer n `a' //														verify the integer integer * pattern; errors stop the command
		if _rc==7{
			dis in red "'`a'' found where integer expected;" as result " 'partition' option ignored" _newline
			loc partition `""'
			continue, break
		}
		if inrange(`a',1,`d')==0{
			dis in red "'`a'' out of varlist range;" as result " 'partition' option ignored" _newline //			verify that the first and last variable to be partitioned exists
			loc partition `""'
			continue, break
		}
	}

	if `b'==1 loc e "`e' `a'"
	if `b'==2 loc e "`e',`a'"
	if `b'==3 loc b = 1
	else loc b = `b' + 1
}

loc a: word count `partition'

if floor(`a'/3)!=`a'/3{
	dis in red "the information specified in the 'partition' option is not complete;" as result " 'partition' option ignored" _newline
	loc partition ""
}

if `"`partition'"'!=`""'{
	foreach a of local e{
		if min(`a')<`b'{
			dis in red "partitions must be entered in the order they appear in the table;" as result " 'partition' option ignored" _newline
			loc partition `""'
			continue, break
		}
		loc b = min(`a')
	}
	foreach a of local e{
		foreach b of local e{ //															allowed are: aabb, bbaa, abba, and baab (of the 4!/(2!*2!)= 6 possible combinations)
			
			if (min(`a')<min(`b') & min(`b')<max(`a') & max(`a')<max(`b')) | (min(`b')<min(`a') & min(`a')<max(`b') & max(`b')<max(`a')){
				dis in red "partitions overlapp;" as result " 'partition' option ignored" _newline
				loc partition `""'
				continue, break
			
			}
		}
		if `"`partition'"'==`""' continue, break
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h3"
*!	CHECK DROP OPTION	!*

loc a `""binlab","binval","emptyc","tv","vv","cn","pc","mm","sd""'
loc b `""mq","iq","deconf","depval","di","ci","po","pi""'
loc c "`drop'"

foreach d of local c{

	if inlist("`d'",`a')==0 & inlist("`d'",`b')==0{ //									filter out unallowed elements of the drop option
		loc drop: subinstr loc drop "`d'" "", word all
		dis in red "'`d'' is no specification of the 'drop' option;" as result " specification '`d'' ignored" _newline
	}

	loc drop: subinstr loc drop "`d'" "`d'", word all count(loc e)
	while `e'>1{ //																		identify duplicates of the drop option
		loc drop: subinstr loc drop "`d'" "", word
		loc e = `e' - 1
		if `e'==1 dis in red "'`d'' has been specified more than once in the 'drop' option;" as result " duplicates ignored" _newline
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h4"
*!	CHECK THE NONMETRIC, CATEGORICAL, AND CONTINUOUS OPTIONS	!*

loc a "`varlist'"

foreach b in nonmetric categorical continuous{ //										filter out all entries which are not in `varlist'
	
	loc c "``b''"
	
	foreach d of local c{
																						if "$rafaeldebug"=="debug" dis as result "h4.1"
		loc a: subinstr loc a "`d'" "`d'", word count(loc e)
		if `e'==0{
			dis in red "'`d'' (specified in the '`b'' option) is no element of varlist;" as result " specification '`d'' ignored" _newline
			loc `b': subinstr loc `b' "`d'" "", word all
			continue
		}
																						if "$rafaeldebug"=="debug" dis as result "h4.2"	
		loc `b': subinstr loc `b' "`d'" "`d'", word all count(loc e)
		while `e'>1{ //																	identify duplicates of the continuous, nonmetric, and categorical options
			loc `b': subinstr loc `b' "`d'" "", word
			loc e = `e' - 1
			if `e'==1 dis in red "'`d'' has been specified more than once in the '`b'' option;" as result " duplicates ignored" _newline
		}
		
		if "`b'"=="categorical"{
			capture tab `d'
			if _rc==134 | (r(r)>99 & r(r)!=.){
				dis in red "'`d'' has too many values to be categorical;" as result " specification '`d'' ignored" _newline
				loc categorical: subinstr loc categorical "`d'" "", word
				continue
			}
			if `d'!=int(`d'){
				tempvar e f
				qui tostring `d', force gen(`f')
				qui replace `f' = "" if substr(`f',1,1)=="."
				encode `f', gen(`e')
				loc categorical: subinstr loc categorical "`d'" "`e'", word
				loc order: subinstr loc order "`d'" "`e'", word
				loc varlist: subinstr loc varlist "`d'" "`e'", word
			}
		}
	}
}
																						if "$rafaeldebug"=="debug" dis as result "h4.5"	
if "`continuous'"!=""{

	foreach a of local nonmetric{
		loc continuous: subinstr loc continuous "`a'" "`a'", word count(loc b)
		if `b'!=0 dis in red "'`a'' specified in options 'nonmetric' and 'continuous';" as result " 'nonmetric' favoured" _newline
		loc continuous: subinstr loc continuous "`a'" "", word
	}
	
	foreach a of local categorical{
		loc continuous: subinstr loc continuous "`a'" "`a'", word count(loc b)
		if `b'!=0{
			dis in red "'`a'' specified in options 'categorical' and 'continuous';" as result " specification '`a'' ignored" _newline
			loc categorical: subinstr loc categorical "`a'" "", word
			loc continuous: subinstr loc continuous "`a'" "", word
		}
	}
}

if "`nonmetric'"!=""{
	
	foreach a of local categorical{
		loc nonmetric: subinstr loc nonmetric "`a'" "`a'", word count(loc b)
		if `b'!=0{
			dis in red "'`a'' specified in options 'nonmetric' and 'categorical';" as result " specification '`a'' ignored" _newline
			loc categorical: subinstr loc categorical "`a'" "", word
			loc nonmetric: subinstr loc nonmetric "`a'" "", word
		}
	}
}	
*

*!	CHECK THE LOWERBIN OPTION !*

// MUST BE DONE AFTER CLASSIFYING VARIABLES INTO BINARY AND ELSE
*
																						if "$rafaeldebug"=="debug" dis as result "h5"
*!	CHECK THE VALUECOUNTS OPTION	!*

loc valuecounts: subinstr loc valuecounts `"totdenom"' "", word all count(loc a)
if `a'>0{ //																			ALLOW FOR THE valuecounts(totdenom) OPTION
	loc not_missing "<=.z" //															if the valuecounts(totdenom) option is specified, denominators of binary and categorical variables are the total value counts (i.e. the sum of valid and missing values)
	loc g "(with exception of 'totdenom')"
}
else{
	loc not_missing "<." //																by default, denominators of binary and categorical variables are the counts of valid values
	loc g ""
}
while `a'>1{ //																			identify duplicates of the drop option
	loc valuecounts: subinstr loc valuecounts "totdenom" "", word
	loc a = `a' - 1
	if `a'==1 dis in red "'totdenom' has been specified more than once in the 'valuecounts' option;" as result " duplicates ignored" _newline
}

loc a: word count `valuecounts' //														in case of wrong entries and duplicates there could be more than 2 words
if floor(`a'/2)!=`a'/2{
	dis in red "the information specified in the 'valuecounts' option is not complete;" as result " 'valuecounts' option ignored" _newline
	loc valuecounts ""
}
else{
	forvalues c = 2 (2) `a'{

		loc b = `c' - 1
		
		loc b: word `b' of `valuecounts'

		if "`b'"!="total"{ //																filter out unallowed elements of the valuecounts option
			dis in red "'`b'' is no specification of the 'valuecounts' option; " as result "'valuecounts' option `g' ignored" _newline
			loc valuecounts `""'
			continue, break
		}
		loc valuecounts: subinstr loc valuecounts "`b'" "`b'", word all count(loc c)
		if `c'>1{
			dis in red "'`b'' has been specified more than once; " as result "'valuecounts' option `g' ignored" _newline
			loc valuecounts `""'
			continue, break
		}
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h6"
*!	CHECK THE DECIMALS OPTION	!*

loc a `""c", "p", "q""'
loc b "`decimals'"

foreach c of local b{
	
	loc d = substr("`c'",1,1) //														the first part contains the designation (c for continuous, p for percents, q for quartiles)
	loc e = substr("`c'",2,.) //														the second part contains the number of decimals
	
	capture confirm number `e'
	
	if inlist("`d'",`a')==0 | _rc==7{ //												filter out unallowed elements of the decimals option
		dis in red "'`c'' is no specification  the 'decimals' option;" as result " '`c'' ignored" _newline
		loc decimals: subinstr loc decimals "`c'" "", word all
		continue
	}
																						if "$rafaeldebug"=="debug" dis as result "h6.5"
	if inrange(`e',0,5)==0{
		dis in red "'`e'' outside of allowed range of the 'decimals' option;" as result " '`c'' ignored" _newline
		loc decimals: subinstr loc decimals "`c'" "", word all
	}

	loc decimals: subinstr loc decimals "`c'" "`c'", word all count(loc f)
	while `f'>1{ //																		identify duplicates of the drop option
		loc decimals: subinstr loc decimals "`c'" "", word
		loc f = `f' - 1
		if `f'==1 dis in red "`c' has been specified more than once in the 'decimals' option;" as result " duplicates ignored" _newline
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h7"
*!	CHECK THE PUNCTUATION OPTION	!*

loc a `""tv","vv","cn","pc","mm","sd","mq","lq","uq""'
loc b `""pdi","pll","pul","mdi","mll","mul""'
loc c: word count `punctuation' //														in case of wrong entries and duplicates there could be more than 30 words

if floor(`c'/2)!=`c'/2{
	dis in red "the information specified in the 'punctuation' option is not complete;" as result " 'punctuation' option ignored" _newline
	loc punctuation ""
}
else{
	forvalues e = 2 (2) `c'{

		loc d = `e' - 1
		
		loc d: word `d' of `punctuation'

		if (inlist(substr("`d'",1,2),`a')==0 | inlist(substr("`d'",3,3),"1","2")==0) & (inlist(substr("`d'",1,3),`b')==0 | inlist(substr("`d'",4,4),"1","2")==0){ //		filter out unallowed elements of the partition option
			dis in red "'`d'' is no specification of the 'punctuation' option; " as result "'punctuation' option ignored" _newline
			loc punctuation `""'
			continue, break
		}
		loc punctuation: subinstr loc punctuation "`d'" "`d'", word all count(loc e) //		identify duplicates of the punctuation option
		if `e'>1{
			dis in red "'`d'' has been specified more than once; " as result "'punctuation' option ignored" _newline
			loc headers `""'
			continue, break
		}
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h8"
*!	CHECK THE HEADERS OPTION	!*

loc b = 1
loc c: word count `subgroups'
loc c = `c' + 1

foreach a of local headers{

	if `b'==1{
		capture confirm integer number `a' //											verify the integer integer * pattern; errors stop the command
		if _rc==7{
			dis in red "'`a'' found where integer expected; " as result "'headers' option ignored" _newline
			loc headers `""'
			continue, break
		}
		if inrange(`a',1,`c')==0{ //													verify that the option does not label non-existing groups
			dis in red "'`a'' out of group range; " as result "'headers' option ignored" _newline
			loc headers `""'
			continue, break
		}
		loc headers: subinstr loc headers "`a'" "`a'", word all count(loc d)
		if `d'>1{
			dis in red "group `a' has been labeled more than once with the 'headers' option; " as result "'headers' option ignored" _newline
			loc headers `""'
			continue, break
		}
	}
	
	if `b'==2 loc b = 1
	else loc b = 2
}
loc a: word count `headers'
if floor(`a'/2)!=`a'/2{
	dis in red "the information specified in option headers is not complete; " as result "option 'headers' ignored" _newline
	loc headers `""'
}
*
																						if "$rafaeldebug"=="debug" dis as result "h9"
*!	CHECK THE TABLENAME OPTION	!*

capture confirm name `tablename'
if _rc==7 | length("`tablename'")>24{ //												names can have a max of 32 characters, but `tablename' can have only 24, to allow derrivates
	dis in red "'`tablename'' invalid name stem; " as result "'tablename' option ignored" _newline
	loc tablename ""
}
if wordcount("`tablename'")>1{ //														verify that only one tablename was specified
	loc tablename: word 1 of `tablename'
	dis in red "more than one tablename specified; " as result "superfluous tablenames ignored" _newline
}
*	
																						if "$rafaeldebug"=="debug" dis as result "h10"
*!	CHECK THE CENTILES AND CCENTILES OPTIONS !*

foreach a in centiles ccentiles{

	loc b: length loc `a'

	if `b'>60{
		dis in red "`b' characters in option '`a'' where 60 allowed; " as result "option '`a'' ignored" _newline
		loc centiles ""
	}
	
	loc `a': subinstr loc `a' `"_centiles"' `"_centiles"', word count(loc b)
	if "`a'"=="ccentiles" & `b'>0 & `"``a''"'!=`"_centiles"' dis in red "the 'ccentiles' option allows the '_centiles' specification instead of other content, but not in addition to it; " ///
	as result "additional content ignored" _newline
	if "`a'"=="ccentiles" & `b'>0 loc ccentiles `"`centiles'"'

	loc c = 1
	
	foreach b of local `a'{

		if `c'==1{
			capture confirm integer number `b' //										verify the integer integer * pattern; errors stop the command
			if _rc==7{
				dis in red "'`b'' found where integer expected; " as result "'`a'' option ignored" _newline
				loc `a' `""'
				continue, break
			}
			if inrange(`b',0,100)==0{
				dis in red "'`b'' out of range; " as result "'`a'' option ignored" _newline //	verify that the first and last variable to be partitioned exists
				loc `a' `""'
				continue, break
			}
		}
		if `c'==3 loc c = 1
		else loc c = `c' + 1
	}
	loc b: word count ``a''
	if floor(`b'/3)!=`b'/3{
		dis in red "the information specified in the '`a'' option is not complete; " as result "'`a'' option ignored" _newline
		loc `a' `""'
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h11"
*!	CHECK THE CONFIDENCE OPTION !*

// THE MESSAGES OF THE CONFIDENCE OPTION ARE GENERATED INTO THE RESPECTIVE COLUMNS
*

*!	CHECK THE CHI2, FISHER, ANOVA, AND KWALLIS OPTIONS !*

// MUST BE DONE AFTER COMPLETE CLASSIFICATION INTO BINARY, CATEGORICAL, CONTINUOUS, AND NONMETRIC
*

*!	ALLOW FOR THE SUBGROUPS OPTION !*

// IS PART OF THE GROUP AND SUBGROUP COLUMN GENERATION
*
																						if "$rafaeldebug"=="debug" dis as result "h12"
*!	ALLOW FOR THE PARTITION OPTION !*

foreach a of local varlist{ // it is essential that all variables are labelled for the partition option, but non-labelled variables are reported later
	loc b: var lab `a'
	if "`b'"==""{
		lab var `a' `a'
		loc novarlab "`novarlab' `a'"
	}
}

loc d: word count `partition'

forvalues c = 3(3)`d'{ //																create macros numbered according to where they belong and containing the corresponding partition name
	
	loc a = `c' - 2
	loc b = `c' - 1
	loc j = `c' - 3
	
	loc a: word `a' of `partition'
	loc b: word `b' of `partition'
	loc c: word `c' of `partition'
	
	loc e = min(`a',`b')
	loc f = max(`a',`b')
	
	loc switch1 = 1
	
	forvalues i = 3(3)`j'{
	
		loc g = `i' - 2
		loc h = `i' - 1
	
		loc g: word `g' of `partition'
		loc h: word `h' of `partition'
	
		if min(`g',`h')<=`e' & `f'<=max(`g',`h'){
			loc c " `c'"
			loc switch1 = `switch1' + 1
		}
	}
		
	loc a = 1
	loc switch2 = 0
																						if "$rafaeldebug"=="debug" dis as result "h12.5"
	foreach b of local varlist{

		if "`a'"=="`e'"{ //																identify the first partitioned variable of each partition group
				
			tempvar g
			qui gen `g' = .
			lab var `g' "`c'" //														label the partition variable with the partition name
			loc order: subinstr loc order "`b'" "`g' `b'", word //						order the partition variable before the first partitioned variable
			
			loc switch2 = `switch2' + 1 //												turn the switch on
		}
																						if "$rafaeldebug"=="debug" dis as result "h12.7"				
		if `switch2'!=0{ //																0 format is not allowed
			loc c: var label `b' //														indent the variable label of the partitioned variables
			loc h: dis %`switch2's ""
			lab var `b' "`h'`c'"
			loc indented "`indented' `b'" //											remember indented variables to adapt their categories and to deindent them at the end
		}
			
		if "`a'"=="`f'" loc switch2 = `switch2' - 1 //									after having indented the last partitioned variable turn the switch of
			
		loc a = `a' + 1
	}
}
*

*!	ALLOW FOR THE DROP OPTION !*

loc initiation = 5 //																	indicates where the variable dependent statistics begin

foreach a of local drop{

	if "`a'"=="binlab"		loc bindrop "on" //											the binary category displayed in the table is not indicated
	if "`a'"=="binval"		loc numdrop "on" //											the binary category number displayed in the table is not indicated
	if "`a'"=="emptyc"		loc emptydrop "on" //										categories defined in the label but not found in the data are not indicated
	
	if "`a'"=="tv"			loc initiation = 3 //										the groupcounts are not displayed
	if "`a'"=="vv"			loc vvdrop "on" //											the varbygroupcounts are not displayed
	if "`a'"=="cn"			loc cndrop "on" //											the counts of binary and categorical variables are not indicated
	if "`a'"=="pc"			loc pcdrop "on" //											the percents of binary and categorical variables are not indicated
	if "`a'"=="mm"			loc mmdrop "on" //											the means of continuous variables are not indicated
	if "`a'"=="sd"			loc sddrop "on" //											the SD of continuous variables are not indicated
	if "`a'"=="mq"			loc mqdrop "on" //											the medians of nonmetric variables are not indicated
	if "`a'"=="iq"			loc iqdrop "on" //											the interquartile ranges of nonmetric variables are not indicated
		
	if "`a'"=="deconf"		loc CIdrop "on" //											the default CI is not displayed
	if "`a'"=="depval"		loc Pdrop "on" //											the pvalue is not displayed
	if "`a'"=="di"			loc didrop "on" //											the difference in default and extended CI is not displayed
	if "`a'"=="ci"			loc cidrop "on" //											the confidence interval in default and extended CI is not displayed
	if "`a'"=="po"			loc podrop "on" //											the individual pvalues of categorical variables are not displayed
	if "`a'"=="pi"			loc pidrop "on" //											the individual pvalues of categorical variables are not displayed
}
*
																						if "$rafaeldebug"=="debug" dis as result "h12"
*!	ALLOW FOR THE CATEGORICAL, CONTINUOUS, AND NONMETRIC OPTIONS AND CLASSIFY IN BINARY VARIABLES AND OTHER VARIABLES WHICH WERE NOT SPECIFIED	!*

loc classify "`varlist'" //																THERE IS NO OPTION WHICH WOULD ALLOW TO MAKE NON-BINARY VARIABLES BINARY; WRONG ENTRIES ARE DISPLAYED

foreach c in `continuous' `nonmetric' `categorical'{ //														"forced" variables need no classification
	loc classify: subinstr loc classify "`c'" "", word all
}

foreach c of local classify{
	
	loc catcond "" //																	set conditions to "", otherwise they will still be active for the next variable (macro drop does not work with local); od this at the beginning, because later there is a "continue"
	loc conda ""
	loc condb ""
	loc condc ""
	
	capture tab `c'
	if _rc==134 | r(r)>99 | `c'!=int(`c'){
		loc continuous "`continuous' `c'" //											if there are so many categories that the variable cannot be tabulated, the variable is continuous
		loc catcond "no"
		continue
	}
	loc cats = r(r)
	loc vallab: value label `c'
	if `cats'>9 & "`vallab'"==""{
		loc continuous "`continuous' `c'" //											if there are more than nine categories and no value labels, the variable is continuous
		loc catcond "no" //																i.e. continuous variables have no value labels; if the values are labelled, the variable must be categorical
	}
	qui su `c'
	loc sumin = r(min)
	loc sumax = r(max)
	if (`cats'==2 & `sumin'==0 & `sumax'==1) | ///										if two value categories, 0/1 coded OR
	(`cats'==2 & `sumin'==1 & `sumax'==2) | ///											two value categories, 0/2 coded OR
	(`cats'==1 & inlist(`sumin',0,1,2)) loc conda "y" //								one value category, 0, 1, or 2 coded, condition a for binary fulfilled
	if "`vallab'"!="" capt lab list `vallab' //											the problem will be solved during the label checks: dis in red "the value label of variable '`c'' ('`vallab'') has never been defined;" as result " '`vallab'' ignored"
	if _rc==111{
		loc vallab ""
	}
	else{
		if (r(k)==2 & r(min)==0 & r(max)==1) | ///										if two label categories, 0/1 coded OR
		(r(k)==2 & r(min)==1 & r(max)==2) | ///											two label categories, 0/2 coded OR
		(r(k)==1 & inlist(r(min),0,1,2)) loc condb "y" //								one label category, 0, 1, or 2 coded, condition b for binary fulfilled
		if (`cats'==1 & r(k)==1) | ///													if two value and two label categories OR
		(`cats'==2 & r(k)==1 & inlist(r(min),`sumin',`sumax')) | ///					one value within two label categories OR
		(r(k)==2 & `cats'==1 & inlist(`sumin',r(min),r(max))) | ///						one label within two value categories OR
		(r(k)==2 & `cats'==2 & `sumin'==r(min) & `sumax'==r(max)) ///					two identical value and label categories,
		loc condc "y" //																condition c for binary fulfilled
		if "`conda'"=="y" & "`condb'"=="y" & "`condc'"=="y"{ //							if all three conditions are fulfilled, the variable is binary
			loc binary "`binary' `c'"
			loc catcond "no"
		}
	}
	if "`vallab'"=="" & "`conda'"=="y"{ //												if all condition a is fulfilled and there is no value label, the variable is binary
		loc binary "`binary' `c'"
		loc catcond "no"
	}
	if "`catcond'"!="no" loc categorical "`categorical' `c'" //							if the variable is neither continuous nor binary, it is categorical
}
*
																						if "$rafaeldebug"=="debug" dis as result "h13"
*!	CHECK THE LOWERBIN OPTION (MUST BE PERFORMED AFTER THE CLASSIFICATION)	!*

loc a "`lowerbin'"

foreach b of local a{
		
	loc binary: subinstr loc binary "`b'" "`b'", word count(loc c)
	if `c'==0{
		dis in red "the 'lowerbin' option is only allowed for binary variables; " as result "specification '`b'' ignored" _newline
		loc lowerbin: subinstr loc lowerbin "`b'" "", word all
	}
	loc lowerbin: subinstr loc lowerbin "`b'" "`b'", word all count(loc c)
	while `c'>1{ //																		identify duplicates of the chi and fisher options
		loc lowerbin: subinstr loc lowerbin "`b'" "", word
		loc c = `c' - 1
		if `c'==1 dis in red "'`b'' has been specified more than once in option 'lowerbin'; " as result "duplicates ignored" _newline
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h14"
*!	CHECK THE CHI2, FISHER, ANOVA, AND KWALLIS OPTIONS (MUST BE PERFORMED AFTER THE CLASSIFICATION)	!*

foreach a in chi2 fisher{

	loc b "``a''"
	
	foreach c of local b{
		
		loc binary: subinstr loc binary "`c'" "`c'", word count(loc d)
		if `d'==0 loc categorical: subinstr loc categorical "`c'" "`c'", word count(loc d)
		if `d'==0{
			dis in red "the '`a'' option is only allowed for binary and categorical variables; " as result "specification '`c'' ignored" _newline
			loc `a': subinstr loc `a' "`c'" "", word all
		}
		loc `a': subinstr loc `a' "`c'" "`c'", word all count(loc d)
		while `d'>1{ //																		identify duplicates of the chi and fisher options
			loc `a': subinstr loc `a' "`c'" "", word
			loc d = `d' - 1
			if `d'==1 dis in red "'`c'' has been specified more than once in the '`a'' option; " as result "duplicates ignored" _newline
		}
	}
}
if "`fisher'"!="" dis as text "	... testing memory limits for the 'fisher' option ..." as result "" _newline
qui su `groupdefiner'
if r(min)!=-99{
	foreach a of local fisher{
		capt tab `a' `groupdefiner' if `genfil', exact
		if _rc==910 | _rc==1400{
			dis in red "memory limits exceeded; " as result "'fisher' option ignored" _newline
			loc fisher ""
			continue, break
		}
	}
}
foreach a of local fisher{
	loc chi2: subinstr loc chi2 "`a'" "`a'", word count(loc b)
	if `b'!=0{
		dis in red "'`a'' specified in options 'fisher' and 'chi2';" as result " specification '`a'' ignored" _newline
		loc fisher: subinstr loc fisher "`a'" "", word
		loc chi2: subinstr loc chi2 "`a'" "", word
	}
}

loc a "`anova'"
	
foreach b of local a{
		
	loc nonmetric: subinstr loc nonmetric "`b'" "`b'", word count(loc c)
	if `c'==0{
		dis in red "the 'anova' option is only allowed for variables specified in the 'nonmetric' option; " as result "specification '`b'' ignored" _newline
		loc anova: subinstr loc anova "`b'" "", word all
	}
	loc anova: subinstr loc anova "`b'" "`b'", word all count(loc c)
	while `c'>1{ //																		identify duplicates of the chi and fisher options
		loc anova: subinstr loc anova "`b'" "", word
		loc c = `c' - 1
		if `c'==1 dis in red "'`b'' has been specified more than once in the 'anova' option; " as result "duplicates ignored" _newline
	}
}

loc a "`kwallis'"

foreach b of local a{
		
	loc continuous: subinstr loc continuous "`b'" "`b'", word count(loc c)
	if `c'==0{
		dis in red "the 'kwallis' option is only allowed for continuous variables; " as result "specification '`b'' ignored" _newline
		loc kwallis: subinstr loc kwallis "`b'" "", word all
	}
	loc kwallis: subinstr loc kwallis "`b'" "`b'", word all count(loc c)
	while `c'>1{ //																		identify duplicates of the chi and fisher options
		loc kwallis: subinstr loc kwallis "`b'" "", word
		loc c = `c' - 1
		if `c'==1 dis in red "'`b'' has been specified more than once in the 'kwallis' option; " as result "duplicates ignored" _newline
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h15"
*!	ALLOW FOR THE VALUECOUNTS OPTION (EXCEPT FOR TOTDENOM, WHICH WAS DONE WHILE CHECKING)!*

loc total_values " "
loc a: word 1 of `valuecounts'
loc b: word 2 of `valuecounts'
if "`a'"=="total" loc total_values "`b'"
*
																						if "$rafaeldebug"=="debug" dis as result "h16"
*!	ALLOW FOR THE ADAPTATION OF THE NUMBER OF DECIMALS	!*

loc c_decim = 1 //																		the default for means and standard deviations variables is one decimal
loc p_decim = 0 //																		the default for percents is zero decimals
loc q_decim = 1 //																		the default for quartiles is one decimal

foreach a of local decimals{

	loc b = substr("`a'",1,1) //														the first part contains the designation (c for continuous, p for percents, q for quartiles)
	loc c = substr("`a'",2,2) //														the second part contains the number of decimals
	
	foreach d in c p q{
		if "`b'"=="`d'" loc `d'_decim = `c' //											change the default decimals
	}
}
foreach a in c p q{
	loc `a'_decim_plus = ``a'_decim' + 1 //												the width must be at least one more than the decimals to be valid
}
*
																						if "$rafaeldebug"=="debug" dis as result "h17"
*!	ALLOW FOR THE PUNCTUATION OPTION	!*

loc tv1 "N = " //																		default punctuation of summary statistics
loc vv1 "n = "
loc vv2 ", "
loc cn2 " "
loc pc1 "("
loc pc2 "%)"
loc mm2 " ± "
loc mq2 " "
loc lq1 "("
loc lq2 "; "
loc uq2 ")"

loc pdi2 "% " //																		default punctuation of confidence intervalls
loc pll1 "("
loc pll2 "%; "
loc pul2 "%)"
loc mdi2 " "
loc mll1 "("
loc mll2 "; "
loc mul2 ")"

if `"`ccentiles'"'!="" loc sd2 "; " //													default punctuation if centiles are added
if `"`centiles'"'!="" loc uq2 ") "

if "`cndrop'"=="on"{ //																	default punctuation if elements are dropped
	loc pc1 ""
	loc pc2 "%"
}
if "`pcdrop'"=="on" loc cn2 ""
if "`sddrop'"=="on" loc mm2 ""
if "`sddrop'"=="on" & `"`ccentiles'"'!="" loc mm2 " "
if "`mqdrop'"=="on"{
	loc lq1 ""
	loc uq2 ""
	if `"`centiles'"'!="" loc uq2 "; "
}
if "`iqdrop'"=="on" & `"`centiles'"'=="" loc mq2 ""

if "`didrop'"=="on"{
	loc mll1 ""
	loc mul2 ""
	loc pll1 ""
	loc pul2 "%"
}
if "`cidrop'"=="on"{
	loc mdi2 ""
	loc pdi2 ""
}

forvalues b = 2 (2) 30{

	loc a = `b' - 1
	
	loc a: word `a' of `punctuation'
	loc b: word `b' of `punctuation'
	
	if "`a'"!="" loc `a' "`b'"
}
*

*!	ALLOW FOR THE HEADERS OPTION !*

// IS DONE AFTER GENERATING THE GROUP COLUMNS AND THEIR DEFAULT HEADERS IN ORDER TO ALLOW TO OVERWRITE THEM
*

*!	ALLOW FOR THE TABLENAME OPTION !*

// IS DONE RIGHT BEFORE GENERATING THE GROUP COLUMNS AS THE FIRST STEP WHICH MAY ALTER THE DATA SET
*

*!	ALLOW FOR THE CENTILES AND CCENTILES OPTIONS !*

// IS PART OF THE GROUP AND SUBGROUP COLUMN GENERATION
*

*!	ALLOW FOR THE CONFIDENCE OPTION !*

// CONFIDENCE INTERVALL COLUMNS ARE GENERATED AFTER THE GROUP AND SUBGROUP COLUMNS
*

*!	ALLOW FOR THE PVALUE OPTION !*

// THE DEFAULT PVALUE COLUMN IS GENERATED AFTER THE GROUP AND SUBGROUP COLUMNS
*
																						if "$rafaeldebug"=="debug" dis as result "h18"
*!	INFORM ABOUT SPECIFICATION BECOMING OBSOLETE THROUGH OTHER SPECIFICATIONS !*																				

if "`continuous'"=="" & `"`ccentiles'"'!=""{
	dis as text "no variables were classified as continuous; " as result "no use was made of the 'ccentiles' option"
	loc ccentiles "" //																	if there are no continuous variables, the ccentile formats are not computed, and an error will be generated processing the formats
}
if "`nonmetric'"=="" & `"`centiles'"'!=""{
	dis as text "no variables were classified as nonmetric; " as result "no use was made of the 'centiles' option"
	loc centiles "" //																	if there are no continuous variables, the ccentile formats are not computed, and an error will be generated processing the formats
}

foreach a in tv vv cn pc mm sd mq{
	loc drop: subinstr loc drop "`a'" "`a'", word count(loc b)
	loc punctuation: subinstr loc punctuation "`a'1" "`a'1", word count(loc c)
	loc punctuation: subinstr loc punctuation "`a'2" "`a'2", word count(loc d)
	if `b'>0 & (`c'>0 | `d'>0) dis as text "it does not make sense to specify a punctuation for '`a'' if '`a'' is dropped" as result "" _newline
}

loc punctuation: subinstr loc punctuation "mq1" "mq1", word count(loc a)
loc punctuation: subinstr loc punctuation "mq2" "mq2", word count(loc b)
loc punctuation: subinstr loc punctuation "lq1" "lq1", word count(loc c)
loc punctuation: subinstr loc punctuation "lq2" "lq2", word count(loc d)
loc punctuation: subinstr loc punctuation "uq1" "uq1", word count(loc e)
loc punctuation: subinstr loc punctuation "uq2" "uq2", word count(loc f)
if "`iqdrop'"=="on" & (`c'>0 | `d'>0 | `e'>0 | `f'>0) dis as text "it does not make sense to specify a punctuation for 'iq' if 'iq' is dropped" as result "" _newline
if "`nonmetric'"=="" & ("`mqdrop'"=="on" | "`iqdrop'"=="on" | `a'>0 | `b'>0 | `c'>0 | `d'>0 | `e'>0 | `f'>0) dis as text "it does not make sense to modify default medians or interquartile ranges if the 'nonmetric' option is not specified" as result "" _newline
																						if "$rafaeldebug"=="debug" dis as result "h18.5"
loc g: word count `subgroups'

loc punctuation: subinstr loc punctuation "pdi1" "pdi1", word count(loc a)
loc punctuation: subinstr loc punctuation "pdi2" "pdi2", word count(loc b)
loc punctuation: subinstr loc punctuation "mdi1" "mdi1", word count(loc c)
loc punctuation: subinstr loc punctuation "mdi2" "mdi2", word count(loc d)
if (`a'>0 | `b'>0 | `c'>0 | `d'>0) & ((("`CIdrop'"=="on" | `g'!=2) & "`confidence'"=="") | "`didrop'"=="on") dis as text "it does not make sense to specify a punctuation for differences if differences are dropped" as result "" _newline

foreach a in ll ul{
	loc punctuation: subinstr loc punctuation "p`a'1" "p`a'1", word count(loc b)
	loc punctuation: subinstr loc punctuation "p`a'2" "p`a'2", word count(loc c)
	loc punctuation: subinstr loc punctuation "m`a'1" "m`a'1", word count(loc d)
	loc punctuation: subinstr loc punctuation "m`a'2" "m`a'2", word count(loc e)
	if (`b'>0 | `c'>0 | `d'>0 | `e'>0) & ((("`CIdrop'"=="on" | `g'!=2) & "`confidence'"=="") | "`cidrop'"=="on") dis as text "it does not make sense to specify a punctuation for confidence intervals if confidence intervals are dropped" as result "" _newline
}
																						
if "`bindrop'"=="on" & "`numdrop'"=="on" dis as text "it is not necessary to specify 'binval' in option 'drop' if 'binlab' is specified" as result "" _newline
if `initiation'==3 & "`total_values'"!=" " dis as text "it does not make sense to specifiy a title for the total value counts if the total value counts are dropped" as result "" _newline

loc a: word count `subgroups'

if "`CIdrop'"=="on" & `a'!=2 dis as text "it is not necessary to specify 'deconf' in option 'drop' - a default confidence interval is only computed if there are exactly two subgroups" as result "" _newline
if "`Pdrop'"=="on" & `a'<2 dis as text "it is not necessary to specify 'depval' in option 'drop' - a default p value is only computed if there are at least two subgroups" as result "" _newline
if ("`didrop'"=="on" | "`cidrop'"=="on") & ("`CIdrop'"=="on" | `a'!=2) & "`confidence'"=="" dis as text "it is not necessary to specify 'di' or 'ci' in the 'drop' option if no confidence intervals are computed" as result "" _newline

if ("`Pdrop'"=="on" | `a'<2) & ("`chi2'"!="" | "`fisher'"!="" | "`anova'"!="" | "`kwallis'"!="") ///
dis as text "the 'chi2', 'fisher', 'anova', and 'kwallis' options do not make sense if there are less than two subgroups or the p-values are dropped" as result "" _newline

foreach b of local chi2{
	loc binary: subinstr loc binary "`b'" "`b'", word count(loc c)
	loc categorical: subinstr loc categorical "`b'" "`b'", word count(loc d)
	if (`c'>0 | `d'>0) & `a'>2 dis as text "it is not necessary to specify the 'chi2' option if there are more than two subgroups; a chi2 exact test is automatically computed for tables with more than four field " as result "" _newline
}
foreach b of local fisher{
	loc binary: subinstr loc binary "`b'" "`b'", word count(loc c)
	if `c'>0 & `a'==2 dis as text "it is not necessary to specify the 'fisher' option for binary variables if there are only two subgroups; a fisher's exact test is automatically computed for four field tables" as result "" _newline
}
*
																						if "$rafaeldebug"=="debug" dis as result "h19"
*!	CHECK VARIABLE LABELS OF SUMMARISED VARIABLES AND GENERATE DUMMY VARIABLES OF CATEGORICAL VARIABLES	!*

dis as result "LABELING OF SUMMARIZED VARIABLES" _newline

if "`novarlab'"!="" loc labelcheck "findings"

foreach a of local novarlab{
	dis as text "the variable '`a'' has no variable label;" as result " '`a'' is displayed as '`a''"
}
foreach a of local varlist{
	if trim("`a'")=="" dis as text "CAVE: the variable '`a'' is labeled with spaces only" as result ""
}

foreach a of local binary{
	
	loc b : val lab `a'
	
	capture lab list `b' //																first check the value label
	if _rc==111{
		dis in red "the value label of the binary variable '`a'' ('`b'') has never been defined;" as result " '`b'' ignored"
		lab def `b' 1 ""
		loc labdrop "`labdrop' `b'" //													avoids putting a cap before each lab list while keeping previous data unchanged
		loc labelcheck "findings"
	}
	
	qui su `a' if `genfil'
	loc c = r(max)
	
	if "`b'"==""{ //																	if there is no value label
		dis as text "the binary variable '`a'' has no value label;" as result " categories are named by their value"
		loc labelcheck "findings"
		
		if r(min)==`c' dis as text "the binary variable '`a'' is constant in the selected data;" as result " the 100% proportion of value `c' is displayed"
	}
	else if "`b'"!="" & r(min)==`c'{
		qui lab list `b' //																lab list does also return r(max)
		loc lowerbin: subinstr loc lowerbin "`a'" "`a'", word count(loc c) //			ALLOW FOR THE LOWERBIN OPTION
		if `c'==0 loc d = r(max)
		else loc d = r(min)
		loc d: lab (`a') `d', strict
		dis as text "the binary variable '`a'' is constant in the selected data; '`a'' has a value label;" as result " the proportion of category '`d'' is displayed"
	}
	else{
		qui lab list `b' // lab list does als generate r(min) and r(max)
		loc d = r(min) 
		if `d'==r(max){
			dis as text "the binary variable '`a'' has a value label, but only the value `d' is labeled" as result ""
			loc labelcheck "findings"
		}
	}
}
																						if "$rafaeldebug"=="debug" dis as result "h19.1"
foreach a of local categorical{
	
	qui su `a'
	local n1 = r(min)
	local n2 = r(max)
	
	loc d: val lab `a'
	
	if "`d'"==""{ //																	if there is no value label
		dis as text "the categorical variable '`a'' has no value label;" as result " categories are named by their value"
		tempname d //																	define a temporary name (but do not label a variable with it) to avoid error meassges with "loc e: label `d' `b', strict"; `d' cannot be empty, but it can correcpond to a non-existing label
		lab def `d' 1 ""	
		loc labelcheck "findings"
	}
																						if "$rafaeldebug"=="debug" dis as result "h19.2"	
	capture lab list `d' //															first check the value label
	if _rc==111{
		dis in red "the value label of the categorical variable '`a'' ('`d'') has never been defined;" as result " '`d'' ignored"
		lab def `d' 1 ""
		loc labdrop "`labdrop' `d'" //													avoid putting a cap before each lab list while keeping previous data unchanged
		loc labelcheck "findings"
	}
	loc n1 = min(`n1',r(min))
	loc n2 = max(`n2',r(max))
																						if "$rafaeldebug"=="debug" dis as result "h19.3"
	loc g ""
	
	foreach b of numlist `n2' (-1) `n1'{ //												check all label and value numbers from the highest to the lowest (so they will be ordered from the lowest to the highest)

		loc e: label `d' `b', strict
		qui count if `a'==`b' & `genfil'
		
		if "`e'"!="" & r(N)!=0{ //														if a value is labelled by the value label and appears in the data
			
			tempvar c

			qui gen `c' = `a'==`b' //													generate a temporary dummy variable
			qui replace `c' = . if `a'>=.
			
			lab var `c' " `e'" //														label the temporary dummy variable with the indented label of the corresponding value
			
			loc indented: subinstr loc indented "`a'" "`a'", word all count(loc f) //	indent the cateory one more than its variable
			loc f = `f' + 1
			loc f: dis %`f's ""
			lab var `c' "`f'`e'"

			loc order: subinstr loc order "`a'" "`a' `c'", word //						order the lower value dummy-variable behind the non-dummy-variable (`order' was defined in the partitioning)
			loc _cat "`_cat' `c'" //													variables in this macro will be treated like binary variables; here, the order does not matter
			loc chi2: subinstr loc chi2 "`a'" "`a' `c'", word //						if a categorical variable is specified in the 'chi2' options, its individual p-values are computed by chi2 test, too
			loc fisher: subinstr loc fisher "`a'" "`a' `c'", word //					if a categorical variable is specified in the 'fisher' options, its individual p-values are computed by fisher's exact test, too
		}
																						if "$rafaeldebug"=="debug" dis as result "h19.4"
		if "`e'"!="" & r(N)==0 { //														if a value is labelled by the value label but does not appear in the data 

			tempvar c
			qui gen `c' = 0 if `a'<. //													generate a temporary dummy variable equal to .
			
			lab var `c' " `e'" //														label the temporary dummy variable with the indented label of the corresponding value
			
			loc indented: subinstr loc indented "`a'" "`a'", word count(loc f) //		indent the cateory one more than its variable
			loc f = `f' + 1
			loc f: dis %`f's ""
			lab var `c' "`f'`e'"
			
			if "`emptydrop'"==""{ //													AND THE DROP(EMPTYCAT) OPTION IS NOT ACTIVATED
				dis as text "the category '`e'' of the categorical variable '`a'' was not found in the selected data;" as result " all counts for this category are zero"
				loc order: subinstr loc order "`a'" "`a' `c'", word //					order the lower value dummy-variable behind the non-dummy-variable (`order' was defined in the partitioning)
				loc _cat "`_cat' `c'"
				loc chi2: subinstr loc chi2 "`a'" "`a' `c'", word //					if a categorical variable is specified in the 'chi2' options, its individual p-values are computed by chi2 test, too
				loc fisher: subinstr loc fisher "`a'" "`a' `c'", word //				if a categorical variable is specified in the 'fisher' options, its individual p-values are computed by fisher's exact test, too
			}
			else dis as text "the category '`e'' of the categorical variable '`a'' was not found in the selected data;" as result " category dropped by specification 'emptyc' of the 'drop' option"
		}
																						if "$rafaeldebug"=="debug" dis as result "h19.5"
		if "`e'"=="" & r(N)!=0{ //														if a value is not labelled by the value label but appears in the data

			loc g "`b', `g'"
			loc labelcheck "findings"

			tempvar c
			qui gen `c' = `a'==`b' //													also generate a temporary dummy variable
			qui replace `c' = . if `a'>=.
			
			loc f: var lab `a' //														but label it with the indented name of the variable and the value in parentheses
			lab var `c' " `f' (`b')" //													`f' is already indented
			loc order: subinstr loc order "`a'" "`a' `c'", word //						order the lower value dummy-variable behind the non-dummy-variable (`order' was defined in the partitioning)
			loc _cat "`_cat' `c'" //													variables in this macro will be treated like binary variables; here, the order does not matter
			loc chi2: subinstr loc chi2 "`a'" "`a' `c'", word //						if a categorical variable is specified in the 'chi2' options, its individual p-values are computed by chi2 test, too
			loc fisher: subinstr loc fisher "`a'" "`a' `c'", word //					if a categorical variable is specified in the 'fisher' options, its individual p-values are computed by fisher's exact test, too
		}
	}
	loc h: length loc g
	loc h = `h' - 2
	loc g = substr("`g'",1,`h')
	if "`g'"!="" dis as text "the categorical variable '`a'' has a value label, but the following values are not labeled: `g'" as result ""
}

if "`labelcheck'"=="" dis as text "all summarized variables and values are labeled" as result ""
*
																						if "$rafaeldebug"=="debug" dis as result "h21"
*!	CHECK THE LABELLING OF SUBGROUP DEFINING VARIABLES AND FIND THE DEFAULT COLUMN HEADERS	!*

dis _newline as result "LABELING OF SUBGROUP DEFINING VARIABLES" _newline

loc g = 0

if `"`subgroups'"'==`""' dis as text "no subgroups defined" as result ""

foreach a of local subgroups{
	
	loc g = `g' + 1
	
	foreach b in "&" "|" "inlist(" "inrange(" "," "(" ")" "==" ">=" "<=" ">" "<" "!=" "~=" {
		loc a: subinstr loc a "`b'" " ", all
	}
																						if "$rafaeldebug"=="debug" dis as result "h21.3"
	foreach b of local a{
		capture confirm numeric var `b'
		if _rc==0{
			loc subgroup_`g' "`b'"
			loc subgroup_`g'_varlab: var lab `b'
			loc subgroup_`g'_vallab: val lab `b'
			if "`subgroup_`g'_varlab'"!="" & "`subgroup_`g'_vallab'"!="" ///
			dis as text "the first variable in the definition of subgroup `g' is '`b''; '`b'' has a variable and a value label"
			else if "`subgroup_`g'_varlab'"!="" & "`subgroup_`g'_vallab'"=="" ///
			dis as text "the first variable in the definition of subgroup `g' is '`b''; '`b'' has a variable but no value label; " ///
			as result "categories are named by their value"
			else if "`subgroup_`g'_varlab'"=="" & "`subgroup_`g'_vallab'"!="" ///
			dis as text "the first variable in the definition of subgroup `g' is '`b''; '`b'' has no variable label; " ///
			as result "'`b'' is displayed as '`b''; " as text "'`b'' has a value label"
			else dis as text "the first variable in the definition of subgroup `g' is '`b''; '`b'' has no variable label; " ///
			as result "'`b'' is displayed as '`b''; " as text "'`b'' has no value label; " as result "categories are named by their value"
			capt lab list `subgroup_`g'_vallab'
			if _rc==111{
				dis in red "the value label of the variable '`a'' ('`subgroup_`g'_vallab'') has never been defined;" as result " '`subgroup_`g'_vallab'' ignored"
				lab def `subgroup_`g'_vallab' 1 ""
				loc labdrop "`labdrop' `subgroup_`g'_vallab'" //						avoids putting a cap before each lab list while keeping previous data unchanged
			}
			continue, break
		}
	}
	if "subgroup_`g'"=="" dis as text "no variables in the definition of subgroup `g'"
}	
*	
																						if "$rafaeldebug"=="debug" dis as result "h22"
*!	LOOK FOR A FREE TABLE NAME (DEFAULT), ... 

if "`tablename'"==""{
	loc a = 1
	capt assert 1==1
	while _rc!=111{
		capt su browsertable`a'*
		if _rc==111{
			loc x "browsertable`a'"
		}
		loc a = `a' + 1
	}
	loc tablename "`x'"
}

*... STOP THE PROGRAM IF THE TABLE IS TOO LONG, ...

loc a: word count `subgroups'
loc b: word count `confidence'
loc a = `a' + (`b'/3) + 2 //															the row title column plus the group 1 column
if "`Pdrop'"!="on" loc a = `a' + 1

if `a'>17{
	dis _newline(2) in red "a mximum of 17 columns per table is allowed (corresponds to one A4 page, landscape orientation, 1 cm margins, courier font, size 5, 15 characters per column); this table would require `a' columns; program stopped"
	
	foreach a of local indented{ //														relabel indented labels
		loc b: var label `a'
		loc b: subinstr loc b " " ""
		lab var `a' "`b'"
	}
	foreach a of local novarlab{ //														for simplicity, all non-labelled variables were labelled with their own varname
		lab var `a'
	}
	if "`labdrop'"!="" lab drop `labdrop' //											avoids putting a cap before each lab list while keeping previous data unchanged
		
	exit
}

loc a: word count `order'
loc a = `a' + `initiation' - 1

if `a'>250{
	dis _newline(2) in red "a mximum of 250 rows per table is allowed (corresponds to two A4 pages, portrait orientation, 1 cm margins, courier font, size 5); this table would require `a' rows; program stopped"
	
	foreach a of local indented{ //														relabel indented labels
		loc b: var label `a'
		loc b: subinstr loc b " " ""
		lab var `a' "`b'"
	}
	foreach a of local novarlab{ //														for simplicity, all non-labelled variables were labelled with their own varname
		lab var `a'
	}
	if "`labdrop'"!="" lab drop `labdrop' //											avoids putting a cap before each lab list while keeping previous data unchanged
	
	exit
}

*... DROP CONFLICTING VARIABLES IF THE TABLENAME WAS SPECIFIED, ...	!*

loc d = _N
if "`tablename'"!="`x'"{ //																if there is another tablename than the one which would have been assigned
	loc b ""
	capt su `tablename'*
	if _rc!=111{
		foreach a of varlist `tablename'*{
			if "`a'"!="`tablename'_`d'" loc b "`b', `a'"
		}
		loc b: subinstr loc b ", " ""
		loc c: subinstr loc b ", " " ", all
		if "`b'"!=""{
			loc remember3 "warning: the following variables have been dropped: `b'"
			drop `c'
		}
	}
}
																						if "$rafaeldebug"=="debug" dis as result "h23"
*... AND ADD OBSERVATIONS IN VERY SMALL DATA SETS	!*

loc a: word count `order'
loc a = `a' + `initiation' - 1
if `a'<7 loc a = 7
 
if `a'>_N{
	loc b = _N
	qui set obs `a'
	qui gen `tablename'_`a' = `genfil'==.
	qui replace `genfil' = 0 if `genfil'==.
	qui count if `tablename'_`a'==1
	loc c = r(N)
	loc remember1 `"in red "the observation number was smaller than the number of table rows; " as result "the observation number has been increased by `c'""'
	loc remember2 "remember to add 1/`b' to the 'in' or '`tablename'_`a'==0' to the 'if' expression of all further commands"
	
	foreach b of varlist _all{
		capture confirm numeric var `b'
		if _rc==0 qui replace `b' = .z if `tablename'_`a'==1 & `b'==.
		else qui replace `b' = "`tablename'_`a'" if `tablename'_`a'==1 & `b'==""
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h24"
*!	GENERATE FIRST (EXPLANATORY) COLUMN OF THE DESCRIPTIVE TABLES AS A NEW VARIABLE	!*

dis _newline as result upper("Creation of `tablename'") _newline
loc preinitiation = `initiation' - 1 //													required to merge columns later on
qui gen `tablename' = " " in 1/`preinitiation' //										the name of the first row is equal to the trunc of all furter table names
format `tablename' %-4s

if `initiation'==5 qui replace `tablename' = "`total_values'" in 3 //					if the groupcounts are not droped, insert the row header for the (large) groupcounts, if any (the second condition is necessary to fit the requirements of the saveltablefile command, i.e. no cell of the table corresponds to missing)

loc row_number = `initiation' - 1
foreach a of local order{ //															the order variable corresponds to gl summarytable completed with the dummy variables
	
	loc row_number = `row_number' + 1 //												required to merge columns later on
	
	qui replace `tablename' = "`a'" in `row_number'
}
*
																						if "$rafaeldebug"=="debug" dis as result "h25"
*!	GENERATE THE TOTAL GROUP COLUMN AND ITS HEADER !* 

qui gen `tablename'c1 = " " in 2/`preinitiation' //										to fit the requirements of the saveltablefile command, make sure that no cell of the table corresponds to missing

loc g = `g' - 1
forvalues a = 1/`g'{
	loc b = `a' + 1
	if "`subgroup_`a''"=="`subgroup_`b''" loc c "same"
	else loc c ""
}
loc a = lower("`subgroup_1'")
if "`subgroup_1_varlab'"!="" loc a = lower("`subgroup_1_varlab'")
if "`c'"=="same"{
	dis as text "the first variable is '`subgroup_1'' in all subgroup definitions; " ///
	as result "the main group is named 'All `a' groups'"
	qui replace `tablename'c1 = "All `a' groups" in 1
}
else if `g'==0{
	dis as text "only one subgroup defined; the first variable of subgroup 1 is '`subgroup_1''; " ///
	as result "the main group is named 'All `a' groups'"
	qui replace `tablename'c1 = "All `a' groups" in 1
}
else if `g'==-1{
	dis as text "no subgroups defined; " as result "the total group is named 'Total group'"
	qui replace `tablename'c1 = "Total group" in 1
}
else{
	dis as text "the first variable is not identical across subgroup definitions; " ///
	as result "the total group is named 'Total group'"
	qui replace `tablename'c1 = "Total group" in 1
}
*
																						if "$rafaeldebug"=="debug" dis as result "h26"
*!	GENERATE THE SUBGROUP COLUMNS AND THEIR HEADERS !*

loc a = 1 //																			are for the varnames and dependent of subgroup definitions

foreach b of local subgroups{
	
	loc a = `a' + 1 //																	`a' counts all groups including the total group, `c' counts only the subgroups
	loc c = `a' - 1
	loc f "" //																			"growing" contents
	loc g ""
	loc i ""
	
	if inlist(`a'`errorcolumns'){
		qui gen `tablename'c`a' = "Subgroup `c'" in 1
		continue
	}
	
	foreach d in "&" "|"{ //															variables are separated by & and |
		loc b: subinstr loc b "`d'" " ", all
	}
	foreach d of local b{ //															isolate all peaces containing the first subgroup defining variable and its values
		loc d: subinstr loc d "`subgroup_`c''" "`subgroup_`c''", count(loc e)
		if `e'>0 loc f "`f' `d'"
	}
	foreach d in "inlist(" "inrange(" "," "(" ")" "==" ">=" "<=" ">" "<" "!=" "~="{ //	separate the subgroup defining variable from its values
		loc f: subinstr loc f "`d'" " ", all
	}
	foreach d of local f{ //															identify the values of the isolated peaces
		capt conf integer n `d'
		if _rc==0{
			if "`subgroup_`c'_vallab'"=="" loc g "`g',`d'" //							if the values are not labelled, generate a string with the varname and the values in parentheses separated by ","
			else{
				loc e: lab `subgroup_`c'_vallab' `d', strict //							else list the leabels by "," and "and"
				if "`e'"!="" loc g "`g', `e'"
				else loc i "`i', `d'"
			}
		}
	}
	if "`subgroup_`c'_vallab'"==""{
		if "`subgroup_`c'_varlab'"=="" loc d = "`subgroup_`c'' (" + substr("`g'",2,.) + ")" //															header string: replace the last comma by a closing parenthese
		else loc d = trim("`subgroup_`c'_varlab' (") + substr("`g'",2,.) + ")"
		loc f = reverse(substr("`g'",2,.)) //											result window string: remove the last comma
		loc f: subinstr loc f "," " ,", all
		loc f: subinstr loc f "," "dna ," //											result window string: replace the last comma by comma and
		loc f = reverse("`f'")
		loc e: word count `f' //														result window string: if there are only two values separated by "and", remove the comma
		if `e'==3 loc f: subinstr loc f "," ""
		if `e'==1{ //																	result window string: if there is only one value, put "is" before the value and put "value" in singular
			loc f "is `f'"
			loc g ""
		}
		else{
			loc f "are `f'"
			loc g "s"
		}
		dis as text "the first variable in the definition of subgroup `c' is '`subgroup_`c'''; the '`subgroup_`c''' value`g' allowed in the definition of subgroup `c' `f'; " ///
		as result "subgroup `c' is named '`d''"
	}
	else{
		loc d = substr("`g'",3,.) //													header and result window string: remove the first comma and space
		loc d = reverse("`d'") //														header and result window string: replace the last comma by "comma and"
		loc d: subinstr loc d "," "dna ,"
		loc d = reverse("`d'")
		
		loc e: subinstr loc d ", " "', '", all
		loc e: subinstr loc e " 'and " " and '"
		
		loc f: word count `d' //														header and result window string: if there are only two categories separated by "and", remove the comma
		if `f'==3{
			loc d: subinstr loc d "," ""
			loc e: subinstr loc e "," ""
		}
		if `f'==1{ //																	result window string: if there is only one category, put "is" before the category and put "category" in singular
			loc e "is '`e''"
			loc f "y"
		}
		else{
			loc e "are '`e''"
			loc f "ies"
		}
		loc d = upper(substr("`d'",1,1)) + substr("`d'",2,.) //							header: uppercase the first letter
		if "`i'"!="" loc i = "the following values allowed in the definition of subgroup `c' are not covered by the value label: " + substr("`i'",3,.) + "; "
		dis as text "the first variable in the definition of subgroup `c' is '`subgroup_`c'''; the '`subgroup_`c''' categor`f' allowed in the definition of subgroup `c' `e'; `i'" ///
		as result "subgroup `c' is named '`d''"
	}
	
	qui gen `tablename'c`a' = "`d'" in 1
	qui replace `tablename'c`a' = " " in 2/`preinitiation' //							to fit the requirements of the saveltablefile command, make sure that no cell of the table corresponds to missing
}
*
																						if "$rafaeldebug"=="debug" dis as result "h27"
*!	HEADERS OPTION !*

loc c: word count `headers'

forvalues b = 2(2)`c'{ //																the g from the column generation adapted for the pvalue check

	loc a = `b' - 1  //																	create macros numbered according to where they belong and containing the corresponding partition name
		
	loc a: word `a' of `headers'
	loc b: word `b' of `headers'
	loc b: subinstr loc b `"""' `""'
	
	qui replace `tablename'c`a' = "`b'" in 1	
}
*
																						if "$rafaeldebug"=="debug" dis as result "h28"
*!	COMPLETE THE TOTAL GROUP COLUMN AND SUBGROUP COLUMNS WITH THE NUMBERS OF TOTAL VALUES OR WITH ERROR MESSAGES	!*

loc a = 0

foreach b of local grouplist{
	
	loc a = `a' + 1
																						if "$rafaeldebug"=="debug" dis as result "h28.5"
	if inlist(`a'`errorcolumns'){
		
		qui replace `tablename'c`a' = "Error: invalid subgroup defining" in 3
		qui replace `tablename'c`a' = "expression; check the syntax and" in 4
		qui replace `tablename'c`a' = "verify that all variables are nu" in 5
		qui replace `tablename'c`a' = "meric and that categories are re" in 6
		qui replace `tablename'c`a' = "fered by their values" in 7
	}
	else{
		qui count if (`b') & `genfil' //												numbers of total values
		loc c = r(N)
		if `initiation'==5 qui replace `tablename'c`a' = "`tv1'`c'`tv2'" in 3 //		(`initiation'==5 means that the numbers of total values are not dropped)
	}
}
*	
																						if "$rafaeldebug"=="debug" dis as result "h29"
*! DETERMINE THE PROPER FORMAT FOR EACH THE VARIABLE DEPENDENT STATISTIC !*
	
loc a = 0

foreach b of local grouplist{

	loc a = `a' + 1

	if inlist(`a'`errorcolumns') continue
	
	loc vv_length`a' = 0 //																number of valid values
	loc cn_length`a' = 0 //																counts of binary and categorical variables
	loc pc_length`a' = 0 //																percentages
	loc mm_length`a' = 0 //																means
	loc sd_length`a' = 0 //																standard deviations
	loc mq_length`a' = 0 //																medians
	loc lq_length`a' = 0 //																lower quartiles
	loc uq_length`a' = 0 //																upper quartiles

	foreach c of local varlist{ //														number of valid values
		
		qui count if `c'<. & (`b') & `genfil'
		loc d = r(N)
		loc d "`d'`vv2'"
		loc d: length loc d
		if `vv_length`a''<`d' loc vv_length`a' = `d'
	}
																						if "$rafaeldebug"=="debug" dis as result "h29.2"
	foreach c of local binary{ //														counts and percentages of binary variables
		
		loc lowerbin: subinstr loc lowerbin "`c'" "`c'", word count(loc d) //			ALLOW FOR THE LOWERBIN OPTION

		qui su `c' if `genfil' //														counts and percentages of binary variables
		
		if r(min)==r(max){ //															if only one value is found in the data, the higher value is tried to be determined by the value label
			loc e: val lab `c'
			if "`e'"!="" qui lab list `e' //											lab list does also return r(max)
		}
		if `d'==0 qui count if `c'==r(max) & (`b') & `genfil'
		
		else qui count if `c'==r(min) & (`b') & `genfil' //								counts of binary variables
		loc d = r(N)
		loc e "`cn1'`d'`cn2'"
		loc e: length loc e
		if `cn_length`a''<`e' loc cn_length`a' = `e'
		
		qui count if `c'`not_missing' & (`b') & `genfil' //								percentages of binary variables
		loc e: dis %`p_decim_plus'.`p_decim'f (`d'/r(N))*100
		loc e "`pc1'`e'`pc2'"
		loc e: length loc e
		if `pc_length`a''<`e' loc pc_length`a' = `e'
	}
	foreach c of local _cat{ //															counts of categorical variables
		
		qui su `c'
		
		if r(max)!=0 qui count if `c'==r(max) & (`b') & `genfil' //						r(N)==0 if dummy of _cat==. (i.e. labeled, but not in the data set) - no problem for binaries, because of the parentheses indication
		else qui count if `c'!=r(max) & `c'<. & (`b') & `genfil' //						sets loc d to 0
		loc d = r(N)
		loc e "`cn1'`d'`cn2'"
		loc e: length loc e
		if `cn_length`a''<`e' loc cn_length`a' = `e'
		
		qui count if `c'`not_missing' & (`b') & `genfil' //								percentages of caegorical variables
		loc e: dis %`p_decim_plus'.`p_decim'f (`d'/r(N))*100
		loc e "`pc1'`e'`pc2'"
		loc e: length loc e
		if `pc_length`a''<`e' loc pc_length`a' = `e'
	}
																						if "$rafaeldebug"=="debug" dis as result "h29.5"
	foreach c of local continuous{ //													means and standard deviations
		
		qui su `c' if (`b') & `genfil'
		
		loc d: dis %`c_decim_plus'.`c_decim'f r(mean) //								means
		loc d "`mm1'`d'`mm2'"
		loc d: length loc d
		if `mm_length`a''<`d' loc mm_length`a' = `d'
				
		loc d: dis %`c_decim_plus'.`c_decim'f r(sd) //									standard deviations
		loc d "`sd1'`d'`sd2'"
		loc d: length loc d
		if `sd_length`a''<`d' loc sd_length`a' = `d'
	}
	
	loc d: word count `ccentiles' //													ALLOW FOR THE CCENTILES OPTION
			
	forvalues g = 3(3)`d'{

		loc e = `g' - 2
		loc f = `g' - 1
								
		loc e: word `e' of `ccentiles'
		loc f: word `f' of `ccentiles'
		loc g: word `g' of `ccentiles'
													
		loc cc`e'_length`a' = 0
			
		foreach h of local continuous{
			
			qui centile `h' if (`b') & `genfil', centile(`e')
						
			loc i: dis %`q_decim_plus'.`q_decim'f r(c_1)
			loc i "`f'`i'`g'"
			loc i: length loc i
			if `cc`e'_length`a''<`i' loc cc`e'_length`a' = `i'
		}
	}
																						if "$rafaeldebug"=="debug" dis as result "h29.7"
	foreach c of local nonmetric{ //													median, lower, and upper quartiles
		
		qui centile `c' if (`b') & `genfil', centile(25 50 75)
		
		loc d: dis %`q_decim_plus'.`q_decim'f r(c_2) //									medians
		loc d "`mq1'`d'`mq2'"
		loc d: length loc d
		if `mq_length`a''<`d' loc mq_length`a' = `d'
		
		loc d: dis %`q_decim_plus'.`q_decim'f r(c_1) //									lower quartiles
		loc d "`lq1'`d'`lq2'"
		loc d: length loc d
		if `lq_length`a''<`d' loc lq_length`a' = `d'
				
		loc d: dis %`q_decim_plus'.`q_decim'f r(c_3) //									upper quartiles
		loc d "`uq1'`d'`uq2'"
		loc d: length loc d
		if `uq_length`a''<`d' loc uq_length`a' = `d'
	}

	loc d: word count `centiles' //														ALLOW FOR THE CENTILES OPTION
			
	forvalues g = 3(3)`d'{

		loc e = `g' - 2
		loc f = `g' - 1
							
		loc e: word `e' of `centiles'
		loc f: word `f' of `centiles'
		loc g: word `g' of `centiles'
												
		loc c`e'_length`a' = 0
		
		foreach h of local nonmetric{
		
			qui centile `h' if (`b') & `genfil', centile(`e')
					
			loc i: dis %`q_decim_plus'.`q_decim'f r(c_1)
			loc i "`f'`i'`g'"
			loc i: length loc i
			if `c`e'_length`a''<`i' loc c`e'_length`a' = `i'
		}
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h30"
*!	DERIVE THE COLUMN WIDTHS FROM THE FORMATS OF THE VARIABLE DEPENDENT STATISTICS !*

loc a = 0

foreach b of local grouplist{
		
	loc a = `a' + 1
	
	if inlist(`a'`errorcolumns') continue
		
	loc _vv`a' = `vv_length`a'' +  length("`vv1'") //									spaces between statistics are accounted for in the behind punctuation
	loc _cp`a' = `cn_length`a'' + `pc_length`a''
	loc _ms`a' = `mm_length`a'' + `sd_length`a''
	loc _mi`a' = `mq_length`a'' + `lq_length`a'' + `uq_length`a''
																						if "$rafaeldebug"=="debug" dis as result "h30.2"
	if "`vvdrop'"=="on" loc _vv`a' = 1
	if "`cndrop'"=="on" loc _cp`a' = `pc_length`a''
	if "`pcdrop'"=="on" loc _cp`a' = `cn_length`a''
	if "`cndrop'"=="on" & "`pcdrop'"=="on" loc _cp`a' = 1
	if "`mmdrop'"=="on" loc _ms`a' = `sd_length`a''
	if "`sddrop'"=="on" loc _ms`a' = `mm_length`a''
	if "`mmdrop'"=="on" & "`sddrop'"=="on" loc _ms`a' = 1
	if "`mqdrop'"=="on" loc _mi`a' = `lq_length`a'' + `uq_length`a''
	if "`iqdrop'"=="on" loc _mi`a' = `mq_length`a''
	if "`mqdrop'"=="on" & "`iqdrop'"=="on" loc _mi`a' = 1
																						if "$rafaeldebug"=="debug" dis as result "h30.4"
	if `"`ccentiles'"'!=`"`centiles'"'{
		foreach ccc in c cc{
		
			loc d: word count ``ccc'entiles'
			loc _`ccc'ent`a' = 0

			forvalues e = 3(3)`d'{
				
				loc e = `e' - 2
				loc e: word `e' of ``ccc'entiles'
				
				loc _`ccc'ent`a' = `_`ccc'ent`a'' /*+ 1*/ + ``ccc'`e'_length`a''
			}
		}
	}
	else{
		loc d: word count `centiles'
		loc _cccent`a' = 0

		forvalues e = 3(3)`d'{
				
			loc e = `e' - 2
			loc e: word `e' of `centiles'
			
			loc ccc`e'_length`a' = max(`c`e'_length`a'',`cc`e'_length`a'')
			
			loc _cccent`a' = `_cccent`a'' + `ccc`e'_length`a''
		}
	}
																						if "$rafaeldebug"=="debug" dis as result "h30.6"
	if `"`ccentiles'"'!=`"`centiles'"'{
		loc _mscc`a' = `_ms`a'' + `_ccent`a'' //										in case `"`ccentiles'"'!=`"`centiles'"'
		loc _mic`a' = `_mi`a'' + `_cent`a''
		loc pretot`a' = max(`_cp`a'',`_mscc`a'',`_mic`a'')
	}
	else{
		loc _msmi`a' = max(`_ms`a'',`_mi`a'') //										in case `"`ccentiles'"'==`"`centiles'"'
		loc _msmiccc`a' = `_msmi`a'' + `_cccent`a''
		loc pretot`a' = max(`_cp`a'',`_msmiccc`a'')
	}
	
	loc tot`a' = `pretot`a'' + `_vv`a''
}	
*
																						if "$rafaeldebug"=="debug" dis as result "h31"
*!	COMPLETE THE TOTAL GROUP COLUMN AND SUBGROUP COLUMNS WITH THE VARIABLE DEPENDENT STATISTICS	!*

loc a = 0

foreach b of local grouplist{

	loc a = `a' + 1

	if inlist(`a'`errorcolumns') continue
	
	foreach c of local binary{ //														binary variables
		
		qui count if `c'<. & (`b') & `genfil' //										numbers of valid values
		loc d = r(N)
		loc vv: dis %`vv_length`a''s "`d'`vv2'" //										vv1 is the only before punctuation which is left aligned, except for opening parentheses
		loc vv: dis %`_vv`a''s "`vv1'`vv'"
		while "`d'"!="`vv'"{
			loc d "`vv'"
			loc vv: subinstr loc vv "( " " (", all
			loc vv: subinstr loc vv "[ " " [", all
		}
		
		loc lowerbin: subinstr loc lowerbin "`c'" "`c'", word count(loc d) //			ALLOW FOR THE LOWERBIN OPTION
			
		qui su `c' if `genfil' //														counts and percentages
	
		if r(min)==r(max){ //															if only one value is found in the data, the higher value is tried to be determined by the value label
			loc e: val lab `c'
			if "`e'"!="" qui lab list `e' //											lab list does also return r(max)
		}
																						if "$rafaeldebug"=="debug" dis "h31.5"
		if `d'==0 qui count if `c'==r(max) & (`b') & `genfil' //						counts
		else qui count if `c'==r(min) & (`b') & `genfil'
		loc d = r(N)
		loc cn: dis %`cn_length`a''s "`cn1'`d'`cn2'"
		
		qui count if `c'`not_missing' & (`b') & `genfil' //								percentages
		loc e: dis %`p_decim_plus'.`p_decim'f (`d'/r(N))*100
		loc pc: dis %`pc_length`a''s "`pc1'`e'`pc2'"
		
		if "`vvdrop'"=="on" loc vv "" //												ALLOW FOR THE drop(vv cn pc) OPTIONS
		if "`cndrop'"=="on" loc cn ""
		if "`pcdrop'"=="on" loc pc ""
		
		loc d: dis %`_cp`a''s "`cn'`pc'" //												counts and percentages are aligned
		loc d: dis %~`pretot`a''s "`d'" //												the combination of counts and percentages is not aligned with other combinations but must take the same space
		loc d: dis %`tot`a''s "`vv'`d'"
		
		qui replace `tablename'c`a' = "`d'" if `tablename'=="`c'" //					insert results into the proper row
	}
	
	foreach c of local categorical{ //													numbers of valid values of categorical variables
	
		qui count if `c'<. & (`b') & `genfil' //										numbers of valid values
		loc d = r(N)
		loc vv: dis %`vv_length`a''s "`d'`vv2'" //										vv1 is the only before punctuation which is left aligned, except for opening parentheses
		loc vv: dis %-`_vv`a''s "`vv1'`vv'"
		while "`d'"!="`vv'"{
			loc d "`vv'"
			loc vv: subinstr loc vv "( " " (", all
			loc vv: subinstr loc vv "[ " " [", all
		}

		if "`vvdrop'"=="on" loc vv "" //												ALLOW FOR THE drop(vv cn pc), valuenum(missing()), and valuenum(missclean()) OPTIONS
		if "`cndrop'"=="on" loc cn ""
		if "`pcdrop'"=="on" loc pc ""
		
		loc d: dis %-`tot`a''s "`vv'"
		
		qui replace `tablename'c`a' = "`d'" if `tablename'=="`c'" //					insert results into the proper row
	}
	
	foreach c of local _cat{ //															categories of categorical variables
		
		loc vv: dis %`_vv`a''s "" //													align statistics of categorical variable with those of binary variables
		
		qui su `c' //																	counts and percentages
		
		if r(max)!=0 qui count if `c'==r(max) & (`b') & `genfil' //						r(N)==0 if dummy of _cat==. (i.e. labeled, but not in the data set) - no problem for binaries, because of the parentheses indication
		else qui count if `c'!=r(max) & `c'<. & (`b') & `genfil' //						sets loc d to 0
		loc d = r(N)
		loc cn: dis %`cn_length`a''s "`cn1'`d'`cn2'"
		
		qui count if `c'`not_missing' & (`b') & `genfil' //								percentages
		loc e: dis %`p_decim_plus'.`p_decim'f (`d'/r(N))*100
		loc pc: dis %`pc_length`a''s "`pc1'`e'`pc2'"		
		
		if "`vvdrop'"=="on" loc vv "" //												ALLOW FOR THE drop(vv cn pc) OPTIONS
		if "`cndrop'"=="on" loc cn ""
		if "`pcdrop'"=="on" loc pc ""
		
		loc d: dis %`_cp`a''s "`cn'`pc'" //												counts and percentages are aligned
		loc d: dis %~`pretot`a''s "`d'" //												the combination of counts and percentages is not aligned with other combinations but must take the same space
		loc d: dis %`tot`a''s "`d'"
		
		qui replace `tablename'c`a' = "`d'" if `tablename'=="`c'" //					insert results into the proper row
	}			
	
	foreach c of local continuous{ //													continuous variables of the second to fourth columns
		
		qui count if `c'<. & (`b') & `genfil' //										numbers of valid values
		loc d = r(N)
		loc vv: dis %`vv_length`a''s "`d'`vv2'" //										vv1 is the only before punctuation which is left aligned, except for opening parentheses
		loc vv: dis %`_vv`a''s "`vv1'`vv'"
		while "`d'"!="`vv'"{
			loc d "`vv'"
			loc vv: subinstr loc vv "( " " (", all
			loc vv: subinstr loc vv "[ " " [", all
		}
		
		qui su `c' if (`b') & `genfil'
		
		loc d: dis %`c_decim_plus'.`c_decim'f r(mean) //								means
		loc mm: dis %`mm_length`a''s "`mm1'`d'`mm2'"

		loc d: dis %`c_decim_plus'.`c_decim'f r(sd) //									standard deviations
		loc sd: dis %`sd_length`a''s "`sd1'`d'`sd2'"
				
		loc j "" //																		loc d: dis %`_msmiccc`a''s "`d'`j'"
		if `"`ccentiles'"'!=`""'{ //													ALLOW FOR THE CCENTILES OPTION
			
			loc d: word count `ccentiles'
			
			forvalues g = 3(3)`d'{

				loc e = `g' - 2  //														create macros numbered according to where they belong and containing the corresponding partition name
				loc f = `g' - 1
							
				loc e: word `e' of `ccentiles'
				loc f: word `f' of `ccentiles'
				loc g: word `g' of `ccentiles'
				
				qui centile `c' if (`b') & `genfil', centile(`e')
				
				loc h: dis %`q_decim_plus'.`q_decim'f r(c_1)
				if `"`ccentiles'"'!=`"`centiles'"' loc f: dis %`cc`e'_length`a''s "`f'`h'`g'"
				else loc f: dis %`ccc`e'_length`a''s "`f'`h'`g'"
				loc j "`j'`f'"
			}
		}
						
		if "`vvdrop'"=="on" loc vv "" //												ALLOW FOR THE drop(cn pc) OPTIONS
		if "`mmdrop'"=="on" loc mm ""
		if "`sddrop'"=="on" loc sd ""
		
		loc d: dis %`_ms`a''s "`mm'`sd'" //												means and standard deviations are always aligned
		
		if `"`ccentiles'"'==`"`centiles'"'{
			loc d: dis %~`_msmi`a''s "`d'" //											in this case, the combination of means and standard deviations is not aligned with the combination of medians and iqr, but must occupy the same space
			loc d: dis %`_msmiccc`a''s "`d'`j'" //										in this case, additional centiles of continuous variables are aligned with aditional centiles of nonmetric variables
			
		}
		else loc d: dis %`_mscc`a''s "`d'`j'" //										in this case, additional centiles of continuous variables are not aligned with aditional centiles of nonmetric variables
		
		loc d: dis %~`pretot`a''s "`d'" //												this combination is not aligned with counts and percentages, but must occupy the same space
		loc d: dis %`tot`a''s "`vv'`d'"
		
		qui replace `tablename'c`a' = "`d'" if `tablename'=="`c'" //					insert results into the proper row
	}			

	foreach c of local nonmetric{ //													ALLOW FOR THE NONMETRIC OPTION
		
		qui count if `c'<. & (`b') & `genfil' //										numbers of valid values
		loc d = r(N)
		loc vv: dis %`vv_length`a''s "`d'`vv2'" //										vv1 is the only before punctuation which is left aligned, except for opening parentheses
		loc vv: dis %`_vv`a''s "`vv1'`vv'"
		while "`d'"!="`vv'"{
			loc d "`vv'"
			loc vv: subinstr loc vv "( " " (", all
			loc vv: subinstr loc vv "[ " " [", all
		}
		
		qui centile `c' if (`b') & `genfil', centile(25 50 75)
		
		loc d: dis %`q_decim_plus'.`q_decim'f r(c_2) //									medians
		loc mq: dis %`mq_length`a''s "`mq1'`d'`mq2'"
		
		loc d: dis %`q_decim_plus'.`q_decim'f r(c_1) //									lower quartiles
		loc lq: dis %`lq_length`a''s "`lq1'`d'`lq2'"
		
		loc d: dis %`q_decim_plus'.`q_decim'f r(c_3) //									upper quartiles
		loc uq: dis %`uq_length`a''s "`uq1'`d'`uq2'"

		loc j "" //																		loc d: dis %`_msmiccc`a''s "`d'`j'"
		if `"`centiles'"'!=`""'{ //														ALLOW FOR THE CCENTILES OPTION
			
			loc d: word count `centiles'
			
			forvalues g = 3(3)`d'{

				loc e = `g' - 2  //														create macros numbered according to where they belong and containing the corresponding partition name
				loc f = `g' - 1
							
				loc e: word `e' of `centiles'
				loc f: word `f' of `centiles'
				loc g: word `g' of `centiles'
				
				qui centile `c' if (`b') & `genfil', centile(`e')
				
				loc h: dis %`q_decim_plus'.`q_decim'f r(c_1)
				if `"`ccentiles'"'!=`"`centiles'"' loc f: dis %`c`e'_length`a''s "`f'`h'`g'"
				else loc f: dis %`ccc`e'_length`a''s "`f'`h'`g'"
				loc j "`j'`f'"
			}
		}
		
		if "`vvdrop'"=="on" loc vv "" //												ALLOW FOR THE drop(vv mq iq) OPTIONS
		if "`mqdrop'"=="on" loc mq ""
		if "`iqdrop'"=="on"{
			loc lq ""
			loc uq ""
		}
		
		loc d: dis %`_mi`a''s "`mq'`lq'`uq'" //											medians and interquartile ranges are always aligned

		if `"`ccentiles'"'==`"`centiles'"'{
			loc d: dis %~`_msmi`a''s "`d'" //											in this case, the combination of medians and iqr is not aligned with the combination of means and standard deviations, but must occupy the same space
			loc d: dis %`_msmiccc`a''s "`d'`j'" //										in this case, additional centiles of nonmetric variables are aligned with aditional centiles of continuous variables
		}
		else loc d: dis %~`_mic`a''s "`d'`j'" //										in this case, additional centiles of nonmetric variables are not aligned with aditional centiles of continuous variables

		loc d: dis %~`pretot`a''s "`d'" //												this combination is not aligned with counts and percentages, but must occupy the same space
		loc d: dis %`tot`a''s "`vv'`d'"

		qui replace `tablename'c`a' = "`d'" if `tablename'=="`c'" //					insert results into the proper row
	}	
}	
*
																						if "$rafaeldebug"=="debug" dis as result "h32"
*!	DEFINE THE FORMAT OF THE STATISTICS OF THE DEFAULT 95% CONFIDENCE INTERVAL  !*

qui tab `groupdefiner' if `genfil'
loc a = r(r)
qui su `groupdefiner' if `genfil'

if `a'==2 & r(min)!=-99 & "`CIdrop'"!="on" {

	loc pdi_length = 0 //																percentage difference
	loc pll_length = 0 //																percentage lower limit
	loc pul_length = 0 //																percentage upper limit
		
	loc mdi_length = 0 //																mean difference
	loc mll_length = 0 //																mean lower limit
	loc mul_length = 0 //																mean upper limit

	qui count if `groupdefiner'!=. & `genfil' //										required for the formula
	loc n = r(N)
	
	foreach a in `binary' `_cat'{
																						if "$rafaeldebug"=="debug" dis as result "h32.3"			
		qui tab `groupdefiner' if `a'<. & `genfil'
		if r(r)<2 continue //															in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
		
		qui ttest `a' if `genfil', by(`groupdefiner')
		
		loc b: dis %`p_decim_plus'.`p_decim'f (r(mu_1)-r(mu_2))*100
		loc b "`pdi1'`b'`pdi2'"
		loc b: length loc b
		if `pdi_length'<`b' loc pdi_length = `b'
			
		loc b: dis %`p_decim_plus'.`p_decim'f ((r(mu_1)-r(mu_2)) - abs(r(se)*invttail(`n',1-(1-.95)/2)))*100
		if `b'>100 loc b: dis %`p_decim_plus'.`p_decim'f 100 //							consider that parametric confidence intervals of percentage differences may lead to strange results
		if `b'<-100 loc b: dis %`p_decim_plus'.`p_decim'f -100
		loc b "`pll1'`b'`pll2'"
		loc b: length loc b
		if `pll_length'<`b' loc pll_length = `b'
			
		loc b: dis %`p_decim_plus'.`p_decim'f ((r(mu_1)-r(mu_2)) + abs(r(se)*invttail(`n',1-(1-.95)/2)))*100
		if `b'>100 loc b: dis %`p_decim_plus'.`p_decim'f 100 //							consider that parametric confidence intervals of percentage differences may lead to strange results
		if `b'<-100 loc b: dis %`p_decim_plus'.`p_decim'f -100
		loc b "`pul1'`b'`pul2'"
		loc b: length loc b
		if `pul_length'<`b' loc pul_length = `b'
	}
	
	foreach a in `continuous' `nonmetric'{
																						if "$rafaeldebug"=="debug" dis as result "h32.6"			
		qui tab `groupdefiner' if `a'<. & `genfil'
		if r(r)<2 continue //															in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
		
		qui ttest `a' if `genfil', by(`groupdefiner')
		
		loc b: dis %`c_decim_plus'.`c_decim'f r(mu_1)-r(mu_2)
		loc b "`mdi1'`b'`mdi2'"
		loc b: length loc b
		if `mdi_length'<`b' loc mdi_length = `b'
			
		loc b: dis %`c_decim_plus'.`c_decim'f (r(mu_1)-r(mu_2)) - abs(r(se)*invttail(`n',1-(1-.95)/2))
		loc b "`mll1'`b'`mll2'"
		loc b: length loc b
		if `mll_length'<`b' loc mll_length = `b'
			
		loc b: dis %`c_decim_plus'.`c_decim'f (r(mu_1)-r(mu_2)) + abs(r(se)*invttail(`n',1-(1-.95)/2))
		loc b "`mul1'`b'`mul2'"
		loc b: length loc b
		if `mul_length'<`b' loc mul_length = `b'
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h33"
*!	GENERATE THE STATISTICS OF THE DEFAULT 95% CONFIDENCE INTERVAL  !*

loc a: word count `subgroups' //														the overlapping of two groups can result in one -99 group, one -99 group and one other group, or one -99 group and two other groups
loc b: subinstr loc errorcolumns "," " ", all
loc b: word count `b'
if "`errorcolumns'"==",0" loc b = 0
loc a = `a' - `b'
qui su `groupdefiner' if `genfil'

if `a'==2 & r(min)==-99 dis _newline as text "two subgroups found in '`tablename'', but the subgroups are not independent;" as result " default 95% confidence intervals not comuted"
if `a'==2 & r(min)!=-99 & "`CIdrop'"=="on" dis _newline as text "two independent subgroups found in '`tablename''; 'drop(deconf)' option specified;" as result " default 95% confidence intervals dropped"

if `a'==2 & r(min)!=-99 & "`CIdrop'"!="on" {
	
	dis _newline as text "two independent subgroups found in '`tablename'';" as result " default 95% confidence intervals computed"
	
	qui gen `tablename'ci = "Difference and 95% confidence interval" in 1
	if "`didrop'"=="on" qui replace `tablename'ci = "95% confidence interval" in 1
	if "`cidrop'"=="on" qui replace `tablename'ci = "Difference" in 1
	qui replace `tablename'ci = " " in 2/`row_number' //								to fit the requirements of the saveltablefile command, make sure that no cell of the table corresponds to missing
	
	qui count if `groupdefiner'!=. & `genfil' //										required for the formula
	loc n = r(N)
	
	foreach a in `binary' `_cat'{
																						if "$rafaeldebug"=="debug" dis as result "h33.3"			
		qui tab `groupdefiner' if `a'<. & `genfil'
		if r(r)==1{ //																	in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
			qui replace `tablename'ci = "one subgroup with valid values" if `tablename'=="`a'"
			continue
		}
		if r(r)==0{ 
			qui replace `tablename'ci = "no subgroups with valid values" if `tablename'=="`a'"
			continue
		}
				
		qui ttest `a' if `genfil', by(`groupdefiner')
		
		loc b: dis %`p_decim_plus'.`p_decim'f (r(mu_1)-r(mu_2))*100
		loc pdi: dis %`pdi_length's "`pdi1'`b'`pdi2'"
		
		loc b: dis %`p_decim_plus'.`p_decim'f ((r(mu_1)-r(mu_2)) - abs(r(se)*invttail(`n',1-(1-.95)/2)))*100
		if `b'>100 loc b: dis %`p_decim_plus'.`p_decim'f 100 //							consider that parametric confidence intervals of percentage differences may lead to strange results
		if `b'<-100 loc b: dis %`p_decim_plus'.`p_decim'f -100
		if trim("`pdi'")==trim("`pdi1'0`pdi2'") loc b "."
		loc pll: dis %`pll_length's "`pll1'`b'`pll2'"
					
		loc b: dis %`p_decim_plus'.`p_decim'f ((r(mu_1)-r(mu_2)) + abs(r(se)*invttail(`n',1-(1-.95)/2)))*100
		if `b'>100 loc b: dis %`p_decim_plus'.`p_decim'f 100 //							consider that parametric confidence intervals of percentage differences may lead to strange results
		if `b'<-100 loc b: dis %`p_decim_plus'.`p_decim'f -100
		if trim("`pdi'")==trim("`pdi1'0`pdi2'") loc b "."
		loc pul: dis %`pul_length's "`pul1'`b'`pul2'"
						
		if "`didrop'"=="on" loc pdi "" //												ALLOW FOR THE drop(di ci) OPTIONS
		if "`cidrop'"=="on"{
			loc pll ""
			loc pul ""
		}
		
		qui replace `tablename'ci = "`pdi'`pll'`pul'" if `tablename'=="`a'"
	}
	
	foreach a in `continuous' `nonmetric'{
																						if "$rafaeldebug"=="debug" dis as result "h33.6"			
		qui tab `groupdefiner' if `a'<. & `genfil'
		if r(r)==1{ //																	in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
			qui replace `tablename'ci = "one subgroup with valid values" if `tablename'=="`a'"
			continue
		}
		if r(r)==0{ 
			qui replace `tablename'ci = "no subgroups with valid values" if `tablename'=="`a'"
			continue
		}
		
		qui ttest `a' if `genfil', by(`groupdefiner')
		
		loc b: dis %`c_decim_plus'.`c_decim'f r(mu_1)-r(mu_2)
		loc mdi: dis %`mdi_length's "`mdi1'`b'`mdi2'"
		
		loc b: dis %`c_decim_plus'.`c_decim'f (r(mu_1)-r(mu_2)) - abs(r(se)*invttail(`n',1-(1-.95)/2))
		loc mll: dis %`mll_length's "`mll1'`b'`mll2'"
					
		loc b: dis %`c_decim_plus'.`c_decim'f (r(mu_1)-r(mu_2)) + abs(r(se)*invttail(`n',1-(1-.95)/2))
		loc mul: dis %`mul_length's "`mul1'`b'`mul2'"

		if "`didrop'"=="on" loc mdi "" //												ALLOW FOR THE drop(di ci) OPTIONS
		if "`cidrop'"=="on"{
			loc mll ""
			loc mul ""
		}
		
		qui replace `tablename'ci = "`mdi'`mll'`mul'" if `tablename'=="`a'"
	}
}
*	
																						if "$rafaeldebug"=="debug" dis as result "h34"
*!	GENERATE A PVALUE COLUMN BY DEFAULT IF THERE ARE MORE THAN ONE SUBGROUPS AND THE OPTION DROP(PVALUE) IS NOT SPECIFIED	!*

qui tab `groupdefiner' if `genfil'
loc a = r(r)
qui su `groupdefiner' if `genfil'
//																						at least two groups are required to produce an r(min) of -99
if r(min)==-99 dis _newline as text "two or more subgroups found in '`tablename'', but the subgroups are not independent;" as result " default p-values not comuted"
if `a'>=2 & r(min)!=-99 & "`Pdrop'"=="on" dis _newline as text "two or more independent subgroups found in '`tablename''; 'drop(depval)' option specified;" as result " p-values dropped"

if `a'>=2 & r(min)!=-99 & "`Pdrop'"!="on"{

	dis _newline as text "two or more independent subgroups found in '`tablename'';" as result " default p-values computed"

	qui gen `tablename'p = "p Value" in 1
	qui replace `tablename'p = " " in 2/`row_number' //									to fit the requirements of the saveltablefile command, make sure that no cell of the table corresponds to missing
	
	foreach a of local binary{ //														binary variables are computed by Fisher's exact test *
			
		qui tab `groupdefiner' if `a'<. & `genfil'
		if r(r)==1{ //																	in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
			qui replace `tablename'p = "one subgroup with valid values" if `tablename'=="`a'"
			continue
		}
		if r(r)==0{ 
			qui replace `tablename'p = "no subgroups with valid values" if `tablename'=="`a'"
			continue
		}
		
		qui tab `groupdefiner' if `a'<. & `genfil'
		if r(r)==2{ //																	* if there are two subgroups (four field tables)
			loc chi2: subinstr loc chi2 "`a'" "`a'", count(loc b) //					ALLOW FOR THE CHI2 OPTION
			if `b'>0{
				qui tab `a' `groupdefiner' if `genfil', chi
				loc c: dis %6.3f r(p)
			}
			else{
				qui tab `a' `groupdefiner' if `genfil', exact
				loc c: dis %6.3f r(p_exact)
			}
		}
		else{
			loc fisher: subinstr loc fisher "`a'" "`a'", count(loc b) //				ALLOW FOR THE FISHER OPTION
			if `b'>0{
				qui tab `a' `groupdefiner' if `genfil', exact
				loc c: dis %6.3f r(p_exact)
			}
			else{
				qui tab `a' `groupdefiner' if `genfil', chi
				loc c: dis %6.3f r(p)
			}
		}
		
		qui replace `tablename'p = "`c' " if `tablename'=="`a'"
		if "`c'"==" 0.000" qui replace `tablename'p = "<0.001 " if `tablename'=="`a'"
	}
		
	if "`podrop'"!="on"{ //																if overal pvalues of cat var are not dropped
		foreach a of local categorical{ //												overal pvalues of categorical variables are compared by chi2
			
			qui tab `groupdefiner' if `a'<. & `genfil'
			if r(r)==1{ //																in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
				qui replace `tablename'p = "one subgroup with valid values" if `tablename'=="`a'"
				continue
			}
			if r(r)==0{ 
				qui replace `tablename'p = "no subgroups with valid values" if `tablename'=="`a'"
				continue
			}
			
			loc fisher: subinstr loc fisher "`a'" "`a'", count(loc b) //				ALLOW FOR THE FISHER OPTION
			if `b'>0{
				qui tab `a' `groupdefiner' if `genfil', exact
				loc c: dis %6.3f r(p_exact)
			}
			else{
				qui tab `a' `groupdefiner' if `genfil', chi
				loc c: dis %6.3f r(p)
			}
			
			qui replace `tablename'p = "`c' " if `tablename'=="`a'"
			if "`c'"==" 0.000" qui replace `tablename'p = "<0.001 " if `tablename'=="`a'"
		}
	}
		
	if "`pidrop'"!="on"{ //																if individual pvalues of cat var are not dropped	
		foreach a of local _cat{ //														individual pvalues of categorical variables are computed by Fisher's exact test *
				
			qui tab `groupdefiner' if `a'<. & `genfil'
			if r(r)==1{ //																in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
				qui replace `tablename'p = "one subgroup with valid values" if `tablename'=="`a'"
				continue
			}
			if r(r)==0{ 
				qui replace `tablename'p = "no subgroups with valid values" if `tablename'=="`a'"
				continue
			}
			
			qui tab `groupdefiner' if `a'<. & `genfil'
			if r(r)==2{ //																* if there are two subgroups (four field tables)
				loc chi2: subinstr loc chi2 "`a'" "`a'", count(loc b) //				ALLOW FOR THE CHI2 OPTION
				if `b'>0{
					qui tab `a' `groupdefiner' if `genfil', chi
					loc c: dis %6.3f r(p)
				}
				else{
					qui tab `a' `groupdefiner' if `genfil', exact
					loc c: dis %6.3f r(p_exact)
				}
			}
			else{
				loc fisher: subinstr loc fisher "`a'" "`a'", count(loc b) //			ALLOW FOR THE FISHER OPTION
				if `b'>0{
					qui tab `a' `groupdefiner' if `genfil', exact
					loc c: dis %6.3f r(p_exact)
				}
				else{
					qui tab `a' `groupdefiner' if `genfil', chi
					loc c: dis %6.3f r(p)
				}
			}
			
			qui replace `tablename'p = " `c'" if `tablename'=="`a'"
			if "`c'"==" 0.000" qui replace `tablename'p = " <0.001" if `tablename'=="`a'"
		}
	}
	
	foreach a of local continuous{ //													continuous variables of independent subgroups are compared with oneway anova
			
		qui tab `groupdefiner' if `a'<. & `genfil'
		loc b = r(r) - 1
		if r(r)==1{ //																	in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
			qui replace `tablename'p = "one subgroup with valid values" if `tablename'=="`a'"
			continue
		}
		if r(r)==0{ 
			qui replace `tablename'p = "no subgroups with valid values" if `tablename'=="`a'"
			continue
		}
							
		loc kwallis: subinstr loc kwallis "`a'" "`a'", count(loc c) //					ALLOW FOR THE KWALLIS OPTION
		if `c'>0{
			qui kwallis `a' if `genfil', by(`groupdefiner')
			loc d: dis %6.3f chi2tail(`b',r(chi2))
		}
		else{
			qui anova `a' `groupdefiner' if `genfil'
			loc d: dis %6.3f 1-F(e(df_m),e(df_r),e(F))
		}
		
		qui replace `tablename'p = "`d' " if `tablename'=="`a'"
		if "`d'"==" 0.000" qui replace `tablename'p = "<0.001 " if `tablename'=="`a'"
	}
	
	foreach a of local nonmetric{ //													continuous variables of independent subgroups are compared with oneway anova
			
		qui tab `groupdefiner' if `a'<. & `genfil'
		loc b = r(r) - 1
		if r(r)==1{ //																	in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
			qui replace `tablename'p = "one subgroup with valid values" if `tablename'=="`a'"
			continue
		}
		if r(r)==0{ 
			qui replace `tablename'p = "no subgroups with valid values" if `tablename'=="`a'"
			continue
		}
		
		loc anova: subinstr loc anova "`a'" "`a'", count(loc c) //						ALLOW FOR THE ANOVA OPTION
		if `c'>0{
			qui anova `a' `groupdefiner' if `genfil'
			loc d: dis %6.3f 1-F(e(df_m),e(df_r),e(F))
		}
		else{
			qui kwallis `a' if `genfil', by(`groupdefiner')
			loc d: dis %6.3f chi2tail(`b',r(chi2))
		}
		
		qui replace `tablename'p = "`d' " if `tablename'=="`a'"
		if "`d'"==" 0.000" qui replace `tablename'p = "<0.001 " if `tablename'=="`a'"
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h35"
*! GENERATE THE COLUMNS FOR NON-DEFAULT CONFIDENCE INTERVALS AND CHECK THE CONFIDENCE INTERVALS EXTENDED OPTION !*

loc d: word count `confidence'

forvalues c = 3(3)`d'{  //																create macros numbered according to where they belong and containing the corresponding partition name
	
	loc a = `c' - 2
	loc b = `c' - 1
	loc e = (`c' / 3)
	
	loc group1: word `a' of `confidence'
	loc group2: word `b' of `confidence'
	loc confid: word `c' of `confidence'
	loc confid: subinstr loc confid "%" ""
	
	qui gen `tablename'ci`e' = "Difference and `confid'% CI for subgroups " in 1
	if "`didrop'"=="on" qui replace `tablename'ci`e' = "`confid'% CI for subgroups " in 1
	if "`cidrop'"=="on" qui replace `tablename'ci`e' = "Difference for subgroups " in 1
	qui replace `tablename'ci`e' = " " in 2/`row_number' //								to fit the requirements of the saveltablefile command, make sure that no cell of the table corresponds to missing
	
	capt conf integer n `group1'
	if _rc==7{
		qui replace `tablename'ci`e' = `tablename'ci`e' + "`group1' and `group2'" in 1
		qui replace `tablename'ci`e' = "Error: '`group1'' found where integer expected" in 3
		continue
	}
	capt conf integer n `group2'
	if _rc==7{
		qui replace `tablename'ci`e' = `tablename'ci`e' + "`group1' and `group2'" in 1
		qui replace `tablename'ci`e' = "Error: '`group2'' found where integer expected" in 3
		continue
	}
																						if "$rafaeldebug"=="debug" dis as result "h35.3"	
	loc fgroup = min(`group1',`group2')
	loc sgroup = max(`group1',`group2')
	
	qui replace `tablename'ci`e' = `tablename'ci`e' + "`fgroup' and `sgroup'" in 1
					
	loc f = `fgroup' + 1 //																the counting starts with the first subgroup
	capt conf str var `tablename'c`f'
	if _rc!=0{ //																		variable found (111), numeric found where string expected (7) invalid name (i.e. neg value; 198)
		qui replace `tablename'ci`e' = "Error: subgroup `fgroup' not found" in 3
		continue
	}
	loc g = `sgroup' + 1 //																the counting starts with the first subgroup
	capt conf str var `tablename'c`g'
	if _rc!=0{
		qui replace `tablename'ci`e' = "Error: subgroup `sgroup' not found" in 3
		continue
	}
	if inlist(`f'`errorcolumns') | inlist(`g'`errorcolumns'){
		qui replace `tablename'ci`e' = "Error: invalid subgroup defining expressions" in 3
		continue
	}
																						if "$rafaeldebug"=="debug" dis as result "h35.6"	
	capt conf integer n `confid'
	if _rc==7{
		qui replace `tablename'ci`e' = "Error: '`conf'' found where integer expected" in 3
		continue
	}
	if abs(`confid')!=`confid'{
		qui replace `tablename'ci`e' = "Error: 'negative confidences are not allowed" in 3
		continue
	}
	
	tempvar ci
	qui gen `ci' = .
	loc f = 0 //																		the counting starts with the first subgroup
	
	foreach group of local subgroups{
		
		loc f = `f' + 1
		loc g = `f' + 1
		
		if inlist(`g'`errorcolumns') continue
		
		qui replace `ci' = -99 if `ci'!=. & (`group') & `genfil'
		qui replace `ci' = `f' if `f'==`fgroup' & `ci'==. & (`group') & `genfil'
		qui replace `ci' = `f' if `f'==`sgroup' & `ci'==. & (`group') & `genfil'	
	}
	loc cilist "`cilist' `ci' `confid' `e'"
}
*
																						if "$rafaeldebug"=="debug" dis as result "h36"
*! FIND THE FORMAT OF THE NON-DEFAULT CONFIDENCE INTERVAL STATISTICS !*

loc a: word count `cilist'

forvalues d = 3(3)`a'{
	
	loc b = `d' - 2
	loc c = `d' - 1
	
	loc b: word `b' of `cilist'
	loc c: word `c' of `cilist'
	loc d: word `d' of `cilist'
	
	qui su `b' if `genfil'
	if r(min)==-99 continue

	loc pdi_length`d' = 0 //															percentage difference
	loc pll_length`d' = 0 //															percentage lower limit
	loc pul_length`d' = 0 //															percentage upper limit
		
	loc mdi_length`d' = 0 //															mean difference
	loc mll_length`d' = 0 //															mean lower limit
	loc mul_length`d' = 0 //															mean upper limit

	qui count if `b'!=. & `genfil' //													required for the formula
	loc n = r(N)
	
	foreach e in `binary' `_cat'{
																						if "$rafaeldebug"=="debug" dis as result "h36.3"			
		qui tab `b' if `e'<. & `genfil'
		if r(r)<2 continue //															in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
		
		qui ttest `e' if `genfil', by(`b')
		
		loc f: dis %`p_decim_plus'.`p_decim'f (r(mu_1)-r(mu_2))*100
		loc f "`pdi1'`f'`pdi2'"
		loc f: length loc f
		if `pdi_length`d''<`f' loc pdi_length`d' = `f'
			
		loc f: dis %`p_decim_plus'.`p_decim'f ((r(mu_1)-r(mu_2)) - abs(r(se)*invttail(`n',1-(1-.`c')/2)))*100
		if `f'>100 loc f: dis %`p_decim_plus'.`p_decim'f 100 //							consider that parametric confidence intervals of percentage differences may lead to strange results
		if `f'<-100 loc f: dis %`p_decim_plus'.`p_decim'f -100
		loc f "`pll1'`f'`pll2'"
		loc f: length loc f
		if `pll_length`d''<`f' loc pll_length`d' = `f'
			
		loc f: dis %`p_decim_plus'.`p_decim'f ((r(mu_1)-r(mu_2)) + abs(r(se)*invttail(`n',1-(1-.`c')/2)))*100
		if `f'>100 loc f: dis %`p_decim_plus'.`p_decim'f 100 //							consider that parametric confidence intervals of percentage differences may lead to strange results
		if `f'<-100 loc f: dis %`p_decim_plus'.`p_decim'f -100
		loc f "`pul1'`f'`pul2'"
		loc f: length loc f
		if `pul_length`d''<`f' loc pul_length`d' = `f'
	}
	
	foreach e in `continuous' `nonmetric'{
																						if "$rafaeldebug"=="debug" dis as result "h36.6"			
		qui tab `b' if `e'<. & `genfil'
		if r(r)<2 continue //															in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
		
		qui ttest `e' if `genfil', by(`b')
		
		loc f: dis %`c_decim_plus'.`c_decim'f r(mu_1)-r(mu_2)
		loc f "`mdi1'`f'`mdi2'"
		loc f: length loc f
		if `mdi_length`d''<`f' loc mdi_length`d' = `f'
			
		loc f: dis %`c_decim_plus'.`c_decim'f (r(mu_1)-r(mu_2)) - abs(r(se)*invttail(`n',1-(1-.`c')/2))
		loc f "`mll1'`f'`mll2'"
		loc f: length loc f
		if `mll_length`d''<`f' loc mll_length`d' = `f'
			
		loc f: dis %`c_decim_plus'.`c_decim'f (r(mu_1)-r(mu_2)) + abs(r(se)*invttail(`n',1-(1-.`c')/2))
		loc f "`mul1'`f'`mul2'"
		loc f: length loc f
		if `mul_length`d''<`f' loc mul_length`d' = `f'
	}
}	
*	
																						if "$rafaeldebug"=="debug" dis as result "h37"
*! GENERATE THE STATISTICS OF THE NON-DEFAULT CONFIDENCE INTERVALLS !*

loc a: word count `cilist'

forvalues d = 3(3)`a'{
	
	loc b = `d' - 2
	loc c = `d' - 1
	
	loc b: word `b' of `cilist'
	loc c: word `c' of `cilist'
	loc d: word `d' of `cilist'
	
	qui su `b' if `genfil'
	if r(min)==-99{
		qui replace `tablename'ci`d' = "not computed because the" in 3
		qui replace `tablename'ci`d' = "subgroups are not independent" in 4
		continue
	}
	
	qui count if `b'!=. & `genfil' //													required for the formula
	loc n = r(N)
	
	foreach e in `binary' `_cat'{
																						if "$rafaeldebug"=="debug" dis as result "h37.3"			
		qui tab `b' if `e'<. & `genfil'
		if r(r)==1{ //																	in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
			qui replace `tablename'ci`d' = "one subgroup with valid values" if `tablename'=="`e'"
			continue
		}
		if r(r)==0{ 
			qui replace `tablename'ci`d' = "no subgroups with valid values" if `tablename'=="`e'"
			continue
		}
				
		qui ttest `e' if `genfil', by(`b')
		
		loc f: dis %`p_decim_plus'.`p_decim'f (r(mu_1)-r(mu_2))*100
		loc pdi: dis %`pdi_length`d''s "`pdi1'`f'`pdi2'"
		
		loc f: dis %`p_decim_plus'.`p_decim'f ((r(mu_1)-r(mu_2)) - abs(r(se)*invttail(`n',1-(1-.`c')/2)))*100
		if `f'>100 loc f: dis %`p_decim_plus'.`p_decim'f 100 //							consider that parametric confidence intervals of percentage differences may lead to strange results
		if `f'<-100 loc f: dis %`p_decim_plus'.`p_decim'f -100
		if trim("`pdi'")==trim("`pdi1'0`pdi2'") loc f "."
		loc pll: dis %`pll_length`d''s "`pll1'`f'`pll2'"
					
		loc f: dis %`p_decim_plus'.`p_decim'f ((r(mu_1)-r(mu_2)) + abs(r(se)*invttail(`n',1-(1-.`c')/2)))*100
		if `f'>100 loc f: dis %`p_decim_plus'.`p_decim'f 100 //							consider that parametric confidence intervals of percentage differences may lead to strange results
		if `f'<-100 loc f: dis %`p_decim_plus'.`p_decim'f -100
		if trim("`pdi'")==trim("`pdi1'0`pdi2'") loc f "."
		loc pul: dis %`pul_length`d''s "`pul1'`f'`pul2'"
						
		if "`didrop'"=="on" loc pdi "" //												ALLOW FOR THE drop(di ci) OPTIONS
		if "`cidrop'"=="on"{
			loc pll ""
			loc pul ""
		}
		
		qui replace `tablename'ci`d' = "`pdi'`pll'`pul'" if `tablename'=="`e'"
	}
	
	foreach e in `continuous' `nonmetric'{
																						if "$rafaeldebug"=="debug" dis as result "h37.6"			
		qui tab `b' if `e'<. & `genfil'
		if r(r)==1{ //																	in case a variable should have only missings in one group (_rc==420) or even in both groups (_rc==2000)
			qui replace `tablename'ci`d' = "one subgroup with valid values" if `tablename'=="`e'"
			continue
		}
		if r(r)==0{ 
			qui replace `tablename'ci`d' = "no subgroups with valid values" if `tablename'=="`e'"
			continue
		}
		
		qui ttest `e' if `genfil', by(`b')
		
		loc f: dis %`c_decim_plus'.`c_decim'f r(mu_1)-r(mu_2)
		loc mdi: dis %`mdi_length`d''s "`mdi1'`f'`mdi2'"
		
		loc f: dis %`c_decim_plus'.`c_decim'f (r(mu_1)-r(mu_2)) - abs(r(se)*invttail(`n',1-(1-.`c')/2))
		loc mll: dis %`mll_length`d''s "`mll1'`f'`mll2'"
					
		loc f: dis %`c_decim_plus'.`c_decim'f (r(mu_1)-r(mu_2)) + abs(r(se)*invttail(`n',1-(1-.`c')/2))
		loc mul: dis %`mul_length`d''s "`mul1'`f'`mul2'"
						
		if "`didrop'"=="on" loc mdi "" //												ALLOW FOR THE drop(di ci) OPTIONS
		if "`cidrop'"=="on"{
			loc mll ""
			loc mul ""
		}
		
		qui replace `tablename'ci`d' = "`mdi'`mll'`mul'" if `tablename'=="`e'"
	}
}	
*	
																						if "$rafaeldebug"=="debug" dis as result "h38"
*!	RENAME THE ROW HEADERS IN THE FIRST (EXPLANATORY) COLUMN WITH THE VARIABLE LABELS ... 

foreach a of local order{ //															rename row headers with variable labels
	loc b: var label `a'
	qui replace `tablename' = "`b'" if `tablename'=="`a'"
}

* ... AND ADD THE LABELS OF THE DISPLAYED BINARY VALUES	!*

if "`binary'"!="" dis ""

foreach a of local binary{ //															add the label of the displayed binary value
	
	loc lowerbin: subinstr loc lowerbin "`a'" "`a'", word count(loc b)
	
	qui su `a'
	
	if r(min)==r(max){ //																if only one value is found in the data, try to determine the higher and the lower value by the label
		loc c: val label `a'
		if "`c'"!="" qui lab list `c'
	}
																						if "$rafaeldebug"=="debug" dis as result "h38.5"
	if `b'!=0 loc b = r(min)
	else loc b = r(max)
	loc c: var label `a' //																retrieve varlabel - cannot be missing (see below)
	loc d = trim("`c'")
	loc e: label (`a') `b', strict //													retrieve vallabel

	if "`e'"!=""{ //																	if any, add to first column if first column==varlabel | (varlabel=="" & first column=="var")
		dis as result "the category of the binary variable '`d'' displayed in '`tablename'' is '`e''"
		if "`bindrop'"!="on" qui replace `tablename' = `tablename' + " (`e')" if `tablename'=="`c'" // ("`c'"!="" & `tablename'=="`c'") | ("`c'"=="" & `tablename'=="`a'")
	}
	else{
		dis as result "the value of the binary variable '`d'' displayed in '`tablename'' is '`b''"
		if "`bindrop'"!="on" & "`numdrop'"!="on" qui replace `tablename' = `tablename' + " (`b')" if `tablename'=="`c'" // see below, not necessary: ("`c'"!="" & `tablename'=="`c'") | ("`c'"=="" & `tablename'=="`a'")
	}
}
*
																						if "$rafaeldebug"=="debug" dis as result "h39"
*! RESTORE CHANGES PERFORMED TO THE DATA SET: DEINDENT THE INDENTED LABELS, ...

foreach a of local indented{
	loc b: var label `a'
	loc b: subinstr loc b " " ""
	lab var `a' "`b'"
}
																						if "$rafaeldebug"=="debug" dis as result "h39.5"
* ... DELABEL INITIALLY NON-LABELLED VARIABLES, ...

foreach a of local novarlab{ //															for simplicity, all non-labelled variables were labelled with their own varname
	lab var `a'
}

* ... AND REDROP VALUE LABELS WHICH WERE ASSIGNED TO VARIABLES DESPITE NOT HAVING BEEN DEFINED !* 

if "`labdrop'"!="" lab drop `labdrop' //												avoids putting a cap before each lab list while keeping previous data unchanged
*

*! IF CHANGES WERE PERFORMED TO THE DATA SET WHICH CANNOT BE RESTORED (NAMELY ADDING OBSERVATIONS), REMEMBER THEM !*

if `"`remember1'"'!=`""'{
	dis _newline(2) `remember1'
	dis _newline in red upper("`remember2'") as result ""
}
if "`remember3'"!="" dis _newline(2) in red upper("`remember3'") as result ""
dis "."
end
*

******************************************************************************************************************************************************************************************************************************************
