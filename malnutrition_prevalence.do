cd "/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/"

*cd "C:\Users\xinxia\OneDrive - Karolinska Institutet\Malnutrition project data analysis\Data\" 



**# 1. SNAC-K
use "Organized data/snack_nutri_hcru_cov_xx.dta",clear
keep if fu_year==0
keep lopnr-smell_taste_imp death fu

gen age_group=1
replace age_group=2 if age1>=78

misstable summarize

mi set wide
mi register imp education tobacco1 mna_screening1_cat 

mi impute chained (ologit,augment) education mna_screening1_cat tobacco1 = age1 sex lives1 mi hf stroke pd_dem copd ra_op cancer depression smell_taste_imp chew_imp poor_appetite, add(20) rseed(2026) augment force


capture frame drop mna_snack
frame create mna_snack str30 cond str30 prev_ci str30 est str30 lb str30 ub

* overall prevalence
mi estimate: proportion mna_screening1_cat

local col_names: colfullnames r(table)
local ncols = colsof(r(table))
	
forvalues c=1/`ncols' {	
		
		local est=string(r(table)[1,`c']*100, "%9.1f") 
		local lb=string(r(table)[5,`c']*100, "%9.1f") 
		local ub=string(r(table)[6,`c']*100, "%9.1f") 
		
		local colname: word `=`c'+1' of local `col_names'
		
	frame post mna_snack ("`colname'") ("`est' (`lb'-`ub')") ("`est'") ("`lb'") ("`ub'")

}


* prevalence by age groups
mi estimate: proportion mna_screening1_cat, over(age_group)


local col_names: colfullnames r(table)
local ncols = colsof(r(table))
	
forvalues c=1/`ncols' {	
		
		local est=string(r(table)[1,`c']*100, "%9.1f") 
		local lb=string(r(table)[5,`c']*100, "%9.1f") 
		local ub=string(r(table)[6,`c']*100, "%9.1f") 
		
		local colname: word `=`c'+1' of local `col_names'
		
	frame post mna_snack ("`colname'") ("`est' (`lb'-`ub')") ("`est'") ("`lb'") ("`ub'")

}

frame mna_snack: list


frame mna_snack: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/malnutrition_prevalence_xx.xlsx", firstrow(varlabels) sheet("snack",replace)



**# 2. SLSO
use "Organized data/slso_nutri_hcru_cov_xx.dta",clear

keep lopnr-dem death_180d

gen age_group=1 
replace age_group=2 if age>=70&age<80
replace age_group=3 if age>=80

misstable summarize

mi set wide
mi register imp edu_level mna_cat ensamboende

mi impute chained (ologit,augment) edu_level mna_cat (logit,augment) ensamboende = age sex ihd hf stroke pd dem copd ra op cancer depression, add(20) rseed(2026) augment force

capture frame drop mna_slso
frame create mna_slso str30 cond str30 prev_ci str30 est str30 lb str30 ub

* overall prevalence
mi estimate: proportion mna_cat

local col_names: colfullnames r(table)
local ncols = colsof(r(table))
	
forvalues c=1/`ncols' {	
		
		local est=string(r(table)[1,`c']*100, "%9.1f") 
		local lb=string(r(table)[5,`c']*100, "%9.1f") 
		local ub=string(r(table)[6,`c']*100, "%9.1f") 
		
		local colname: word `=`c'+1' of local `col_names'
		
	frame post mna_slso ("`colname'") ("`est' (`lb'-`ub')") ("`est'") ("`lb'") ("`ub'")

}


* prevalence by age groups
mi estimate: proportion mna_cat, over(age_group)


local col_names: colfullnames r(table)
local ncols = colsof(r(table))
	
forvalues c=1/`ncols' {	
		
		local est=string(r(table)[1,`c']*100, "%9.1f") 
		local lb=string(r(table)[5,`c']*100, "%9.1f") 
		local ub=string(r(table)[6,`c']*100, "%9.1f") 
		
		local colname: word `=`c'+1' of local `col_names'
		
	frame post mna_slso ("`colname'") ("`est' (`lb'-`ub')") ("`est'") ("`lb'") ("`ub'")

}

frame mna_slso: list


frame mna_slso: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/malnutrition_prevalence_xx.xlsx", firstrow(varlabels) sheet("slso",replace)





**# 3. SveDem
use "Organized data/svedem_nutri_hcru_cov_xx.dta",clear

keep if fu_year_start==0

* replace emptiness in state_base with missing
gen dem_state=.
replace dem_state=1 if state_base=="Very mild"
replace dem_state=2 if state_base=="Mild"
replace dem_state=3 if state_base=="Moderate"
replace dem_state=4 if state_base=="Severe"

gen sex=1 if SEX=="FEMALE"
replace sex=2 if SEX=="MALE"

gen inst=.
replace inst=0 if sabo==1
replace inst=1 if sabo==2

label drop malnut
replace malnut=malnut-1


mi set wide
mi register imp dem_state malnut inst

mi impute chained (mlogit,augment) dem_state (logit,augment) malnut inst = diaggrp age sex mi hf stroke copd ra op cancer depression, add(20) rseed(2026) augment force



capture frame drop malnut_svedem
frame create malnut_svedem str30 cond str30 prev_ci str30 est str30 lb str30 ub

* overall prevalence
mi estimate: proportion malnut

local col_names: colfullnames r(table)
local ncols = colsof(r(table))
	
forvalues c=1/`ncols' {	
		
		local est=string(r(table)[1,`c']*100, "%9.1f") 
		local lb=string(r(table)[5,`c']*100, "%9.1f") 
		local ub=string(r(table)[6,`c']*100, "%9.1f") 
		
		local colname: word `=`c'+1' of local `col_names'
		
	frame post malnut_svedem ("`colname'") ("`est' (`lb'-`ub')") ("`est'") ("`lb'") ("`ub'")

}


* prevalence by age groups
mi estimate: proportion malnut, over(age_group)


local col_names: colfullnames r(table)
local ncols = colsof(r(table))
	
forvalues c=1/`ncols' {	
		
		local est=string(r(table)[1,`c']*100, "%9.1f") 
		local lb=string(r(table)[5,`c']*100, "%9.1f") 
		local ub=string(r(table)[6,`c']*100, "%9.1f") 
		
		local colname: word `=`c'+1' of local `col_names'
		
	frame post malnut_svedem ("`colname'") ("`est' (`lb'-`ub')") ("`est'") ("`lb'") ("`ub'")

}

frame malnut_svedem: list


frame malnut_svedem: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/malnutrition_prevalence_xx.xlsx", firstrow(varlabels) sheet("svedem",replace)
