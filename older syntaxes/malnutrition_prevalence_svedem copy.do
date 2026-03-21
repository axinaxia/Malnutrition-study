*cd "/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/"

cd "C:\Users\xinxia\OneDrive - Karolinska Institutet\Malnutrition project data analysis\Data\Malnutrition statistical analysis\Malnutrition-study\" 






**# 1. Malnutrition
use "svedem_nutri_hcru_cov_xx.dta",clear

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

**# 1.1 Impute missing data in malnutrition
mi set wide
mi register imp dem_state malnut inst

mi impute chained (mlogit,augment) dem_state (logit,augment) malnut inst = diaggrp age sex mi hf stroke copd ra op cancer depression, add(20) rseed(2026) augment force



**# 1.2. Calculate malnut proportion 
capture frame drop malnut_ageg
frame create malnut_ageg str30 cond str30 prev_ci str30 est str30 lb str30 ub


mi estimate: proportion malnut, over(age_group)


local col_names: colfullnames r(table)
local ncols = colsof(r(table))
	
forvalues c=1/`ncols' {	
		
		local est=string(r(table)[1,`c']*100, "%9.1f") 
		local lb=string(r(table)[5,`c']*100, "%9.1f") 
		local ub=string(r(table)[6,`c']*100, "%9.1f") 
		
		local colname: word `=`c'+1' of local `col_names'
		
	frame post malnut_ageg ("`colname'") ("`est' (`lb'-`ub')") ("`est'") ("`lb'") ("`ub'")

}

frame malnut_ageg: list


mi estimate: proportion malnut

local col_names: colfullnames r(table)
local ncols = colsof(r(table))
	
forvalues c=1/`ncols' {	
		
		local est=string(r(table)[1,`c']*100, "%9.1f") 
		local lb=string(r(table)[5,`c']*100, "%9.1f") 
		local ub=string(r(table)[6,`c']*100, "%9.1f") 
		
		local colname: word `=`c'+1' of local `col_names'
		
	frame post malnut_ageg ("`colname'") ("`est' (`lb'-`ub')") ("`est'") ("`lb'") ("`ub'")

}

frame malnut_ageg: list
frame malnut_ageg: export excel using "results/svedem_malnutrition_prevalence_xx.xlsx", firstrow(varlabels) sheet("malnut_prev",replace)


**# 1.3. Analyse each variable's association with malnutrition
capture frame drop mal_pred
frame create mal_pred str30 pred str30 or_ci str30 pvalue 

local pred inst mi hf stroke copd ra op cancer depression

foreach p of local pred{
		
mi estimate: logit malnut i.`p' age i.sex

matrix list r(table)

local b1=string(exp(el(r(table),1,2)), "%9.2f") 
local b2=string(exp(el(r(table),5,2)), "%9.2f") 
local b3=string(exp(el(r(table),6,2)), "%9.2f") 
local pvalue=string(el(r(table),4,2), "%9.3f") 

frame post mal_pred ("`p'") ( "`b1' (`b2'-`b3')") ("`pvalue'")
} 

frame mal_pred:list 


forvalues c=1/10{
	local lbl : label (diaggrp) `c'
		
mi estimate: logit malnut i.diaggrp age i.sex

matrix list r(table)

local b1=string(exp(el(r(table),1,`c')), "%9.2f") 
local b2=string(exp(el(r(table),5,`c')), "%9.2f") 
local b3=string(exp(el(r(table),6,`c')), "%9.2f") 
local pvalue=string(el(r(table),4,`c'), "%9.3f") 

frame post mal_pred ("`lbl'") ( "`b1' (`b2'-`b3')") ("`pvalue'")
} 

frame mal_pred:list 
frame mal_pred: export excel using "results/svedem_malnutrition_prevalence_xx.xlsx", firstrow(varlabels) sheet("malnut_pred",replace)







* 2. BMI categories
use "svedem_nutri_hcru_cov_xx.dta",clear

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


**# 2.1 Impute missing data in BMI
mi set wide
mi register imputed bmi_cat dem_state inst

mi impute chained (mlogit,augment) bmi_cat dem_state (logit,augment) inst = diaggrp age sex mi hf stroke copd ra op cancer depression, add(20) rseed(2026) augment force

mi passive: gen malnutrition=(bmi_cat == 2)


**# 2.2. Calculate BMI status proportion 
capture frame drop bmi_ageg
frame create bmi_ageg str30 cond str30 prev_ci str30 est str30 lb str30 ub


mi estimate: proportion bmi_cat, over(age_group)


local col_names: colfullnames r(table)
local ncols = colsof(r(table))
	
forvalues c=1/`ncols' {	
		
		local est=string(r(table)[1,`c']*100, "%9.1f") 
		local lb=string(r(table)[5,`c']*100, "%9.1f") 
		local ub=string(r(table)[6,`c']*100, "%9.1f") 
		
		local colname: word `=`c'+1' of local `col_names'
		
	frame post bmi_ageg ("`colname'") ("`est' (`lb'-`ub')") ("`est'") ("`lb'") ("`ub'")

}

frame bmi_ageg: list


mi estimate: proportion bmi_cat

local col_names: colfullnames r(table)
local ncols = colsof(r(table))
	
forvalues c=1/`ncols' {	
		
		local est=string(r(table)[1,`c']*100, "%9.1f") 
		local lb=string(r(table)[5,`c']*100, "%9.1f") 
		local ub=string(r(table)[6,`c']*100, "%9.1f") 
		
		local colname: word `=`c'+1' of local `col_names'
		
	frame post bmi_ageg ("`colname'") ("`est' (`lb'-`ub')") ("`est'") ("`lb'") ("`ub'")

}

frame bmi_ageg: list
frame bmi_ageg: export excel using "results/svedem_malnutrition_prevalence_xx.xlsx", firstrow(varlabels) sheet("bmi_prev",replace)



