cd "/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/"

use "Organized data/snack_nutri_hcru_cov_xx.dta",clear


**# 1. Impute missing data
keep if fu_year==0
keep lopnr-smell_taste_imp
misstable summarize

**# 1.1. Impute missing data in baseline covariates
mi set wide
mi register imp education tobacco1 mna_screening1_cat 

mi impute chained (ologit,augment) education mna_screening1_cat tobacco1 = age1 sex lives1 mi hf stroke copd ra_op cancer depression pd_dem, add(20) rseed(2026) augment force

* create dummy variables for at risk and malnourished to address problem of few individuals for combinations of mna_screening1_cat and fu_year
mi passive: gen mna_at_risk=mna_screening1_cat==2
mi passive: gen mna_malnourished=mna_screening1_cat==3



**# 1.2. Merge imputed baseline covariates with outcome data
merge 1:m lopnr using snack_nutri_hcru_cov_xx.dta, nogen

* fu_year>=11 is sparse, merge fu_year>=11 to a single category
replace fu_year=11 if fu_year>=11

misstable summarize


**# 2. Two-part models with multiple imputation
**# 2.1. Write a program to automate the process
capture program drop twopm_model
program define twopm_model, eclass

    syntax varname [if] [in], COV1(string) COV2(string)

    local depvar `varlist'
    local cond "`if' `in'"
    
    mi estimate, cmdok: twopm (`depvar' = i.mna_at_risk i.mna_malnourished i.fu_year `cov1') (`depvar' = i.mna_at_risk##i.fu_year i.mna_malnourished `cov2') `cond', firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)

end



**# 2.2. Model 1: adjust for age, sex, fu_year, and death in year
* total costs
* drop records not having total costs due to PV data restriction
twopm_model total_cost_trim if total_cost_trim!=., cov1(age1 i.sex i.pv_death_in_year) cov2(age1 i.sex i.pv_death_in_year)
estimates store total_costs_mod1

* PV costs
* drop records not having PV costs due to PV data restriction
twopm_model pv_cost_trim if pv_cost_trim!=., cov1(age1 i.sex i.pv_death_in_year) cov2(age1 i.sex i.pv_death_in_year)
estimates store pv_costs_mod1

* SV costs
twopm_model sv_cost_trim, cov1(age1 i.sex i.death_in_year) cov2(age1 i.sex i.death_in_year)
estimates store sv_costs_mod1

* OV costs
twopm_model ov_cost_trim, cov1(age1 i.sex i.death_in_year) cov2(age1 i.sex i.death_in_year)
estimates store ov_costs_mod1



**# 2.3. Model 2: additionally adjust for tobacco1 lives1 mi hf ra_op cancer depression pd_dem
* total costs
* drop records not having total costs due to PV data restriction
twopm_model total_cost_trim if total_cost_trim!=., cov1(age1 i.sex i.pv_death_in_year i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem) cov2(age1 i.sex i.pv_death_in_year i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem)
estimates store total_costs_mod2

* PV costs
* drop records not having PV costs due to PV data restriction
twopm_model pv_cost_trim if pv_cost_trim!=., cov1(age1 i.sex i.pv_death_in_year i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem) cov2(age1 i.sex i.pv_death_in_year i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem)
estimates store pv_costs_mod2

* SV costs
twopm_model sv_cost_trim, cov1(age1 i.sex i.death_in_year i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem) cov2(age1 i.sex i.death_in_year i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem)
estimates store sv_costs_mod2

* OV costs
twopm_model ov_cost_trim, cov1(age1 i.sex i.death_in_year i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem) cov2(age1 i.sex i.death_in_year i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem)
estimates store ov_costs_mod2






**# 3. Margins estimation
**# 3.1. Costs by malnutriton status
* write a program to automate the process for estimating marginal estimates
capture program drop margin_cost
program define margin_cost, eclass properties(mi)

    syntax varname [if] [in], COV1(string) COV2(string)

    twopm (`varlist' = i.mna_at_risk i.mna_malnourished i.fu_year `cov1') (`varlist' = i.mna_at_risk##i.fu_year i.mna_malnourished `cov2') `if' `in', firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)
	
	margins, at(mna_at_risk=0 mna_malnourished=0) at(mna_at_risk=1 mna_malnourished=0) at(mna_at_risk=0 mna_malnourished=1) post
	
end

* Model 1: adjust for age, sex, fu_year, and death in year
* Model 2: additionally adjust for tobacco1 lives1 mi hf ra_op cancer depression pd_dem

* total costs
* drop records not having total costs due to PV data restriction
mi estimate: margin_cost total_cost_trim if total_cost_trim!=., cov1(age1 i.sex i.pv_death_in_year) cov2(age1 i.sex i.pv_death_in_year)
estimates store total_cost_margins_mod1

mi estimate: margin_cost total_cost_trim if total_cost_trim!=., cov1(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) cov2(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year)
estimates store total_cost_margins_mod2


* PV costs
* drop records not having PV costs due to PV data restriction
mi estimate: margin_cost pv_cost_trim if pv_cost_trim!=., cov1(age1 i.sex i.pv_death_in_year) cov2(age1 i.sex i.pv_death_in_year)
estimates store pv_cost_margins_mod1

mi estimate: margin_cost pv_cost_trim if pv_cost_trim!=., cov1(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) cov2(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year)
estimates store pv_cost_margins_mod2


* SV costs
mi estimate: margin_cost sv_cost_trim, cov1(age1 i.sex i.death_in_year) cov2(age1 i.sex i.death_in_year)
estimates store sv_cost_margins_mod1

mi estimate: margin_cost sv_cost_trim, cov1(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.death_in_year) cov2(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.death_in_year)
estimates store sv_cost_margins_mod2


* OV costs
mi estimate: margin_cost ov_cost_trim, cov1(age1 i.sex i.death_in_year) cov2(age1 i.sex i.death_in_year)
estimates store ov_cost_margins_mod1

mi estimate: margin_cost ov_cost_trim, cov1(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.death_in_year) cov2(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.death_in_year)
estimates store ov_cost_margins_mod2



**# 3.2. Marginal differences in costs by malnutriton status
* write a program to automate the process for estimating marginal differences
capture program drop margin_costdiff
program define margin_costdiff, eclass properties(mi)

    syntax varname [if] [in], COV1(string) COV2(string)

    twopm (`varlist' = i.mna_at_risk i.mna_malnourished i.fu_year `cov1') (`varlist' = i.mna_at_risk##i.fu_year i.mna_malnourished `cov2') `if' `in', firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)

    margins, at(mna_at_risk=0 mna_malnourished=0) at(mna_at_risk=1 mna_malnourished=0) at(mna_at_risk=0 mna_malnourished=1) contrast(atcontrast(r)) post

end

* Model 1: adjust for age, sex, fu_year, and death in year
* Model 2: additionally adjust for tobacco1 lives1 mi hf ra_op cancer depression pd_dem

* total costs
* drop records not having total costs due to PV data restriction
mi estimate: margin_costdiff total_cost_trim if total_cost_trim!=., cov1(age1 i.sex i.pv_death_in_year) cov2(age1 i.sex i.pv_death_in_year)
estimates store total_costdiff_margins_mod1

mi estimate: margin_costdiff total_cost_trim if total_cost_trim!=., cov1(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) cov2(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year)
estimates store total_costdiff_margins_mod2


* PV costs
* drop records not having PV costs due to PV data restriction
mi estimate: margin_costdiff pv_cost_trim if pv_cost_trim!=., cov1(age1 i.sex i.pv_death_in_year) cov2(age1 i.sex i.pv_death_in_year)
estimates store pv_costdiff_margins_mod1

mi estimate: margin_costdiff pv_cost_trim if pv_cost_trim!=., cov1(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) cov2(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year)
estimates store pv_costdiff_margins_mod2


* SV costs
mi estimate: margin_costdiff sv_cost_trim, cov1(age1 i.sex i.death_in_year) cov2(age1 i.sex i.death_in_year)
estimates store sv_costdiff_margins_mod1

mi estimate: margin_costdiff sv_cost_trim, cov1(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.death_in_year) cov2(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.death_in_year)
estimates store sv_costdiff_margins_mod2


* OV costs
mi estimate: margin_costdiff ov_cost_trim, cov1(age1 i.sex i.death_in_year) cov2(age1 i.sex i.death_in_year)
estimates store ov_costdiff_margins_mod1

mi estimate: margin_costdiff ov_cost_trim, cov1(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.death_in_year) cov2(age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.death_in_year)
estimates store ov_costdiff_margins_mod2



**# 4. Export results to excel 
**# 4.1. Export model coefficients
local model total_costs_mod1 total_costs_mod2 pv_costs_mod1 pv_costs_mod2 sv_costs_mod1 sv_costs_mod2 ov_costs_mod1 ov_costs_mod2

foreach m of local model{
	
	local modelname="`m'"
	capture frame drop mod_coef
	frame create mod_coef str30 colname str30 coef str30 pv

estimates restore `m'
estimates replay `m'
matrix list r(table)

local col_names: colfullnames r(table)
local ncols=colsof(r(table))

forvalues c=1/`ncols'{ 

    local colname: word `c' of `col_names'

    local b=r(table)[1,`c']
    local lb=r(table)[5,`c'] 
    local ub=r(table)[6,`c']
	
	if strpos("`colname'", "logit") > 0 {
        local coef=string(exp(`b'), "%9.2f")
        local lb_s=string(exp(`lb'), "%9.2f")
        local ub_s=string(exp(`ub'), "%9.2f")
    }
    else {
        local coef=string(`b', "%9.0f")
        local lb_s=string(`lb', "%9.0f")
        local ub_s=string(`ub', "%9.0f")
    }
	
    local pv=string(r(table)[4,`c'], "%9.3f") 
    
    frame post mod_coef ("`colname'") ("`coef' (`lb_s'-`ub_s')") ("`pv'")
}

frame mod_coef: drop if pv == "."
frame mod_coef: drop if strpos(colname, "_cons") > 0
frame mod_coef: gen str20 part = ""
frame mod_coef: gen str40 cov = ""
frame mod_coef: split colname, parse(":") gen(tmp)
frame mod_coef: replace part = tmp1
frame mod_coef: replace cov = tmp2
frame mod_coef: drop tmp1 tmp2 colname
frame mod_coef: order part cov coef pv
frame mod_coef: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/snack_model_output_xx.xlsx", firstrow(varlabels) sheet("`modelname'",replace)
}




**# 4.2. Export margins estimates
capture frame drop med_costs
frame create med_costs str30 hcru_mod str30 mna str30 cost str30 pv

local out total_cost_margins_mod1 total_cost_margins_mod2 pv_cost_margins_mod1 pv_cost_margins_mod2 sv_cost_margins_mod1 sv_cost_margins_mod2 ov_cost_margins_mod1 ov_cost_margins_mod2

foreach var of local out{
	
estimates restore `var'
estimates replay `var'

forvalues i=1/3{ 
	local cost=string(r(table)[1,`i'], "%9.0f") 
	local lb=string(r(table)[5,`i'], "%9.0f") 
	local ub=string(r(table)[6,`i'], "%9.0f") 
	local pv=string(r(table)[4,`i'], "%9.3f") 
	
	frame post med_costs ("`var'") ("mna`i'") ( "`cost' (`lb',`ub')") ("`pv'")
	}
}


local outdiff total_costdiff_margins_mod1 total_costdiff_margins_mod2 pv_costdiff_margins_mod1 pv_costdiff_margins_mod2 sv_costdiff_margins_mod1 sv_costdiff_margins_mod2 ov_costdiff_margins_mod1 ov_costdiff_margins_mod2

foreach var of local outdiff{
	
estimates restore `var'
estimates replay `var'

forvalues i=1/2{ 
	local cost=string(r(table)[1,`i'], "%9.0f") 
	local lb=string(r(table)[5,`i'], "%9.0f") 
	local ub=string(r(table)[6,`i'], "%9.0f") 
	local pv=string(r(table)[4,`i'], "%9.3f") 
	local j=`i'+1
	
	frame post med_costs ("`var'") ("mna`j'") ( "`cost' (`lb',`ub')") ("`pv'")
	}
}



frame med_costs: gen model = substr(hcru_mod, strrpos(hcru_mod, "_") + 1, .)
frame med_costs: gen hcru = substr(hcru_mod, 1, strrpos(hcru_mod, "_") - 1)
frame med_costs: drop hcru_mod
frame med_costs: order hcru model mna cost pv

frame med_costs: list


frame med_costs: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/snack_margins_xx.xlsx", firstrow(varlabels) sheet("costs",replace)

