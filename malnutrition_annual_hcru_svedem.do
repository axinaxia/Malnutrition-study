*cd "/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/"

cd "C:\Users\xinxia\OneDrive - Karolinska Institutet\Malnutrition project data analysis\Data" 

use "Organized data\svedem_nutri_hcru_cov_xx.dta",clear

gen sex=0
replace sex=1 if SEX=="FEMALE"

gen state_base_num=1 if state_base=="Very mild"
replace state_base_num=2 if state_base=="Mild"
replace state_base_num=3 if state_base=="Moderate"
replace state_base_num=4 if state_base=="Severe"
replace state_base_num=. if state_base==""

drop state_base
rename state_base_num state_base

label drop sabo
replace sabo=sabo-1

label drop malnut
replace malnut=malnut-1

**# 1. Impute missing data
keep if fu_year_start==0
keep LopNr diaggrp malnut sabo age state_base sex mi-depression
misstable summarize

**# 1.1. Impute missing data in baseline covariates
mi set wide
mi register imp state_base malnut sabo

mi impute chained (mlogit,augment) state_base (logit,augment) malnut sabo = diaggrp age sex mi hf stroke copd ra op cancer depression, add(20) rseed(2026) augment force


**# 1.2. Merge imputed baseline covariates with outcome data
merge 1:m LopNr using svedem_nutri_hcru_cov_xx.dta, keepusing(fu_year_start fu_year_end death_in_year drug_costs_trim ov_costs_trim sv_costs_trim sol_costs_trim total_costs_trim) nogen

misstable summarize


**# 2. Two-part models with multiple imputation
**# 2.1. Write a program to automate the process
capture program drop twopm_model
program define twopm_model, eclass

    syntax varname, COV1(string) COV2(string)

    local depvar `varlist'
    
    mi estimate, cmdok: twopm (`depvar' = i.malnut i.fu_year_start `cov1') (`depvar' = i.malnut##i.fu_year_start `cov2'), firstpart(logit) secondpart(glm, family(gaussian) link(identity)) vce(cluster LopNr)

end



**# 2.2. Model 1: adjust for age, sex, diaggrp, fu_year_start, and year in death
* total costs
twopm_model total_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store total_costs_mod1

* drug costs
twopm_model drug_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store drug_costs_mod1

* SV costs
twopm_model sv_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store sv_costs_mod1

* OV costs
twopm_model ov_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store ov_costs_mod1

* social care costs
twopm_model sol_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store sol_costs_mod1


**# 2.3. Model 2: additionally adjust for state_base mi hf stroke copd ra op cancer depression
* total costs
twopm_model total_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store total_costs_mod2

* drug costs
twopm_model drug_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store drug_costs_mod2

* SV costs
twopm_model sv_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store sv_costs_mod2

* OV costs
twopm_model ov_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store ov_costs_mod2

* social care costs
twopm_model sol_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store sol_costs_mod2





**# 3. Margins estimation
**# 3.1. Costs by malnutriton status
* write a program to automate the process for estimating marginal estimates
capture program drop margin_cost
program define margin_cost, eclass properties(mi)

    syntax varname, COV1(string) COV2(string)

    twopm (`varlist' = i.malnut i.fu_year_start `cov1') (`varlist' = i.malnut##i.fu_year_start `cov2'), firstpart(logit) secondpart(glm, family(gaussian) link(identity)) vce(cluster LopNr)
	
	margins, at(malnut=0) at(malnut=1) post
	
end

* Model 1: adjust for age, sex, diaggrp, fu_year_start, and year in death
* Model 2: additionally adjust for state_base mi hf stroke copd ra op cancer 

* total costs
mi estimate: margin_cost total_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store total_cost_margins_mod1

mi estimate: margin_cost total_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store total_cost_margins_mod2


* drug costs
mi estimate: margin_cost drug_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store drug_cost_margins_mod1

mi estimate: margin_cost drug_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store drug_cost_margins_mod2


* SV costs
mi estimate: margin_cost sv_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store sv_cost_margins_mod1

mi estimate: margin_cost sv_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store sv_cost_margins_mod2


* OV costs
mi estimate: margin_cost ov_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store ov_cost_margins_mod1

mi estimate: margin_cost ov_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store ov_cost_margins_mod2


* social care costs
mi estimate: margin_cost sol_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store sol_cost_margins_mod1

mi estimate: margin_cost sol_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store sol_cost_margins_mod2



**# 3.2. Marginal differences in costs by malnutriton status
* write a program to automate the process for estimating marginal differences
capture program drop margin_costdiff
program define margin_costdiff, eclass properties(mi)

    syntax varname [if] [in], COV1(string) COV2(string)

    twopm (`varlist' = i.malnut i.fu_year_start `cov1') (`varlist' = i.malnut##i.fu_year_start `cov2') `if' `in', firstpart(logit) secondpart(glm, family(gaussian) link(identity)) vce(cluster LopNr)

    margins, at(malnut=0) at(malnut=1) contrast(atcontrast(r)) post

end

* Model 1: adjust for age, sex, diaggrp, fu_year_start, and year in death
* Model 2: additionally adjust for state_base mi hf stroke copd ra op cancer 

* total costs
mi estimate: margin_costdiff total_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store total_costdiff_margins_mod1

mi estimate: margin_costdiff total_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store total_costdiff_margins_mod2


* drug costs
mi estimate: margin_costdiff drug_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store drug_costdiff_margins_mod1

mi estimate: margin_costdiff drug_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store drug_costdiff_margins_mod2


* SV costs
mi estimate: margin_costdiff sv_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store sv_costdiff_margins_mod1

mi estimate: margin_costdiff sv_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store sv_costdiff_margins_mod2


* OV costs
mi estimate: margin_costdiff ov_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store ov_costdiff_margins_mod1

mi estimate: margin_costdiff ov_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store ov_costdiff_margins_mod2


* social care costs
mi estimate: margin_costdiff sol_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year) cov2(age i.sex i.diaggrp i.death_in_year)
estimates store sol_costdiff_margins_mod1

mi estimate: margin_costdiff sol_costs_trim, cov1(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer) cov2(age i.sex i.diaggrp i.death_in_year i.state_base i.mi i.hf i.stroke i.copd i.ra i.op i.cancer)
estimates store sol_costdiff_margins_mod2



**# 4. Export results to excel 
**# 4.1. Export model coefficients
local model total_costs_mod1 total_costs_mod2 drug_costs_mod1 drug_costs_mod2 sv_costs_mod1 sv_costs_mod2 ov_costs_mod1 ov_costs_mod2 sol_costs_mod1 sol_costs_mod2

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
frame mod_coef: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/svedem_model_output_xx.xlsx", firstrow(varlabels) sheet("`modelname'",replace)
}




**# 4.2. Export margins estimates
capture frame drop med_costs
frame create med_costs str30 hcru_mod str30 malnut str30 cost str30 pv

local out total_cost_margins_mod1 total_cost_margins_mod2 drug_cost_margins_mod1 drug_cost_margins_mod2 sv_cost_margins_mod1 sv_cost_margins_mod2 ov_cost_margins_mod1 ov_cost_margins_mod2 sol_cost_margins_mod1 sol_cost_margins_mod2

foreach var of local out{
	
estimates restore `var'
estimates replay `var'

forvalues i=1/2{ 
	local cost=string(r(table)[1,`i'], "%9.0f") 
	local lb=string(r(table)[5,`i'], "%9.0f") 
	local ub=string(r(table)[6,`i'], "%9.0f") 
	local pv=string(r(table)[4,`i'], "%9.3f") 
	
	frame post med_costs ("`var'") ("malnut`i'") ( "`cost' (`lb',`ub')") ("`pv'")
	}
}


local outdiff total_costdiff_margins_mod1 total_costdiff_margins_mod2 drug_costdiff_margins_mod1 drug_costdiff_margins_mod2 sv_costdiff_margins_mod1 sv_costdiff_margins_mod2 ov_costdiff_margins_mod1 ov_costdiff_margins_mod2 sol_costdiff_margins_mod1 sol_costdiff_margins_mod2

foreach var of local outdiff{
	
estimates restore `var'
estimates replay `var'

	local cost=string(r(table)[1,1], "%9.0f") 
	local lb=string(r(table)[5,1], "%9.0f") 
	local ub=string(r(table)[6,1], "%9.0f") 
	local pv=string(r(table)[4,1], "%9.3f") 
	
	frame post med_costs ("`var'") ("malnut2") ( "`cost' (`lb',`ub')") ("`pv'")
}



frame med_costs: gen model = substr(hcru_mod, strrpos(hcru_mod, "_") + 1, .)
frame med_costs: gen hcru = substr(hcru_mod, 1, strrpos(hcru_mod, "_") - 1)
frame med_costs: drop hcru_mod
frame med_costs: order hcru model malnut cost pv

frame med_costs: list


frame med_costs: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/svedem_margins_xx.xlsx", firstrow(varlabels) sheet("costs",replace)

