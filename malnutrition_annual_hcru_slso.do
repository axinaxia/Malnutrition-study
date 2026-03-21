cd "/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/"

use "Organized data/slso_nutri_hcru_cov_xx.dta",clear


**# 1. Impute missing data
misstable summarize

**# 1.1. Impute missing data in baseline covariates
mi set wide
mi register imp civil_status edu_level ensamboende mna_cat adl_bi_cat

mi impute chained (ologit,augment) civil_status edu_level mna_cat adl_bi_cat (logit,augment) ensamboende = age sex ihd hf stroke pd copd ra op cancer depression dem, add(20) rseed(2026) augment force




**# 1.2. Merge imputed baseline covariates with outcome data
merge 1:m lopnr using slso_nutri_hcru_cov_xx.dta, nogen
misstable summarize


**# 2. GLM and two-part models with multiple imputation
**# 2.1. Write programs to automate the process
capture program drop glm_model
program define glm_model, eclass

    syntax varname [if] [in], COV1(string)

    local depvar `varlist'
    local cond "`if' `in'"
    
    mi estimate, cmdok: glm `depvar' i.mna_cat `cov1' `cond',family(gamma) link(log)

end



capture program drop twopm_model
program define twopm_model, eclass

    syntax varname [if] [in], COV1(string) COV2(string)

    local depvar `varlist'
    local cond "`if' `in'"
    
    mi estimate, cmdok: twopm (`depvar' = i.mna_cat  `cov1') (`depvar' = i.mna_cat  `cov2') `cond', firstpart(logit) secondpart(glm, family(gamma) link(log)) 

end



**# 2.2. Model 1: adjust for age, sex
* total costs
glm_model total_cost_trim if total_cost_trim!=., cov1(age i.sex)
estimates store total_costs_mod1

* geriatric costs - first admission
glm_model geri1st_cost_trim if geri1st_cost_trim!=., cov1(age i.sex)
estimates store geri1st_costs_mod1

* SOL costs
twopm_model sol_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store sol_costs_mod1

* PV costs
twopm_model pv_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store pv_costs_mod1

* OV costs
twopm_model ov_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store ov_costs_mod1

* geriatric costs - readmission
twopm_model gerireadm_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store gerireadm_costs_mod1

* other SV costs
twopm_model othersv_cost_trim , cov1(age i.sex) cov2(age i.sex)
estimates store othersv_costs_mod1



**# 2.3. Model 2: additionally adjust for civil_status edu_level adl_bi_cat ihd hf stroke pd copd ra op cancer depression dem
* total costs
glm_model total_cost_trim if total_cost_trim!=., cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store total_costs_mod2

* geriatric costs - first admission
glm_model geri1st_cost_trim if geri1st_cost_trim!=., cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store geri1st_costs_mod2

* SOL costs
twopm_model sol_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store sol_costs_mod2

* PV costs
twopm_model pv_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store pv_costs_mod2

* OV costs
twopm_model ov_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store ov_costs_mod2

* geriatric costs - readmission
twopm_model gerireadm_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store gerireadm_costs_mod2

* other SV costs
twopm_model othersv_cost_trim , cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store othersv_costs_mod2





**# 3. Margins estimation
**# 3.1. Costs by malnutriton status
* write programs to automate the process for estimating marginal estimates
capture program drop glm_margin_cost
program define glm_margin_cost, eclass properties(mi)

    syntax varname [if] [in], COV1(string)

    glm `varlist' i.mna_cat `cov1' `if' `in', family(gamma) link(log)
	
	margins, at(mna_cat=1) at(mna_cat=2) at(mna_cat=3) post
	
end



capture program drop tp_margin_cost
program define tp_margin_cost, eclass properties(mi)

    syntax varname [if] [in], COV1(string) COV2(string)

    twopm (`varlist' = i.mna_cat  `cov1') (`varlist' = i.mna_cat  `cov2') `if' `in', firstpart(logit) secondpart(glm, family(gamma) link(log)) 
	
	margins, at(mna_cat=1) at(mna_cat=2) at(mna_cat=3) post
	
end



* Model 1: adjust for age, sex
* Model 2: additionally adjust for civil_status edu_level adl_bi_cat ihd hf stroke pd copd ra op cancer depression dem

* total costs
mi estimate: glm_margin_cost total_cost_trim if total_cost_trim!=., cov1(age i.sex)
estimates store total_cost_margins_mod1

mi estimate: glm_margin_cost total_cost_trim if total_cost_trim!=., cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store total_cost_margins_mod2


* geriatric costs - first admission
mi estimate: glm_margin_cost geri1st_cost_trim if geri1st_cost_trim!=., cov1(age i.sex)
estimates store geri1st_cost_margins_mod1

mi estimate: glm_margin_cost geri1st_cost_trim if geri1st_cost_trim!=., cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store geri1st_cost_margins_mod2


* SOL costs
mi estimate: tp_margin_cost sol_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store sol_cost_margins_mod1

mi estimate: tp_margin_cost sol_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store sol_cost_margins_mod2


* PV costs
mi estimate: tp_margin_cost pv_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store pv_cost_margins_mod1

mi estimate: tp_margin_cost pv_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store pv_cost_margins_mod2


* OV costs
mi estimate: tp_margin_cost ov_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store ov_cost_margins_mod1

mi estimate: tp_margin_cost ov_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store ov_cost_margins_mod2


* geriatric costs - readmission
mi estimate: tp_margin_cost gerireadm_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store gerireadm_cost_margins_mod1

mi estimate: tp_margin_cost gerireadm_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store gerireadm_cost_margins_mod2


* other SV costs
mi estimate: tp_margin_cost othersv_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store othersv_cost_margins_mod1

mi estimate: tp_margin_cost othersv_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store othersv_cost_margins_mod2





**# 3.2. Marginal differences in costs by malnutriton status
* write programs to automate the process for estimating marginal differences
capture program drop glm_margin_costdiff
program define glm_margin_costdiff, eclass properties(mi)

    syntax varname [if] [in], COV1(string)

    glm `varlist' i.mna_cat  `cov1' `if' `in',family(gamma) link(log)

    margins, at(mna_cat=1) at(mna_cat=2) at(mna_cat=3) contrast(atcontrast(r)) post

end


capture program drop tp_margin_costdiff
program define tp_margin_costdiff, eclass properties(mi)

    syntax varname [if] [in], COV1(string) COV2(string)

    twopm (`varlist' = i.mna_cat  `cov1') (`varlist' = i.mna_cat  `cov2') `if' `in', firstpart(logit) secondpart(glm, family(gamma) link(log)) 

    margins, at(mna_cat=1) at(mna_cat=2) at(mna_cat=3) contrast(atcontrast(r)) post

end


* Model 1: adjust for age, sex
* Model 2: additionally adjust for civil_status edu_level adl_bi_cat ihd hf stroke pd copd ra op cancer depression dem

* total costs
mi estimate: glm_margin_costdiff total_cost_trim if total_cost_trim!=., cov1(age i.sex)
estimates store total_costdiff_margins_mod1

mi estimate: glm_margin_costdiff total_cost_trim if total_cost_trim!=., cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store total_costdiff_margins_mod2


* geriatric costs - first admission
mi estimate: glm_margin_costdiff geri1st_cost_trim if geri1st_cost_trim!=., cov1(age i.sex)
estimates store geri1_costdiff_margins_mod1

mi estimate: glm_margin_costdiff geri1st_cost_trim if geri1st_cost_trim!=., cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store geri1_costdiff_margins_mod2


* SOL costs
mi estimate: tp_margin_costdiff sol_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store sol_costdiff_margins_mod1

mi estimate: tp_margin_costdiff sol_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store sol_costdiff_margins_mod2


* PV costs
mi estimate: tp_margin_costdiff pv_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store pv_costdiff_margins_mod1

mi estimate: tp_margin_costdiff pv_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store pv_costdiff_margins_mod2


* OV costs
mi estimate: tp_margin_costdiff ov_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store ov_costdiff_margins_mod1

mi estimate: tp_margin_costdiff ov_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store ov_costdiff_margins_mod2


* geriatric costs - readmission
mi estimate: tp_margin_costdiff gerireadm_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store gerir_costdiff_margins_mod1

mi estimate: tp_margin_costdiff gerireadm_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store gerir_costdiff_margins_mod2


* other SV costs
mi estimate: tp_margin_costdiff othersv_cost_trim, cov1(age i.sex) cov2(age i.sex)
estimates store svo_costdiff_margins_mod1

mi estimate: tp_margin_costdiff othersv_cost_trim, cov1(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem) cov2(age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem)
estimates store svo_costdiff_margins_mod2



**# 4. Export results to excel 
**# 4.1. Export model coefficients
local model total_costs_mod1 geri1st_costs_mod1 sol_costs_mod1 pv_costs_mod1 ov_costs_mod1 gerireadm_costs_mod1 othersv_costs_mod1 total_costs_mod2 geri1st_costs_mod2 sol_costs_mod2 pv_costs_mod2  ov_costs_mod2 gerireadm_costs_mod2 othersv_costs_mod2


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
frame mod_coef: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/slso_model_output_xx.xlsx", firstrow(varlabels) sheet("`modelname'",replace)
}




**# 4.2. Export margins estimates
capture frame drop med_costs
frame create med_costs str30 hcru_mod str30 mna str30 cost str30 pv

local out total_cost_margins_mod1 total_cost_margins_mod2 geri1st_cost_margins_mod1 geri1st_cost_margins_mod2 sol_cost_margins_mod1 sol_cost_margins_mod2 pv_cost_margins_mod1 pv_cost_margins_mod2 ov_cost_margins_mod1 ov_cost_margins_mod2 gerireadm_cost_margins_mod1 gerireadm_cost_margins_mod2 othersv_cost_margins_mod1 othersv_cost_margins_mod2

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


local outdiff total_costdiff_margins_mod1 total_costdiff_margins_mod2 geri1_costdiff_margins_mod1 geri1_costdiff_margins_mod2 sol_costdiff_margins_mod1 sol_costdiff_margins_mod2 pv_costdiff_margins_mod1 pv_costdiff_margins_mod2 ov_costdiff_margins_mod1 ov_costdiff_margins_mod2 gerir_costdiff_margins_mod1 gerir_costdiff_margins_mod2 svo_costdiff_margins_mod1 svo_costdiff_margins_mod2

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


frame med_costs: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/slso_margins_xx.xlsx", firstrow(varlabels) sheet("costs",replace)

