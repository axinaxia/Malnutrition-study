cd "/Users/xin.xia/Library/CloudStorage/OneDrive-SharedLibraries-KarolinskaInstitutet/GRP_Economic evaluations of malnutrition in Swedish older adults (KI Livsmedelsverket) - Documents/Individal level data analysis/Data/"

use "nutri_hcru_cov_xx.dta",clear

**# 1. Check and impute missing data
misstable summarize 

mi set wide
mi xtset lopnr fu_year
mi register imp education tobacco1 mna_screening1_cat smell_taste_imp poor_appetite

mi impute chained (ologit,augment) education mna_screening1_cat tobacco1 (logit,augment) smell_taste_imp poor_appetite = age1 sex lives1 mi hf stroke copd ra_op cancer depression pd_dem, add(20) rseed(202511) augment force



**# 2. Medical care costs per MNA status
capture program drop margin_cost
program margin_cost, eclass properties(mi)
args out
	glm `out' age1 i.sex i.education i.mna_screening1_cat i.tobacco1 lives1 mi hf stroke copd ra_op cancer depression pd_dem smell_taste_imp poor_appetite i.fu_year, family(gamma) link(log) vce(cluster lopnr)
	
	margins mna_screening1_cat, post
	
end


* primary care
mi estimate, cmdok: margin_cost pv_cost
estimates store pooled_pv_cost

* outpatient care 
mi estimate, cmdok: margin_cost ov_cost
estimates store pooled_ov_cost

* inpatient care
mi estimate, cmdok: margin_cost sv_cost
estimates store pooled_sv_cost

* total costs
mi estimate, cmdok: margin_cost total_cost
estimates store pooled_total_cost



**# 2. Differences in medical care costs per MNA status
program margin_costdiff, eclass properties(mi)
args out
	glm `out' age1 i.sex i.education i.mna_screening1_cat i.tobacco1 lives1 mi hf stroke copd ra_op cancer depression pd_dem smell_taste_imp poor_appetite i.fu_year, family(gamma) link(log) vce(cluster lopnr)
	
	margins, dydx(mna_screening1_cat) post
	
end


* primary care
mi estimate, cmdok: margin_costdiff pv_cost
estimates store pooled_pv_costdiff

* outpatient care 
mi estimate, cmdok: margin_costdiff ov_cost
estimates store pooled_ov_costdiff

* inpatient care
mi estimate, cmdok: margin_costdiff sv_cost
estimates store pooled_sv_costdiff

* total costs
mi estimate, cmdok: margin_costdiff total_cost
estimates store pooled_total_costdiff



**# 4. Export results to excel 
capture frame drop med_costs
frame create med_costs str30 hcru str30 mna str30 cost str30 pv

local out pooled_pv_cost pooled_sv_cost pooled_ov_cost pooled_total_cost pooled_pv_costdiff pooled_sv_costdiff pooled_ov_costdiff pooled_total_costdiff 
foreach var of local out{
	
estimates restore `var'
estimates replay `var'

forvalues i=1/3{ 
	local cost=string(r(table)[1,`i'], "%9.0f") 
	local lb=string(r(table)[5,`i'], "%9.0f") 
	local ub=string(r(table)[6,`i'], "%9.0f") 
	local pv=string(r(table)[4,`i'], "%9.3f") 
	
	frame post med_costs ("`var'") ("mna`i'") ( "`cost' (`lb'-`ub')") ("`pv'")
	}
}

frame med_costs: list


frame med_costs: export excel using "Malnutrition statistical analysis/Malnutrition-study/results_xx.xlsx", firstrow(varlabels) sheet("glm",replace)

