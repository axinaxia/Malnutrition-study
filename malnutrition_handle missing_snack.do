cd "/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/"

use "Organized data/snack_nutri_hcru_cov_xx.dta",clear


**# 1. Check and impute missing data in baseline covariates
keep if fu_year==0
keep lopnr-smell_taste_imp
misstable summarize

**# 1.1. Impute missing data in baseline covariates
mi set wide
mi register imp education tobacco1 mna_screening1_cat 

mi impute chained (ologit,augment) education mna_screening1_cat tobacco1 = age1 sex lives1 mi hf stroke copd ra_op cancer depression pd_dem, add(3) rseed(2026) augment force


**# 1.2. Merge imputed baseline covariates with outcome data
merge 1:m lopnr using snack_nutri_hcru_cov_xx.dta, nogen

* fu_year>=11 is sparse, merge fu_year>=11 to a single category
replace fu_year=11 if fu_year>=11

misstable summarize




**# 2. Check coefficients of two-part model with imputation
**# 2.1. Use cmdok to pool results (stata's command for allowing mi estimate for non-supported command) 
mi estimate, cmdok: twopm (total_cost_trim = i.mna_screening1_cat i.fu_year age1 i.sex i.pv_death_in_year) (total_cost_trim = i.mna_screening1_cat##i.fu_year age1 i.sex i.pv_death_in_year) if total_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)


**# 2.2. Compare with individual results for each imputed dataset (error shooting)
twopm (total_cost_trim = i._1_mna_screening1_cat i.fu_year age1 i.sex i.pv_death_in_year) (total_cost_trim = i._1_mna_screening1_cat##i.fu_year age1 i.sex i.pv_death_in_year) if total_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)


twopm (total_cost_trim = i._2_mna_screening1_cat i.fu_year age1 i.sex i.pv_death_in_year) (total_cost_trim = i._2_mna_screening1_cat##i.fu_year age1 i.sex i.pv_death_in_year) if total_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)


twopm (total_cost_trim = i._3_mna_screening1_cat i.fu_year age1 i.sex i.pv_death_in_year) (total_cost_trim = i._3_mna_screening1_cat##i.fu_year age1 i.sex i.pv_death_in_year) if total_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)






**#3. Check margins estimation after two-part model with imputation

**# 3.1. Costs by malnutriton status
capture program drop margin_cost
program margin_cost, eclass properties(mi)
args out

	twopm (`out' = i.mna_screening1_cat i.fu_year age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) (`out' = i.mna_screening1_cat##i.fu_year age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) if total_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)
	
	margins mna_screening1_cat, post
	
end


mi estimate, cmdok: margin_cost total_cost_trim




**# 3.2. Differences in costs by malnutriton status
capture program drop margin_costdiff
program margin_costdiff, eclass properties(mi)
args out
	twopm (`out' = i.mna_screening1_cat i.fu_year age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) (`out' = i.mna_screening1_cat##i.fu_year age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) if total_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)
	
	margins, dydx(mna_screening1_cat) post
	
end

mi estimate, cmdok: margin_costdiff total_cost_trim



**# 3.2. Compare with individual results for each imputed dataset (error shooting)
twopm (total_cost_trim= i._1_mna_screening1_cat i.fu_year age1 i.sex i._1_tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) (total_cost_trim= i._1_mna_screening1_cat##i.fu_year age1 i.sex i._1_tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) if total_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)

margins _1_mna_screening1_cat
margins, dydx(_1_mna_screening1_cat)


twopm (total_cost_trim= i._2_mna_screening1_cat i.fu_year age1 i.sex i._2_tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) (total_cost_trim= i._2_mna_screening1_cat##i.fu_year age1 i.sex i._2_tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) if total_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)

margins _2_mna_screening1_cat
margins, dydx(_2_mna_screening1_cat)


twopm (total_cost_trim= i._3_mna_screening1_cat i.fu_year age1 i.sex i._3_tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) (total_cost_trim= i._3_mna_screening1_cat##i.fu_year age1 i.sex i._3_tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.pv_death_in_year) if total_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)

margins _3_mna_screening1_cat
margins, dydx(_3_mna_screening1_cat)

