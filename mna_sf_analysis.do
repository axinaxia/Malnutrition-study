cd "/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/Malnutrition statistical analysis/Malnutrition-study/"

use "Organized data/sf_snack.dta",clear

**# 1. Impute missingness
misstable summarize

gen ra_op=ra==1|op==1
gen pd_dem=pd==1|dementia==1

mi set wide
mi register imp PF_lvl RL_lvl SF_lvl PAIN_lvl MH_lvl VIT_lvl mna_screening1_cat education tobacco1 smell_imp taste_imp chew_imp

mi impute chained (ologit,augment) PF_lvl RL_lvl SF_lvl PAIN_lvl MH_lvl VIT_lvl mna_screening1_cat education tobacco1 (logit,augment) smell_imp taste_imp chew_imp = age1 sex lives1 mi hf stroke copd ra_op cancer depression pd_dem, add(20) rseed(2026) augment force


* define dimension "most" (binary variable): level 3 for physical functioning; levels 3 and 4 for role limitation; and levels 4 and 5 for social functioning, pain, and mental health; and level 5 for vitality
mi passive: gen most=PF_lvl==3|RL_lvl>=3|SF_lvl>=4|PAIN_lvl>=4|MH_lvl>=4|VIT_lvl==5

* disutility for each dimension
mi passive: gen d_pf=(PF_lvl==1)*0.000+(PF_lvl==2)*0.0+(PF_lvl==3)*0.045
mi passive: gen d_rl=(RL_lvl>1)*0.063
mi passive: gen d_sf=(SF_lvl==1)*0.000+(SF_lvl==2)*0.063+(SF_lvl==3)*0.066+(SF_lvl==4)*0.081+(SF_lvl==5)*0.093
mi passive: gen d_pain=(PAIN_lvl==1)*0.000+(PAIN_lvl==2)*0.00+(PAIN_lvl==3)*0.042+(PAIN_lvl==4)*0.077+(PAIN_lvl==5)*0.137
mi passive: gen d_mh=(MH_lvl==1)*0+(MH_lvl==2)*0.059+(MH_lvl==3)*0.059+(MH_lvl==4)*0.113+(MH_lvl==5)*0.134
mi passive: gen d_vit=(VIT_lvl==1)*0+(VIT_lvl==2)*0.078+(VIT_lvl==3)*0.078+(VIT_lvl==4)*0.078+(VIT_lvl==5)*0.106


*calculate SF-12 utility using the algorithm by Brazier et al.2024
mi passive: gen ui=1.0-(d_pf+d_rl+d_sf+d_pain+d_mh+d_vit)-most*0.077


* sanity check by comparing with Linus's calculation
* br if lopnr==8|lopnr==17|lopnr==20|lopnr==21|lopnr==26|lopnr==29




**# 2. GLM for predicting SF-12 utility from malnutrition
**# 2.1. GLM models
* check if gamma with log link is reasonable
glm ui i.mna_screening1_cat age1 i.sex i.education i.tobacco1 i.lives1 i.mi i.hf i.stroke i.copd ra_op i.cancer i.depression i.pd_dem, family(gamma) link(log)

predict ui_yhat
hist ui_yhat, name(ui_h1, replace)
hist ui, name(ui_h2, replace)

graph combine ui_h1 ui_h2

graph drop _all


* write progams to run marginal effects using imputed data analysis
* SF12 utility by MNA category
capture program drop margin_sf
program define margin_sf, eclass properties(mi)

    syntax varname [if] [in], COV1(string)

    glm `varlist' i.mna_screening1_cat `cov1' `if' `in', family(gamma) link(log)
	
	margins, at(mna_screening1_cat=1) at(mna_screening1_cat=2) at(mna_screening1_cat=3) post
	
end


mi estimate: margin_sf ui, cov1(age1 i.sex)
estimates store sf_m1
mi estimate: margin_sf ui, cov1(age1 i.sex i.education i.tobacco1 i.lives1 i.mi i.hf i.stroke i.copd ra_op i.cancer i.depression i.pd_dem)
estimates store sf_m2



* SF12 utility difference by MNA category
capture program drop margin_sfdiff
program define margin_sfdiff, eclass properties(mi)

    syntax varname [if] [in], COV1(string)

    glm `varlist' i.mna_screening1_cat  `cov1' `if' `in',family(gamma) link(log)

    margins, at(mna_screening1_cat=1) at(mna_screening1_cat=2) at(mna_screening1_cat=3) contrast(atcontrast(r)) post

end


mi estimate: margin_sfdiff ui, cov1(age1 i.sex)
estimates store sfdiff_m1
mi estimate: margin_sfdiff ui, cov1(age1 i.sex i.education i.tobacco1 i.lives1 i.mi i.hf i.stroke i.copd ra_op i.cancer i.depression i.pd_dem)
estimates store sfdiff_m2



**# 2.2. Export margins 
capture frame drop margins_sf
frame create margins_sf str30 qol_mod str30 mna str30 utility str30 pv

local out sf_m1 sf_m2

foreach var of local out{
	
estimates restore `var'
estimates replay `var'

forvalues i=1/3{ 
	local utility=string(r(table)[1,`i'], "%9.3f") 
	local lb=string(r(table)[5,`i'], "%9.3f") 
	local ub=string(r(table)[6,`i'], "%9.3f") 
	local pv=string(r(table)[4,`i'], "%9.3f") 
	
	frame post margins_sf ("`var'") ("mna`i'") ( "`utility' (`lb',`ub')") ("`pv'")
	}
}


local outdiff sfdiff_m1 sfdiff_m2

foreach var of local outdiff{
	
estimates restore `var'
estimates replay `var'

forvalues i=1/2{ 
	local utility=string(r(table)[1,`i'], "%9.3f") 
	local lb=string(r(table)[5,`i'], "%9.3f") 
	local ub=string(r(table)[6,`i'], "%9.3f") 
	local pv=string(r(table)[4,`i'], "%9.3f") 
	local j=`i'+1
	
	frame post margins_sf ("`var'") ("mna`j'") ( "`utility' (`lb',`ub')") ("`pv'")
	}
}



frame margins_sf: gen model = substr(qol_mod, strrpos(qol_mod, "_") + 1, .)
frame margins_sf: gen hcru = substr(qol_mod, 1, strrpos(qol_mod, "_") - 1)
frame margins_sf: drop qol_mod
frame margins_sf: order hcru model mna utility pv

frame margins_sf: list


frame margins_sf: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/malnutrition_qol.xlsx", firstrow(varlabels) sheet("utility",replace)





**# 3. Associations between MNA and individual dimension
**# 3.1. Logistic regressions
local dim PF RL SF PAIN MH VIT
foreach d of local dim{ 
	mi estimate: ologit `d'_lvl i.mna_screening1_cat age1 i.sex
estimates store `d'_m1

mi estimate: ologit `d'_lvl i.mna_screening1_cat age1 i.sex i.education i.tobacco1 i.lives1 i.mi i.hf i.stroke i.copd ra_op i.cancer i.depression i.pd_dem
estimates store `d'_m2
}



**# 3.2. Export coefficients from the model
capture frame drop mod_coef
frame create mod_coef str30 model str30 colname str30 coef str30 pv

local mod PF_m1 RL_m1 SF_m1 PAIN_m1 MH_m1 VIT_m1 PF_m2 RL_m2 SF_m2 PAIN_m2 MH_m2 VIT_m2
foreach m of local mod {
    
	local modelname="`m'"

estimates restore `m'
estimates replay `m'

    matrix tbl = r(table)
    local ncols = colsof(tbl)

    local cnames : colnames tbl

    forvalues j = 1/`ncols' {
        
        local colname : word `j' of `cnames'
        
        scalar bval  = tbl[1, `j']
        scalar lbval = tbl[5, `j']
        scalar ubval = tbl[6, `j']
        scalar pvval = tbl[4, `j']

        local b  = bval
        local lb = lbval
        local ub = ubval
        local pv = pvval

		if !(strpos("`colname'", "mna")) continue
		if missing(`pv') continue

        local coef = string(exp(`b'), "%9.3f")
        local lb_s = string(exp(`lb'), "%9.3f")
        local ub_s = string(exp(`ub'), "%9.3f")
        local pv_s = string(`pv', "%9.3f")

        frame post mod_coef ("`modelname'") ("`colname'") ("`coef' (`lb_s'-`ub_s')") ("`pv_s'")
    }
}

frame mod_coef: list
frame mod_coef: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/malnutrition_qol.xlsx", firstrow(varlabels) sheet("dim",replace)


