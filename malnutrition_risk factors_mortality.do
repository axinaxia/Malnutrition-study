cd "/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/"

cd "C:\Users\xinxia\OneDrive - Karolinska Institutet\Malnutrition project data analysis\Data\" 


**# 1. Risk factors for malnutrition
**# 1.1. SNAC-K cohort
use "Organized data/snack_nutri_hcru_cov_xx.dta",clear
keep if fu_year==0
keep lopnr-smell_taste_imp death fu

misstable summarize

mi set wide
mi register imp education tobacco1 mna_screening1_cat 

mi impute chained (ologit,augment) education mna_screening1_cat tobacco1 = age1 sex lives1 mi hf stroke pd_dem copd ra_op cancer depression smell_taste_imp chew_imp poor_appetite, add(20) rseed(2026) augment force




capture frame drop prec_snack
frame create prec_snack str30 colname str30 coef str30 pv

local cov education lives1 tobacco1 mi hf stroke pd_dem copd ra_op cancer depression smell_taste_imp chew_imp poor_appetite

foreach c of local cov {
    
    mi estimate: mlogit mna_screening1_cat i.`c' age1 i.sex

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

		if !strpos("`colname'", "`c'") continue
		if missing(`pv') continue

        local coef = string(exp(`b'), "%9.2f")
        local lb_s = string(exp(`lb'), "%9.2f")
        local ub_s = string(exp(`ub'), "%9.2f")
        local pv_s = string(`pv', "%9.3f")

        frame post prec_snack ("`colname'") ("`coef' (`lb_s'-`ub_s')") ("`pv_s'")
    }
}

frame prec_snack: list
frame prec_snack: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/malnutrition_predictor_mortality.xlsx", firstrow(varlabels) sheet("snac-k",replace)



**# 1.2. SLSO cohort
use "Organized data/slso_nutri_hcru_cov_xx.dta",clear

keep lopnr-dem death_180d

misstable summarize

mi set wide
mi register imp civil_status edu_level ensamboende mna_cat adl_bi_cat

mi impute chained (ologit,augment) civil_status edu_level mna_cat adl_bi_cat (logit,augment) ensamboende = age sex ihd hf stroke pd dem copd ra op cancer depression, add(20) rseed(2026) augment force




capture frame drop prec_slso
frame create prec_slso str30 colname str30 coef str30 pv

local cov edu_level ensamboende ihd hf stroke pd dem copd ra op cancer depression

foreach c of local cov {
    
    mi estimate: mlogit mna_cat i.`c' age i.sex

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

		if !strpos("`colname'", "`c'") continue
		if missing(`pv') continue

        local coef = string(exp(`b'), "%9.2f")
        local lb_s = string(exp(`lb'), "%9.2f")
        local ub_s = string(exp(`ub'), "%9.2f")
        local pv_s = string(`pv', "%9.3f")

        frame post prec_slso ("`colname'") ("`coef' (`lb_s'-`ub_s')") ("`pv_s'")
    }
}

frame prec_slso: list
frame prec_slso: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/malnutrition_predictor_mortality.xlsx", firstrow(varlabels) sheet("slso",replace)



**# 1.3. SveDem
use "Organized data/svedem_nutri_hcru_cov_xx.dta",clear
keep if fu_year_start==0

keep LopNr BOENDEFORHALLANDE malnut sabo age SEX mi-depression fu

gen sex=0
replace sex=1 if SEX=="FEMALE"

gen live_alone=.
replace live_alone=0 if BOENDEFORHALLANDE=="SAMMANBOENDE"
replace live_alone=1 if BOENDEFORHALLANDE=="ENSAMBOENDE"

label drop sabo
replace sabo=sabo-1

label drop malnut
replace malnut=malnut-1

misstable summarize

mi set wide
mi register imp malnut sabo live_alone

mi impute chained (logit,augment) malnut sabo live_alone = age sex mi hf stroke pd copd ra op cancer depression, add(20) rseed(2026) augment force




capture frame drop prec_svedem
frame create prec_svedem str30 colname str30 coef str30 pv

local cov sabo live_alone mi hf stroke pd copd ra op cancer depression

foreach c of local cov {
    
    mi estimate: logit malnut i.`c' age i.sex

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

		if !strpos("`colname'", "`c'") continue
		if missing(`pv') continue

        local coef = string(exp(`b'), "%9.2f")
        local lb_s = string(exp(`lb'), "%9.2f")
        local ub_s = string(exp(`ub'), "%9.2f")
        local pv_s = string(`pv', "%9.3f")

        frame post prec_svedem ("`colname'") ("`coef' (`lb_s'-`ub_s')") ("`pv_s'")
    }
}

frame prec_svedem: list
frame prec_svedem: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/malnutrition_predictor_mortality.xlsx", firstrow(varlabels) sheet("svedem",replace)




**# 2. Malnutrition and mortality
**# 2.1. SNAC-K cohort
use "Organized data/snack_nutri_hcru_cov_xx.dta",clear
keep if fu_year==0
keep lopnr-smell_taste_imp death fu

* generate cumulative hazard for death for use in imputation
stset fu, failure(death==1)
sts generate nae=na

misstable summarize

mi set wide
mi register imp education tobacco1 mna_screening1_cat 

mi impute chained (ologit,augment) education mna_screening1_cat tobacco1 = nae _d age1 sex lives1 mi hf stroke pd_dem copd ra_op cancer depression smell_taste_imp chew_imp poor_appetite, add(20) rseed(2026) augment force


mi estimate: stcox i.mna_screening1_cat age1 i.sex
estimates store cox_mod1_snack


mi estimate: stcox i.mna_screening1_cat age1 i.sex i.education i.tobacco1 i.lives1 i.mi i.hf i.stroke i.pd_dem i.copd i.ra_op i.cancer i.depression i.smell_taste_imp i.chew_imp i.poor_appetite
estimates store cox_mod2_snack


**# 2.2. SLSO cohort
use "Organized data/slso_nutri_hcru_cov_xx.dta",clear


* generate cumulative hazard for death for use in imputation
stset fu, failure(death==1)
sts generate nae=na

misstable summarize

mi set wide
mi register imp civil_status edu_level ensamboende mna_cat adl_bi_cat

mi impute chained (ologit,augment) civil_status edu_level mna_cat adl_bi_cat (logit,augment) ensamboende = nae _d age sex ihd hf stroke pd copd ra op cancer depression dem, add(20) rseed(2026) augment force


mi estimate: stcox i.mna_cat age i.sex 
estimates store cox_mod1_slso

mi estimate: stcox i.mna_cat age i.sex i.civil_status i.edu_level i.adl_bi_cat i.ihd i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.dem
estimates store cox_mod2_slso


**# 2.3. SveDem
use "Organized data/svedem_nutri_hcru_cov_xx.dta",clear
keep if fu_year_start==0

keep LopNr BOENDEFORHALLANDE malnut sabo age SEX mi-depression fu death

gen sex=0
replace sex=1 if SEX=="FEMALE"

gen live_alone=.
replace live_alone=0 if BOENDEFORHALLANDE=="SAMMANBOENDE"
replace live_alone=1 if BOENDEFORHALLANDE=="ENSAMBOENDE"

label drop sabo
replace sabo=sabo-1

label drop malnut
replace malnut=malnut-1

* generate cumulative hazard for death for use in imputation
stset fu, failure(death==1)
sts generate nae=na

misstable summarize

mi set wide
mi register imp malnut sabo live_alone

mi impute chained (logit,augment) malnut sabo live_alone = nae _d age sex mi hf stroke pd copd ra op cancer depression, add(20) rseed(2026) augment force


mi estimate: stcox i.malnut age i.sex
estimates store cox_mod1_svedem


mi estimate: stcox i.malnut age i.sabo i.live_alone i.sex i.mi i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression
estimates store cox_mod2_svedem


**# 2.4. Export coefficients from the model

capture frame drop mod_coef
frame create mod_coef str30 model str30 colname str30 coef str30 pv

local mod cox_mod1_snack cox_mod2_snack cox_mod1_slso cox_mod2_slso cox_mod1_svedem cox_mod2_svedem
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

		if !(strpos("`colname'", "mna")|strpos("`colname'", "malnut")) continue
		if missing(`pv') continue

        local coef = string(exp(`b'), "%9.2f")
        local lb_s = string(exp(`lb'), "%9.2f")
        local ub_s = string(exp(`ub'), "%9.2f")
        local pv_s = string(`pv', "%9.3f")

        frame post mod_coef ("`modelname'") ("`colname'") ("`coef' (`lb_s'-`ub_s')") ("`pv_s'")
    }
}

frame mod_coef: list
frame mod_coef: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/malnutrition_predictor_mortality.xlsx", firstrow(varlabels) sheet("mortality",replace)
