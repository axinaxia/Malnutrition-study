*cd "/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data"

cd "C:\Users\xinxia\OneDrive - Karolinska Institutet\Malnutrition project data analysis\Data\" 

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

**# 1. Compare alternative specifications in two-part models
capture frame drop model
frame create model str30 outcome str30 family str30 link str30 me str30 mae str30 rmse 

local costs total drug ov sv sol
local fam gaussian igaussian gamma
local link identity log

foreach c of local costs {
    foreach f of local fam {
        foreach l of local link {

            twopm (`c'_costs_trim = i.malnut i.fu_year_start age i.sex i.diaggrp i.death_in_year) (`c'_costs_trim = i.malnut i.fu_year_start age i.sex i.diaggrp i.death_in_year),firstpart(logit) secondpart(glm, family(`f') link(`l')) vce(cluster LopNr)

            * Predicted mean
            predict `c'_yhat_`f'_`l'

            * Prediction error
            gen `c'_err_`f'_`l' = `c'_costs_trim - `c'_yhat_`f'_`l'
			quietly summ `c'_err_`f'_`l'
			local me= r(mean)

            * Absolute error
            gen `c'_abs_`f'_`l' = abs(`c'_err_`f'_`l')
			quietly summ `c'_abs_`f'_`l'
			local mae= r(mean)

            * Squared error
            gen `c'_sq_`f'_`l' = (`c'_err_`f'_`l')^2

            * RMSE
            quietly summ `c'_sq_`f'_`l'
            local rmse= sqrt(r(mean))

frame post model ("`c'") ("`f'") ( "`l'") ("`me'") ("`mae'") ("`rmse'")
        }
    }
}


frame model:list 



frame model: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/svedem_glm_model_performance.xlsx",  firstrow(varlabels) sheet("model",replace)



* gaussian with identity link is reasonable
*check decile plot

local costs total drug ov sv sol
foreach c of local costs {
twopm (`c'_costs_trim = i.malnut i.fu_year_start age i.sex i.diaggrp i.death_in_year) (`c'_costs_trim = i.malnut i.fu_year_start age i.sex i.diaggrp i.death_in_year),firstpart(logit) secondpart(glm, family(gaussian) link(identity)) vce(cluster LopNr)

predict `c'_yhat

xtile `c'_decile=`c'_yhat,n(10)
egen `c'_mean_pred=mean(`c'_yhat), by(`c'_decile)
egen `c'_mean_obs=mean(`c'_costs_trim), by(`c'_decile)

}



* scatter plot for deciles
scatter total_mean_pred total_mean_obs, mcolor(black) name(g1, replace)
scatter drug_mean_pred drug_mean_obs, mcolor(black) name(g2, replace)
scatter ov_mean_pred ov_mean_obs, mcolor(black) name(g3, replace)
scatter sv_mean_pred sv_mean_obs, mcolor(black) name(g4, replace)
scatter sol_mean_pred sol_mean_obs, mcolor(black) name(g5, replace)

graph combine g1 g2 g3 g4 g5, cols(2) 

graph drop _all


* predicted vs observed histograms
hist total_yhat, name(total_h1, replace)
hist total_costs_trim, name(total_h2, replace)

graph combine total_h1 total_h2


hist drug_yhat, name(drug_h1, replace)
hist drug_costs_trim, name(drug_h2, replace)

graph combine drug_h1 drug_h2


hist sv_yhat, name(sv_h1, replace)
hist sv_costs_trim, name(sv_h2, replace)

graph combine sv_h1 sv_h2

hist ov_yhat, name(ov_h1, replace)
hist ov_costs_trim, name(ov_h2, replace)

graph combine ov_h1 ov_h2


hist sol_yhat, name(sol_h1, replace)
hist sol_costs_trim, name(sol_h2, replace)

graph combine sol_h1 sol_h2


graph drop _all

drop total_yhat


**# 2. Check important predictors
twopm (total_costs_trim = i.malnut i.fu_year_start age i.sex i.diaggrp i.state_base i.sabo i.mi i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.death_in_year) (total_costs_trim = i.malnut i.fu_year_start age i.sex i.diaggrp i.state_base i.sabo i.mi i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.death_in_year),firstpart(logit) secondpart(glm, family(gaussian) link(identity)) vce(cluster LopNr)

predict total_yhat
hist total_yhat, name(h1, replace)
hist total_costs_trim, name(h2, replace)

graph combine h1 h2

graph drop _all
drop total_yhat total_decile total_mean_pred total_mean_obs


predict total_yhat

xtile total_decile=total_yhat,n(10)
egen total_mean_pred=mean(total_yhat), by(total_decile)
egen total_mean_obs=mean(total_costs_trim), by(total_decile)
scatter total_mean_pred total_mean_obs, mcolor(black)

graph drop _all
drop total_yhat total_decile total_mean_pred total_mean_obs



**# 3. Test interaction between malnutrition and follow-up year
twopm (total_costs_trim = i.malnut##i.fu_year_start age i.sex i.diaggrp i.state_base i.sabo i.mi i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.death_in_year) (total_costs_trim = i.malnut##i.fu_year_start age i.sex i.diaggrp i.state_base i.sabo i.mi i.hf i.stroke i.pd i.copd i.ra i.op i.cancer i.depression i.death_in_year),firstpart(logit) secondpart(glm, family(gaussian) link(identity)) vce(cluster LopNr)


predict total_yhat
hist total_yhat, name(h1, replace)
hist total_costs_trim, name(h2, replace)

graph combine h1 h2

graph drop _all


xtile total_decile=total_yhat,n(10)
egen total_mean_pred=mean(total_yhat), by(total_decile)
egen total_mean_obs=mean(total_costs_trim), by(total_decile)
scatter total_mean_pred total_mean_obs, mcolor(black)

graph drop _all
drop total_yhat

* no interaction between malnutrition and fu_year for the likelihod of incurring costs
