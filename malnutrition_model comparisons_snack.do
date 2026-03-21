cd "/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/"

use "Organized data/snack_nutri_hcru_cov_xx.dta",clear


* fu_year>=11 is sparse, merge fu_year>=11 to a single category
replace fu_year=11 if fu_year>=11


**# 1. Compare alternative specifications in two-part models
capture frame drop model
frame create model str30 outcome str30 family str30 link str30 me str30 mae str30 rmse 

* total costs and primary care costs
local costs total pv
local fam gaussian igaussian gamma
local link identity log

foreach c of local costs {
    foreach f of local fam {
        foreach l of local link {

            twopm (`c'_cost_trim = i.mna_screening1_cat i.fu_year age1 i.sex i.pv_death_in_year) (`c'_cost_trim = i.mna_screening1_cat i.fu_year age1 i.sex i.pv_death_in_year) if `c'_cost_trim!=. ,firstpart(logit) secondpart(glm, family(`f') link(`l')) vce(cluster lopnr)

            * Predicted mean
            predict `c'_yhat_`f'_`l'

            * Prediction error
            gen `c'_err_`f'_`l' = `c'_cost_trim - `c'_yhat_`f'_`l'
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


* specialist care costs
local costs ov sv
local fam gaussian igaussian gamma
local link identity log

foreach c of local costs {
    foreach f of local fam {
        foreach l of local link {

            twopm (`c'_cost_trim = i.mna_screening1_cat i.fu_year age1 i.sex i.death_in_year) (`c'_cost_trim = i.mna_screening1_cat i.fu_year age1 i.sex i.death_in_year) if `c'_cost_trim!=. ,firstpart(logit) secondpart(glm, family(`f') link(`l')) vce(cluster lopnr)

            * Predicted mean
            predict `c'_yhat_`f'_`l'

            * Prediction error
            gen `c'_err_`f'_`l' = `c'_cost_trim - `c'_yhat_`f'_`l'
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



frame model: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/snack_glm_model_performance.xlsx",  firstrow(varlabels) sheet("model",replace)



* gamma with identity link is reasonable
*check decile plot
* total costs and primary care costs
local costs total pv
foreach c of local costs {
twopm (`c'_cost_trim = i.mna_screening1_cat i.fu_year age1 i.sex i.pv_death_in_year) (`c'_cost_trim = i.mna_screening1_cat i.fu_year age1 i.sex i.pv_death_in_year) if `c'_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)

predict `c'_yhat

xtile `c'_decile=`c'_yhat,n(10)
egen `c'_mean_pred=mean(`c'_yhat), by(`c'_decile)
egen `c'_mean_obs=mean(`c'_cost_trim), by(`c'_decile)

}


* specialist care costs
local costs ov sv
foreach c of local costs {
twopm (`c'_cost_trim = i.mna_screening1_cat i.fu_year age1 i.sex i.death_in_year) (`c'_cost_trim = i.mna_screening1_cat i.fu_year age1 i.sex i.death_in_year) if `c'_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)

predict `c'_yhat

xtile `c'_decile=`c'_yhat,n(10)
egen `c'_mean_pred=mean(`c'_yhat), by(`c'_decile)
egen `c'_mean_obs=mean(`c'_cost_trim), by(`c'_decile)

}



* scatter plot for deciles
scatter total_mean_pred total_mean_obs, mcolor(black) name(g1, replace)
scatter ov_mean_pred ov_mean_obs, mcolor(black) name(g2, replace)
scatter sv_mean_pred sv_mean_obs, mcolor(black) name(g3, replace)
scatter pv_mean_pred pv_mean_obs, mcolor(black) name(g4, replace)

graph combine g1 g2 g3 g4, cols(2) 

graph drop _all


* predicted vs observed histograms
hist sv_yhat, name(sv_h1, replace)
hist sv_cost_trim, name(sv_h2, replace)

graph combine sv_h1 sv_h2

hist ov_yhat, name(ov_h1, replace)
hist ov_cost_trim, name(ov_h2, replace)

graph combine ov_h1 ov_h2


hist pv_yhat, name(pv_h1, replace)
hist pv_cost_trim, name(pv_h2, replace)

graph combine pv_h1 pv_h2


hist total_yhat, name(total_h1, replace)
hist total_cost_trim, name(total_h2, replace)

graph combine total_h1 total_h2

graph drop _all

drop total_yhat


**# 2. Check important predictors
twopm (total_cost_trim = i.mna_screening1_cat i.fu_year age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.poor_appetite i.pv_death_in_year) (total_cost_trim = i.mna_screening1_cat i.fu_year age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.poor_appetite i.pv_death_in_year) if total_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)


predict total_yhat
hist total_yhat, name(h1, replace)
hist total_cost_trim, name(h2, replace)

graph combine h1 h2

graph drop _all

drop total_yhat


**# 3. Test interaction between malnutrition and follow-up year
twopm (total_cost_trim = i.mna_screening1_cat##i.fu_year age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.poor_appetite i.pv_death_in_year) (total_cost_trim = i.mna_screening1_cat##i.fu_year age1 i.sex i.tobacco1 i.lives1 i.mi i.hf i.ra_op i.cancer i.depression i.pd_dem i.poor_appetite i.pv_death_in_year) if total_cost_trim!=. ,firstpart(logit) secondpart(glm, family(gamma) link(identity)) vce(cluster lopnr)


predict total_yhat
hist total_yhat, name(h1, replace)
hist total_cost_trim, name(h2, replace)

graph combine h1 h2

graph drop _all

drop total_yhat

* interaction with at risk, no significant interaction with malnurished, no interaction for incurring costs (costs>0)
