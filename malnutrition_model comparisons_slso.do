cd "/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/"

use "Organized data/slso_nutri_hcru_cov_xx.dta",clear

* very few people have 0 vardtid_cost or total costs

**# 1. Compare alternative specifications in glm or two-part models
capture frame drop model
frame create model str30 outcome str30 family str30 link str30 me str30 mae str30 rmse 


**# 1.1. GLMs for costs that in theory should not be 0
local costs total totalsv geri1st
local fam gaussian igaussian gamma
local link identity log

foreach c of local costs {
    foreach f of local fam {
        foreach l of local link {

            glm `c'_cost_trim i.mna_cat age i.sex if `c'_cost_trim!=0, family(`f') link(`l')

            * Predicted mean
            predict `c'_yh_`f'_`l'

            * Prediction error
            gen `c'_err_`f'_`l' = `c'_cost_trim - `c'_yh_`f'_`l'
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


**# 1.2. Two-part models for costs that can be 0
local costs sol pv ov gerireadm othersv
local fam gaussian igaussian gamma
local link identity log

foreach c of local costs {
    foreach f of local fam {
        foreach l of local link {

            twopm (`c'_cost_trim = i.mna_cat age i.sex) (`c'_cost_trim = i.mna_cat age i.sex),firstpart(logit) secondpart(glm, family(`f') link(`l'))

            * Predicted mean
            predict `c'_yh_`f'_`l'

            * Prediction error
            gen `c'_err_`f'_`l' = `c'_cost_trim - `c'_yh_`f'_`l'
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



frame model: export excel using "Malnutrition statistical analysis/Malnutrition-study/results/slso_glm_model_performance.xlsx",  firstrow(varlabels) sheet("model",replace)



* gamma with log link is reasonable
*check decile plot

local costs total totalsv geri1st
foreach c of local costs {
glm `c'_cost_trim i.mna_cat age i.sex if `c'_cost_trim!=0, family(gamma) link(log)
predict `c'_yhat

xtile `c'_decile=`c'_yhat,n(10)
egen `c'_mean_pred=mean(`c'_yhat), by(`c'_decile)
egen `c'_mean_obs=mean(`c'_cost_trim), by(`c'_decile)

}


local costs sol pv ov gerireadm othersv
foreach c of local costs {
 twopm (`c'_cost_trim = i.mna_cat age i.sex) (`c'_cost_trim = i.mna_cat age i.sex),firstpart(logit) secondpart(glm, family(gamma) link(log))
 
predict `c'_yhat

xtile `c'_decile=`c'_yhat,n(10)
egen `c'_mean_pred=mean(`c'_yhat), by(`c'_decile)
egen `c'_mean_obs=mean(`c'_cost_trim), by(`c'_decile)

}


* scatter plot for deciles
scatter totalsv_mean_pred totalsv_mean_obs, mcolor(black) name(g1, replace)
scatter geri1st_mean_pred geri1st_mean_obs, mcolor(black) name(g2, replace)

scatter sol_mean_pred sol_mean_obs, mcolor(black) name(g3, replace)
scatter pv_mean_pred pv_mean_obs, mcolor(black) name(g4, replace)
scatter ov_mean_pred ov_mean_obs, mcolor(black) name(g5, replace)
scatter gerireadm_mean_pred gerireadm_mean_obs, mcolor(black) name(g6, replace)
scatter othersv_mean_pred othersv_mean_obs, mcolor(black) name(g7, replace)


graph combine g1 g2 g3 g4 g5 g6 g7, cols(4) 

graph drop _all


* predicted vs observed histograms


hist sol_yhat, name(sol_h1, replace)
hist sol_cost_trim, name(sol_h2, replace)

graph combine sol_h1 sol_h2



hist pv_yhat, name(pv_h1, replace)
hist pv_cost_trim, name(pv_h2, replace)

graph combine pv_h1 pv_h2


hist total_yhat, name(total_h1, replace)
hist total_cost_trim, name(total_h2, replace)

graph combine total_h1 total_h2

graph drop _all

drop total_yhat


**# 2. Check important predictors
glm total_cost_trim i.mna_cat age i.sex i.edu_level i.civil_status i.ensamboende i.ihd i.hf i.ra i.op i.cancer i.depression i.pd i.dem if total_cost_trim>0, family(gamma) link(identity)


predict total_yhat
hist total_yhat, name(h1, replace)
hist total_cost_trim, name(h2, replace)

graph combine h1 h2

graph drop _all

drop total_yhat


glm total_cost_trim i.mna_cat age i.sex if total_cost_trim>0, family(gamma) link(log)

margins mna_cat
margins, dydx(mna_cat)


glm total_cost_trim i.mna_cat age i.sex i.civil_status i.ensamboende i.hf i.depression i.pd i.dem if total_cost_trim>0, family(gamma) link(log)

margins mna_cat
margins, dydx(mna_cat)
