## 0. RESET & LIBRARIES ----

# (Optional but recommended)
rm(list = ls())
gc()

# Core libraries
library(tidyverse)
library(haven)
library(lubridate)
library(readxl)
library(table1)
library(mice)
library(nnet)
library(flexsurv)
library(boot)
library(forcats)
library(ggpubr)
library(scales)
library(Hmisc)    # label()
library(broom)
library(writexl)

## 0.1 Set working directory & load SNAC-K object ----
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-KarolinskaInstitutet/GRP_Economic evaluations of malnutrition in Swedish older adults (KI Livsmedelsverket) - Documents/Individal level data analysis/Data/")

## 0.2 Quick diagnostics after loading ----
cat(">>> After loading nutri_hcru_cov:\n")

cat("\nSex (raw coding):\n")
print(table(nutri_hcru_cov$sex, useNA = "ifany"))

cat("\nMNA (raw coding):\n")
print(table(nutri_hcru_cov$mna_screening1_cat, useNA = "ifany"))

cat("\nGLIM (raw coding):\n")
print(table(nutri_hcru_cov$glim_malnutr1, useNA = "ifany"))

cat("\nFollow-up time (fu) summary:\n")
print(summary(nutri_hcru_cov$fu))

## 1. BASELINE CHARACTERISTICS ----

# 1.1 Variable labels (for table1)
label(nutri_hcru_cov$age1)               <- "Age"
label(nutri_hcru_cov$sex)                <- "Sex"
label(nutri_hcru_cov$education)          <- "Education"
label(nutri_hcru_cov$lives1)             <- "Living alone"
label(nutri_hcru_cov$tobacco1)           <- "Smoking status"
label(nutri_hcru_cov$mna_screening1_cat) <- "Nutritional status (MNA)"
label(nutri_hcru_cov$glim_malnutr1)      <- "Malnutrition (GLIM)"
label(nutri_hcru_cov$bmi_cat_w1)         <- "BMI category"
label(nutri_hcru_cov$mi)                 <- "Ischemic heart disease"
label(nutri_hcru_cov$hf)                 <- "Heart failure"
label(nutri_hcru_cov$stroke)             <- "Stroke"
label(nutri_hcru_cov$pd)                 <- "Parkinson's disease"
label(nutri_hcru_cov$copd)               <- "Chronic obstructive pulmonary disease"
label(nutri_hcru_cov$ra)                 <- "Rheumatoid arthritis"
label(nutri_hcru_cov$op)                 <- "Osteoporosis"
label(nutri_hcru_cov$cancer)             <- "Cancer"
label(nutri_hcru_cov$depression)         <- "Depression"
label(nutri_hcru_cov$dementia)           <- "Dementia"
label(nutri_hcru_cov$smell_imp)          <- "Loss of smell"
label(nutri_hcru_cov$taste_imp)          <- "Loss of taste"
label(nutri_hcru_cov$chew_imp)           <- "Difficulty with chewing food"
label(nutri_hcru_cov$poor_appetite)      <- "Poor appetite"

# 1.2 Make key categorical vars explicit factors with labels
nutri_hcru_cov <- nutri_hcru_cov %>%
  mutate(
    sex = factor(sex, levels = c(1, 2),
                 labels = c("Male", "Female")),
    education = factor(education,
                       levels = c(1, 2, 3),
                       labels = c("Elementary", "High school", "University")),
    lives1 = factor(lives1,
                    levels = c(1, 2),
                    labels = c("Not alone", "Alone")),
    tobacco1 = factor(tobacco1,
                      levels = c(1, 2, 3),
                      labels = c("Never","Former","Current")),
    mna_screening1_cat = factor(mna_screening1_cat,
                                levels = c(1, 2, 3),
                                labels = c("Normal","At risk","Malnourished")),
    glim_malnutr1 = factor(glim_malnutr1,
                           levels = c(1, 2, 3),
                           labels = c("No malnutrition",
                                      "Moderate malnutrition",
                                      "Severe malnutrition")),
    bmi_cat_w1 = factor(bmi_cat_w1),
    mi = factor(mi), hf = factor(hf), stroke = factor(stroke),
    pd = factor(pd), copd = factor(copd), ra = factor(ra), op = factor(op),
    cancer = factor(cancer), depression = factor(depression),
    dementia = factor(dementia),
    smell_imp = factor(smell_imp), taste_imp = factor(taste_imp),
    chew_imp = factor(chew_imp)
  )

## 1.3 Baseline dataset: one row per person with fu > 0
baseline_df <- nutri_hcru_cov %>%
  filter(fu > 0) %>%
  mutate(age_group = factor(ifelse(age1 < 78, "60-72", "78+"))) %>%
  distinct(lopnr, .keep_all = TRUE)

# Strip haven 'labelled' class from numeric vars only
baseline_df <- baseline_df %>%
  mutate(across(where(~ "labelled" %in% class(.x) && is.numeric(.x)),
                haven::zap_labels))

## Diagnostic
cat(">>> In baseline_df (after distinct):\n")
cat("Sex:\n");  print(table(baseline_df$sex, useNA = "ifany"))
cat("MNA:\n");  print(table(baseline_df$mna_screening1_cat, useNA = "ifany"))

## 1.4 Table 1
table1_df <- table1::table1(
  ~ age1 + sex + education + lives1 + tobacco1 +
    mna_screening1_cat + glim_malnutr1 +
    bmi_cat_w1 + mi + hf + stroke + pd + copd + ra + op +
    cancer + depression + dementia +
    smell_imp + taste_imp + chew_imp | age_group,
  data = baseline_df
) %>%
  as.data.frame()

colnames(table1_df)[1] <- "Characteristics"
class(table1_df) <- "data.frame"

# Drop % sign 
table1_df[] <- lapply(table1_df, function(x) gsub("%","",x, fixed = TRUE))

## 2. MICE IMPUTATION ----
nutri_hcru_cov <- nutri_hcru_cov %>%
  mutate(across(everything(), ~ {
    if ("labelled" %in% class(.x)) haven::zap_labels(.x) else .x
  }))

# 2.2 Build MI dataset
mi_clean <- nutri_hcru_cov %>%
  filter(fu > 0) %>%
  distinct(lopnr, .keep_all = TRUE) %>%
  dplyr::select(
    lopnr, mna_screening1_cat, glim_malnutr1,
    age1, sex, bmi_cat_w1, education, lives1, tobacco1,
    mi, hf, stroke, pd_dem, copd, ra_op, cancer, depression,
    smell_taste_imp, chew_imp, fu, death
  ) %>%
  mutate(
    across(
      c(
        mna_screening1_cat, glim_malnutr1, sex, bmi_cat_w1,
        education, lives1, tobacco1,
        mi, hf, stroke, pd_dem, copd, ra_op, cancer, depression,
        smell_taste_imp, chew_imp
      ),
      ~ factor(.x)
    )
  )

# 2.3 Initialise MICE and set methods
imp0 <- mice(mi_clean, maxit = 0, seed = 2025)
predM <- imp0$predictorMatrix
meth  <- imp0$method

# Do NOT impute ID, fu, death
predM[, c("lopnr", "fu", "death")] <- 0
meth[c("lopnr","fu","death")]      <- ""

# categorical variables -> multinomial logistic
cat_vars <- c(
  "mna_screening1_cat","glim_malnutr1","sex","bmi_cat_w1",
  "education","lives1","tobacco1",
  "mi","hf","stroke","pd_dem","copd","ra_op",
  "cancer","depression","smell_taste_imp","chew_imp"
)
meth[cat_vars] <- "polyreg"

# numeric -> predictive mean matching
meth["age1"] <- "pmm"

# 2.4 Run MICE
set.seed(2025)
nutri_hcru_cov_imp <- mice(
  data            = mi_clean,
  predictorMatrix = predM,
  method          = meth,
  m               = 5,
  maxit           = 20,
  seed            = 2025
)

## 2.5 Diagnostics
cat(">>> In imputed data (first completed dataset):\n")
comp1 <- complete(nutri_hcru_cov_imp, 1)

cat("MNA:\n");  print(table(comp1$mna_screening1_cat))
cat("GLIM:\n"); print(table(comp1$glim_malnutr1))
cat("Sex:\n");  print(table(comp1$sex))

#### 3. Factors associated with malnutrition (multinomial models) ####
## 3.0 Take one completed imputed dataset as analysis base ----
## (Uses multiple imputation to fill missing covariates, but analyses are
##  run on the first completed dataset for stability and simplicity.)
analysis_df <- complete(nutri_hcru_cov_imp, 1)

## Clean classes: drop 'labelled' and enforce factor/numeric types ----
analysis_df <- analysis_df %>%
  mutate(across(everything(), ~ {
    x <- .
    if ("labelled" %in% class(x)) x <- haven::zap_labels(x)
    attributes(x) <- NULL
    x
  })) %>%
  mutate(
    mna_screening1_cat = factor(mna_screening1_cat),   # 1/2/3
    glim_malnutr1      = factor(glim_malnutr1),        # 1/2/3
    sex                = factor(sex),
    education          = factor(education),
    lives1             = factor(lives1),
    tobacco1           = factor(tobacco1),
    bmi_cat_w1         = factor(bmi_cat_w1),
    mi                 = factor(mi),
    hf                 = factor(hf),
    stroke             = factor(stroke),
    pd_dem             = factor(pd_dem),
    copd               = factor(copd),
    ra_op              = factor(ra_op),
    cancer             = factor(cancer),
    depression         = factor(depression),
    smell_taste_imp    = factor(smell_taste_imp),
    chew_imp           = factor(chew_imp),
    age1               = as.numeric(age1)
  )

## 3.1 Quick diagnostics – make sure we still have 3-category outcomes ----
cat(">>> Section 3 diagnostics on analysis_df:\n")
cat("MNA (mna_screening1_cat):\n");      print(table(analysis_df$mna_screening1_cat, useNA = "ifany"))
cat("GLIM (glim_malnutr1):\n");          print(table(analysis_df$glim_malnutr1, useNA = "ifany"))
cat("Sex:\n");                           print(table(analysis_df$sex, useNA = "ifany"))

## 3.2 Covariate list & pretty labels ----
covarlist <- c(
  "education","lives1","tobacco1","bmi_cat_w1",
  "mi","hf","stroke","pd_dem","copd","ra_op","cancer",
  "depression","smell_taste_imp","chew_imp"
)

var_names <- c(
  "education"       = "Education",
  "lives1"          = "Living alone",
  "tobacco1"        = "Smoking status",
  "bmi_cat_w1"      = "BMI category",
  "mi"              = "Myocardial infarction",
  "hf"              = "Heart failure",
  "stroke"          = "Stroke",
  "pd_dem"          = "Parkinson's disease or dementia",
  "copd"            = "COPD",
  "ra_op"           = "Rheumatoid arthritis or osteoporosis",
  "cancer"          = "Cancer",
  "depression"      = "Depression",
  "smell_taste_imp" = "Loss of smell or taste",
  "chew_imp"        = "Difficulty with chewing food"
)

## 3.3 Helper: extract OR, CI, p from multinom() for one outcome & one predictor ----
extract_multinom <- function(fit, scale_name, pred_name) {
  s <- summary(fit)
  coefs <- s$coefficients      # rows = outcome levels vs ref, cols = predictors
  ses   <- s$standard.errors
  
  out_levels <- rownames(coefs)   # e.g., "2","3"
  terms      <- colnames(coefs)   # e.g., "(Intercept)","education2","age1","sex2"
  
  out <- list()
  idx <- 1
  
  for (i in seq_along(out_levels)) {
    lev <- out_levels[i]
    for (j in seq_along(terms)) {
      term_j <- terms[j]
      
      # Skip intercept and covariates not of interest
      if (term_j %in% c("(Intercept)", "age1") || grepl("^sex", term_j)) next
      # Keep only the terms for this specific predictor (e.g. 'education2','education3')
      if (!startsWith(term_j, pred_name)) next
      
      beta <- coefs[i, j]
      se   <- ses[i, j]
      z    <- beta / se
      pval <- 2 * pnorm(abs(z), lower.tail = FALSE)
      
      OR      <- exp(beta)
      OR_low  <- exp(beta - 1.96 * se)
      OR_high <- exp(beta + 1.96 * se)
      
      out[[idx]] <- data.frame(
        scale     = scale_name,
        out_level = as.integer(lev),    # 2 or 3
        pred      = pred_name,
        term      = term_j,
        OR_CI     = sprintf("%.2f (%.2f–%.2f)", OR, OR_low, OR_high),
        p         = ifelse(pval < 0.001, "<0.001", sprintf("%.3f", pval)),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  
  if (length(out) == 0) {
    return(NULL)
  } else {
    do.call(rbind, out)
  }
}

## 3.4 Run multinomial models for each outcome (MNA, GLIM) and each predictor ----
nutri_pred_sum <- data.frame(
  scale     = character(0),
  out_level = integer(0),
  pred      = character(0),
  term      = character(0),
  OR_CI     = character(0),
  p         = character(0),
  stringsAsFactors = FALSE
)

for (s in c("mna_screening1_cat", "glim_malnutr1")) {
  for (v in covarlist) {
    
    cat("Running multinom for:", s, "~", v, "+ age1 + sex\n")
    
    # Select needed columns and drop rows with missing data
    df_mod <- analysis_df %>%
      dplyr::select(all_of(c(s, "age1", "sex", v))) %>%
      tidyr::drop_na()
    
    # Need at least 2 outcome levels for multinom to work
    if (dplyr::n_distinct(df_mod[[s]]) < 2) {
      cat("  -> Skipped (less than 2 outcome levels in", s, "after complete-cases)\n")
      next
    }
    
    fml <- as.formula(paste0(s, " ~ ", v, " + age1 + sex"))
    
    fit <- nnet::multinom(fml, data = df_mod, trace = FALSE)
    
    tmp <- extract_multinom(fit, scale_name = s, pred_name = v)
    if (!is.null(tmp)) {
      nutri_pred_sum <- dplyr::bind_rows(nutri_pred_sum, tmp)
    }
  }
}

## 3.5 Format results table ----
nutri_pred_results <- nutri_pred_sum %>%
  mutate(
    pred_label = dplyr::recode(pred, !!!var_names),
    `Nutritional status` = dplyr::case_when(
      scale == "mna_screening1_cat" & out_level == 2 ~ "At risk",
      scale == "mna_screening1_cat" & out_level == 3 ~ "Malnourished",
      scale == "glim_malnutr1"      & out_level == 2 ~ "Moderate malnutrition",
      scale == "glim_malnutr1"      & out_level == 3 ~ "Severe malnutrition",
      TRUE ~ NA_character_
    )
  ) %>%
  rowwise() %>%
  mutate(
    # Remove the predictor name from the term, leaving only the category index (e.g., "2","3")
    suffix = gsub(paste0("^", pred), "", term),
    suffix = ifelse(suffix %in% c("", "1"), "", paste0(" (cat ", suffix, ")")),
    Factor = paste0(pred_label, suffix),
    `Nutritional assessment scale` =
      ifelse(scale == "glim_malnutr1",
             "Malnutrition (GLIM)",
             "Nutritional status (MNA)")
  ) %>%
  ungroup() %>%
  dplyr::select(
    `Nutritional assessment scale`,
    `Nutritional status`,
    Factor,
    `OR (95% CI)` = OR_CI,
    `P-value`     = p
  )

cat(">>> Finished Section 3. Preview of nutri_pred_results:\n")
print(head(nutri_pred_results, 10))

## 4. HCRU VISITS & COSTS BY MNA ----
annual_hcru_95ci <- function(data, indices, scale, outcome, time) {
  d <- data[indices, ]
  
  d <- d %>%
    group_by(lopnr, .data[[scale]], .data[[time]]) %>%
    summarise(
      sum_outcome = sum(.data[[outcome]], na.rm = TRUE),
      .groups     = "drop"
    )
  
  tmp <- d %>%
    group_by(.data[[scale]]) %>%
    summarise(
      sum_hcru   = sum(sum_outcome),
      sum_length = sum(.data[[time]]),
      .groups    = "drop"
    ) %>%
    mutate(annual_hcru = sum_hcru / sum_length)
  
  return(tmp$annual_hcru)
}

# 4.2 Bootstrap visits and costs (MNA)
set.seed(2025)

# Visits
mna_ov_visits_95ci <- tidy(
  boot(
    data = nutri_hcru_cov %>%
      filter(fu > 0,
             !is.na(mna_screening1_cat),
             !is.infinite(ov_num),
             !is.na(ov_num)),
    statistic = annual_hcru_95ci,
    scale     = "mna_screening1_cat",
    outcome   = "ov_num",
    time      = "fu",
    R         = 1000,
    parallel  = "multicore",
    ncpus     = 4
  ),
  conf.int  = TRUE,
  conf.method = "perc"
) %>% cbind(cat = c(1, 2, 3))

mna_sv_visits_95ci <- tidy(
  boot(
    data = nutri_hcru_cov %>%
      filter(fu > 0,
             !is.na(mna_screening1_cat),
             !is.infinite(sv_num),
             !is.na(sv_num)),
    statistic = annual_hcru_95ci,
    scale     = "mna_screening1_cat",
    outcome   = "sv_num",
    time      = "fu",
    R         = 1000,
    parallel  = "multicore",
    ncpus     = 4
  ),
  conf.int  = TRUE,
  conf.method = "perc"
) %>% cbind(cat = c(1, 2, 3))

mna_pv_visits_95ci <- tidy(
  boot(
    data = nutri_hcru_cov %>%
      filter(pv_fu > 0,
             !is.na(mna_screening1_cat),
             !is.infinite(pv_num),
             !is.na(pv_num)),
    statistic = annual_hcru_95ci,
    scale     = "mna_screening1_cat",
    outcome   = "pv_num",
    time      = "pv_fu",
    R         = 1000,
    parallel  = "multicore",
    ncpus     = 4
  ),
  conf.int  = TRUE,
  conf.method = "perc"
) %>% cbind(cat = c(1, 2, 3))

# Costs
mna_ov_costs_95ci <- tidy(
  boot(
    data = nutri_hcru_cov %>%
      filter(fu > 0,
             !is.na(mna_screening1_cat),
             !is.infinite(ov_cost),
             !is.na(ov_cost)),
    statistic = annual_hcru_95ci,
    scale     = "mna_screening1_cat",
    outcome   = "ov_cost",
    time      = "fu",
    R         = 1000,
    parallel  = "multicore",
    ncpus     = 4
  ),
  conf.int  = TRUE,
  conf.method = "perc"
) %>% cbind(cat = c(1, 2, 3))

mna_sv_costs_95ci <- tidy(
  boot(
    data = nutri_hcru_cov %>%
      filter(fu > 0,
             !is.na(mna_screening1_cat),
             !is.infinite(sv_cost),
             !is.na(sv_cost)),
    statistic = annual_hcru_95ci,
    scale     = "mna_screening1_cat",
    outcome   = "sv_cost",
    time      = "fu",
    R         = 1000,
    parallel  = "multicore",
    ncpus     = 4
  ),
  conf.int  = TRUE,
  conf.method = "perc"
) %>% cbind(cat = c(1, 2, 3))

mna_pv_costs_95ci <- tidy(
  boot(
    data = nutri_hcru_cov %>%
      filter(pv_fu > 0,
             !is.na(mna_screening1_cat),
             !is.infinite(pv_cost),
             !is.na(pv_cost)),
    statistic = annual_hcru_95ci,
    scale     = "mna_screening1_cat",
    outcome   = "pv_cost",
    time      = "pv_fu",
    R         = 1000,
    parallel  = "multicore",
    ncpus     = 4
  ),
  conf.int  = TRUE,
  conf.method = "perc"
) %>% cbind(cat = c(1, 2, 3))

# 4.3 Plot------
mna_labels <- c("Normal","At risk","Malnourished")

for (i in c("ov","sv","pv")) {
  temp_num <- get(paste0("mna_", i, "_visits_95ci")) %>%
    rename_with(~ "cat", ncol(.)) %>%
    mutate(
      scale = "Number",
      cat   = factor(cat, levels = 1:3,
                     labels = stringr::str_wrap(mna_labels, width = 15))
    ) %>%
    rename(est = statistic, lb = conf.low, ub = conf.high)
  
  temp_cost <- get(paste0("mna_", i, "_costs_95ci")) %>%
    rename_with(~ "cat", ncol(.)) %>%
    mutate(
      scale = "Costs",
      cat   = factor(cat, levels = 1:3,
                     labels = stringr::str_wrap(mna_labels, width = 15))
    ) %>%
    rename(est = statistic, lb = conf.low, ub = conf.high)
  
  assign(
    paste0(i, "_visits_plot"),
    ggplot(temp_num, aes(x = cat, y = est)) +
      geom_col(width = 0.6) +
      geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2) +
      labs(x = "", y = "Annual number of visits and 95% CI") +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(),
            axis.line = element_line(color = "black"),
            panel.border = element_blank(),
            axis.title.x = element_text(size = 10))
  )
  
  assign(
    paste0(i, "_costs_plot"),
    ggplot(temp_cost, aes(x = cat, y = est)) +
      geom_col(width = 0.6) +
      geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2) +
      scale_y_continuous(labels = comma) +
      labs(x = "", y = "Annual costs in 2024 SEK and 95% CI") +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(),
            axis.line = element_line(color = "black"),
            panel.border = element_blank(),
            axis.title.x = element_text(size = 10))
  )
}

# Save in *current* working directory
pdf("figure_1_combined_visits.pdf", width = 10, height = 10)
ggarrange(
  pv_visits_plot, ov_visits_plot, sv_visits_plot,
  pv_costs_plot,  ov_costs_plot,  sv_costs_plot,
  ncol = 3, nrow = 2,
  labels = c("A. Primary care", "B. Outpatient care","C. Inpatient care",
             "D. Primary care","E. Outpatient care","F. Inpatient care")
)
dev.off()

## 5. MORTALITY MODELS ----
# 5.1 Fit models in each imputed dataset
# Unadjusted MNA
mna_mort <- with(
  nutri_hcru_cov_imp,
  flexsurvreg(Surv(fu, death) ~ mna_screening1_cat + age1 + sex,
              dist = "weibullPH")
)

# Adjusted MNA
mna_mort_adj <- with(
  nutri_hcru_cov_imp,
  flexsurvreg(
    Surv(fu, death) ~ mna_screening1_cat + age1 +
      education + lives1 + tobacco1 + mi + hf + stroke +
      pd_dem + copd + ra_op + cancer + depression +
      smell_taste_imp + chew_imp,
    dist = "weibullPH"
  )
)

# Unadjusted GLIM
glim_mort <- with(
  nutri_hcru_cov_imp,
  flexsurvreg(Surv(fu, death) ~ glim_malnutr1 + age1 + sex,
              dist = "weibullPH")
)

# Adjusted GLIM
glim_mort_adj <- with(
  nutri_hcru_cov_imp,
  flexsurvreg(
    Surv(fu, death) ~ glim_malnutr1 + age1 +
      education + lives1 + tobacco1 + mi + hf + stroke +
      pd_dem + copd + ra_op + cancer + depression +
      smell_taste_imp + chew_imp,
    dist = "weibullPH"
  )
)
# 5.2 Pool and format results
# Helper to pool flexsurvreg outputs into HR table
pool_flexsurv <- function(mids_flex) {
  s <- summary(pool(mids_flex), conf.int = TRUE, conf.level = 0.95) %>%
    as.data.frame()
  
  s %>%
    filter(term != "(Intercept)") %>%
    mutate(
      HR    = exp(estimate),
      HR_lo = exp(`2.5 %`),
      HR_hi = exp(`97.5 %`),
      CI    = paste0(
        format(round(HR,    2), nsmall = 2), " (",
        format(round(HR_lo, 2), nsmall = 2), "–",
        format(round(HR_hi, 2), nsmall = 2), ")"
      ),
      p = ifelse(
        p.value < 0.001, "<0.001",
        as.character(format(round(p.value, 3), nsmall = 3))
      )
    ) %>%
    dplyr::select(term, CI, p)
}

mna_mort_pool      <- pool_flexsurv(mna_mort)      %>% mutate(model = "Unadjusted", scale = "Nutritional status (MNA)")
mna_mort_adj_pool  <- pool_flexsurv(mna_mort_adj)  %>% mutate(model = "Adjusted",   scale = "Nutritional status (MNA)")
glim_mort_pool     <- pool_flexsurv(glim_mort)     %>% mutate(model = "Unadjusted", scale = "Malnutrition (GLIM)")
glim_mort_adj_pool <- pool_flexsurv(glim_mort_adj) %>% mutate(model = "Adjusted",   scale = "Malnutrition (GLIM)")

nutri_mort_results <- bind_rows(
  mna_mort_pool,
  mna_mort_adj_pool,
  glim_mort_pool,
  glim_mort_adj_pool
) %>%
  filter(grepl("mna_", term) | grepl("glim_", term)) %>%
  mutate(
    term = gsub("mna_screening1_cat", "", term),
    term = gsub("glim_malnutr1",      "", term),
    term = trimws(term),
    term = dplyr::recode(term,
                         "2" = "At risk",
                         "3" = "Malnourished",
                         "Moderate malnutrition" = "Moderate malnutrition",
                         "Severe malnutrition"   = "Severe malnutrition")
  ) %>%
  dplyr::select(
    scale,
    model,
    term,
    `HR (95% CI)` = CI,
    `P-value`     = p
  )

writexl::write_xlsx(
  list(
    "table1" = table1_df,
    "pred"   = nutri_pred_results,   # from Section 3
    "mort"   = nutri_mort_results
  ),
  path = "results_YM.xlsx"
)