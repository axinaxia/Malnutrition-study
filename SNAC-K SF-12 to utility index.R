library(tidyverse)
library(haven)
library(lubridate)
library(readxl)
library(lubridate)

setwd("/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/")

# Load datasets
base_sf12<-read_sav("SF12 wave 1-6/wave1_C1_SF12.sav") %>% 
  as.data.frame()

colnames(base_sf12)

# pain variables
pain_w1<-read_sav("SF12 wave 1-6/SNAC-K wave 1 C1 pain clean.SAV") %>% 
  as.data.frame()

# Inputs expected for calculating utility index (one row per person):
#   pf_mod     = SF-12 physical functioning (moderate activities): 1=limited a lot, 2=limited a little, 3=not limited at all ???
#   rp_limit   = SF-12 role-physical: yes/no item "limited in kind of work/activities due to physical health"
#               (recommended coding: 1=Yes (limited), 0=No (not limited))
#   re_less    = SF-12 role-emotional: yes/no item "accomplished less due to emotional problems"
#               (recommended coding: 1=Yes, 0=No)
#   sf_interf  = SF-12 social functioning interference: typically 1=all of the time ... 5=none of the time ???
#   pain_int   = SF-12 pain interference with normal work: often 1=not at all ... 5=extremely
#               (some datasets use 6 levels; handled below)
#   mh_down    = SF-12 mental health "downhearted and low": often 1=all ... 6=none ???
#   vit_energy = SF-12 vitality/energy: often 1=all ... 6=none


# 1. Convert original SNAC-K variables to input for SF-12 utility index algorithm ----
# 1.1. Physical functioning (pf_mod) ----
# ****************************************************************************************
# SF-6D question: Your health limits you in moderate activities
# 1=limited a lot, 2=limited a little, 3=not limited at all ???
# Corresponding SNAC-K variables (from SelfPSY):
# SP1H1: Does your general health limit you in moderately strenous activities, 
#        such as moving a table, vacuuming, walking in the forest, or gardening?
# SP1H2: Does your general health limit you in walking up several flights of stairs?
# 1. No, not limited at all  2. Yes, slightly limited 3. Yes, very limited
# 8=No response, 0=several options chosen
# ****************************************************************************************
table(base_sf12$SP1H1,useNA = "ifany")

base_sf12<-base_sf12 %>% 
  mutate(pf_mod=ifelse(SP1H1 %in% c(0,8),NA,SP1H1))

table(base_sf12$pf_mod,useNA = "ifany")



# 1.2. Role limitation - physical (rp_limit) ----
# ****************************************************************************************
# 1=Yes (limited), 0=No (not limited)
# Corresponding SNAC-K variables (from SelfPSY):
# SP1H3: In the last four weeks, have you accomplished less than you wanted, 
#        because of your physical health?
# 1. No 2. Yes
# 8=No response, 0=several options chosen
# Proxies:
# SP1H10	Do you have any chronic illness, accident-related trouble, handicap, or other weakness?
# 1. Yes 2. No
# SP1H11	(if you are working:) Does this illness/Do these illnesses limit your ability to work?
# 1. To a high degree 2. Somewhat 3. Not at all
# SP1H12	(if you are retired:) Does this illness/do these illnesses limit you in any activities?
# 1. To a high degree 2. Somewhat 3. Not at all
# ****************************************************************************************
table(base_sf12$SP1H3,useNA = "ifany")
table(base_sf12$SP1H10,useNA = "ifany")
table(base_sf12$SP1H11,useNA = "ifany")
table(base_sf12$SP1H12,useNA = "ifany")

base_sf12<-base_sf12 %>% 
  mutate(rp_limit=ifelse(SP1H3==8,NA,SP1H3)-1)

table(base_sf12$rp_limit,useNA = "ifany")



# 1.3. Role limitation - emotional (re_less) ----
# ****************************************************************************************
# 1=Yes (limited), 0=No (not limited)
# Corresponding SNAC-K variables (from SelfPSY):
# SP1H4: In the last four weeks, have you accomplished less than you wanted, 
#        because of emotional troubles? (such as anxiety or depression)
# 1. No 2. Yes
# 8=No response, 0=several options chosen
# ****************************************************************************************
table(base_sf12$SP1H4,useNA = "ifany")
base_sf12<-base_sf12 %>% 
  mutate(re_less=ifelse(SP1H4 %in% c(0,8),NA,SP1H4)-1)

table(base_sf12$re_less,useNA = "ifany")



# 1.4. Social functioning (sf_interf) ----
# ****************************************************************************************
# SF-6D question: Your health limits your social activities
# 1=all of the time, 2=a little of the time, 3=some of the time, 4=most of the time,
# 5=none of the time
# Corresponding SNAC-K variables (from SelfPSY):
# SP1H8: In the last four weeks, how much of the time has your physical health or 
#       emotional troubles interefered with your social activities? (such as visit relatives, friends, etc.)	
# 1. None of the time 2. A little of the time 3. Some of the time 4. A large part of the time 
# 5. Most of the time 6. All of the time
# ****************************************************************************************
table(base_sf12$SP1H8,useNA = "ifany")

base_sf12<-base_sf12 %>% 
  mutate(sf_interf=case_when(SP1H8 %in% c(0,8)~NA,
                             SP1H8==5~4,
                             SP1H8==6~5,
                             T~SP1H8))

table(base_sf12$sf_interf,useNA = "ifany")




# 1.5. Pain interference (pain_int) ----
# ****************************************************************************************
# SF-6D question: You have pain that interferes with your normal work
# 1=not at all, 2=a little bit, 3=moderately, 4=quite a bit, 5=extremely
# Corresponding SNAC-K variables (from Physician):
# PH1.0H20D: In the last 4 weeks, how much has pain affected the following states/activities?
# Daily work (including household chores)	
# 1. Not at all 2. A little 3. Moderately 4. Fairly much 5. Very much 8. No response 9. Don't know
# ****************************************************************************************
table(pain_w1$PH1H20D,useNA = "ifany")

pain_w1<-pain_w1 %>% 
  mutate(pain_int=ifelse(PH1H20D %in% c(8,9),NA,PH1H20D))

table(pain_w1$pain_int,useNA = "ifany")


# 1.6. Mental health (mh_down) ----
# ****************************************************************************************
# SF-6D question: You feel downhearted and low
# 1=none of the time, 2=a little of the time, 3=some of the time, 4=most of the time,
# 5=all of the time
# Corresponding SNAC-K variables (from SelfPSY):
# SP1H7: How much of the time in the last four weeks have you felt gloomy or sad?	
# 1. None of the time 2. A little of the time 3. Some of the time 4. A large part of the time 
# 5. Most of the time 6. All of the time
# ****************************************************************************************
table(base_sf12$SP1H7,useNA = "ifany")

base_sf12<-base_sf12 %>% 
  mutate(mh_down=case_when(SP1H7 %in% c(0,8)~NA,
                           SP1H7==5~4,
                           SP1H7==6~5,
                           T~SP1H7))

table(base_sf12$mh_down,useNA = "ifany")


# 1.7. Vitality/energy (vit_energy) ----
# ****************************************************************************************
# SF-6D question: You have a lot of energy
# 1=all of the time, 2=most of the time, 3=some of the time, 4=a little of the time,
# 5=none of the time
# Corresponding SNAC-K variables (from SelfPSY):
# SP1H6: How much of the time in the last four weeks have you felt full of energy?	
# 1. None of the time 2. A little of the time 3. Some of the time 4. A large part of the time 
# 5. Most of the time 6. All of the time
# ****************************************************************************************
table(base_sf12$SP1H6,useNA = "ifany")

base_sf12<-base_sf12 %>% 
  mutate(vit_energy=case_when(SP1H6 %in% c(0,8)~NA,
                              SP1H6==6~1,
                              SP1H6 %in% c(4,5)~2,
                              SP1H6==3~3,
                              SP1H6==2~4,
                              SP1H6==1~5))

table(base_sf12$vit_energy,useNA = "ifany")

# 1.8. Combine all converted variables ----
sf_snack<-base_sf12 %>% 
  select(lopnr=löpnr,pf_mod:vit_energy) %>% 
  full_join(pain_w1 %>% 
              select(lopnr=löpnr,pain_int))

# sanity check
table(sf_snack$pf_mod,useNA = "ifany")
table(sf_snack$rp_limit,useNA = "ifany")
table(sf_snack$re_less,useNA = "ifany")
table(sf_snack$sf_interf,useNA = "ifany")
table(sf_snack$pain_int,useNA = "ifany")
table(sf_snack$mh_down,useNA = "ifany")
table(sf_snack$vit_energy,useNA = "ifany")



# 2. Convert to utility index using modified Linus' script ----
# based on (Brazier & Roberts 2004, model 4)

sf12_to_sf6d <- function(
    data,
    pf_mod     = "pf_mod",
    rp_limit   = "rp_limit",
    re_less    = "re_less",
    sf_interf  = "sf_interf",
    pain_int   = "pain_int",
    mh_down    = "mh_down",
    vit_energy = "vit_energy",
    strict = TRUE
) {
  d <- data
  
  # Helper: check columns
  cols <- c(pf_mod, rp_limit, re_less, sf_interf, pain_int, mh_down, vit_energy)
  missing_cols <- setdiff(cols, names(d))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # ---- 1) Map SF-12 responses -> SF-6D(SF-12) dimension levels (Table 2) ----
  # Physical functioning (3 levels): Table 2 says:
  #   1 = not limited, 2 = limited a little, 3 = limited a lot
  PF_lvl <- d[[pf_mod]]

  
  # Role limitations (4 levels) from two yes/no items (Table 2):
  #   1 = none (no phys, no emot)
  #   2 = physical only
  #   3 = emotional only
  #   4 = both physical and emotional
  RP <- d[[rp_limit]]
  RE <- d[[re_less]]
  # allow common encodings: 1/0 or "Yes"/"No"
  to01 <- function(x) {
    if (is.logical(x)) return(ifelse(is.na(x), NA_integer_, ifelse(x, 1L, 0L)))
    if (is.numeric(x)) return(ifelse(is.na(x), NA_integer_,
                                     ifelse(x %in% c(1, "1"), 1L,
                                            ifelse(x %in% c(0, "0"), 0L, NA_integer_))))
    if (is.character(x)) {
      x2 <- trimws(tolower(x))
      return(ifelse(is.na(x), NA_integer_,
                    ifelse(x2 %in% c("yes","y","true","t","1"), 1L,
                           ifelse(x2 %in% c("no","n","false","f","0"), 0L, NA_integer_))))
    }
    return(rep(NA_integer_, length(x)))
  }
  RP01 <- to01(RP)
  RE01 <- to01(RE)
  
  RL_lvl <- ifelse(is.na(RP01) | is.na(RE01), NA_integer_,
                   ifelse(RP01 == 0 & RE01 == 0, 1L,
                          ifelse(RP01 == 1 & RE01 == 0, 2L,
                                 ifelse(RP01 == 0 & RE01 == 1, 3L,
                                        ifelse(RP01 == 1 & RE01 == 1, 4L, NA_integer_)))))
  
  # Social functioning
  # Table 2: 1=none, 2=little, 3=some, 4=most, 5=all
  SF_lvl <- d[[sf_interf]]

  # Pain interference (Table 2 has 5 levels):
  # 1 not at all, 2 a little bit, 3 moderately, 4 quite a bit, 5 extremely
  PAIN_lvl <- d[[pain_int]]

  # Mental health
  # Table 2 has 5 levels (none, little, some, most, all).
  MH_lvl <- d[[mh_down]]

  # Vitality/energy 
  # Table 2 has 5 levels (all, most, some, little, none).

  VIT_lvl <- d[[vit_energy]]

  # Optional strict range checking
  if (strict) {
    bad <- which(
      (!is.na(PF_lvl)   & !(PF_lvl   %in% 1:3)) |
        (!is.na(RL_lvl)   & !(RL_lvl   %in% 1:4)) |
        (!is.na(SF_lvl)   & !(SF_lvl   %in% 1:5)) |
        (!is.na(PAIN_lvl) & !(PAIN_lvl %in% 1:5)) |
        (!is.na(MH_lvl)   & !(MH_lvl   %in% 1:5)) |
        (!is.na(VIT_lvl)  & !(VIT_lvl  %in% 1:5))
    )
    if (length(bad) > 0) {
      stop("Found out-of-range mapped levels in rows: ", paste(head(bad, 20), collapse = ", "),
           if (length(bad) > 20) " ..." else "")
    }
  }
  
  # ---- 2) Apply Brazier & Roberts (2004) SF-6D(SF-12) SG algorithm: Table 4, model (4) ----
  # Utility = 1 - (sum of decrements for non-baseline levels) - MOST*decrement
  # (Coefficients below are the model (4) "parsimonious consistent" estimates.)   [oai_citation:3‡ResearchGate](https://www.researchgate.net/profile/Ariel-Linden/post/How_does_one_calculate_utility_values_using_QOL_instruments/attachment/59d63d62c49f478072ea872e/AS%3A273759624204295%401442280788202/download/Brazier_2004.pdf)
  
  # Decrements (note: all are utility *losses*)
  dec_PF  <- c(`1`=0.000, `2`=0.0, `3`=0.045)
  # RL is aggregated as RL234 (one decrement applied whenever RL level is 2,3,or 4)
  dec_RL234 <- 0.063
  # Social functioning decrements (levels 2..5 vs 1)
  dec_SF  <- c(`1`=0.000, `2`=0.063, `3`=0.066, `4`=0.081, `5`=0.093)
  # Pain decrements (levels 2..5 vs 1)
  dec_PAIN <- c(`1`=0.000, `2`=0.00, `3`=0.042, `4`=0.077, `5`=0.137)
  # Mental health: MH23 aggregated (applies to level 2 or 3), plus separate for 4 and 5
  dec_MH <- function(mh_lvl) {
    ifelse(is.na(mh_lvl), NA_real_,
           ifelse(mh_lvl == 1, 0.000,
                  ifelse(mh_lvl %in% c(2,3), 0.059,
                         ifelse(mh_lvl == 4, 0.113,
                                ifelse(mh_lvl == 5, 0.134, NA_real_)))))
  }
  # Vitality: VIT234 aggregated, plus separate VIT5
  dec_VIT <- function(vit_lvl) {
    ifelse(is.na(vit_lvl), NA_real_,
           ifelse(vit_lvl == 1, 0.000,
                  ifelse(vit_lvl %in% c(2,3,4), 0.078,
                         ifelse(vit_lvl == 5, 0.106, NA_real_))))
  }
  
  # MOST indicator definition as described in the paper:
  # "most severe" = PF level 3; RL levels 3 or 4; SF levels 4 or 5; PAIN levels 4 or 5;
  # MH levels 4 or 5; VIT level 5  [oai_citation:4‡ResearchGate](https://www.researchgate.net/profile/Ariel-Linden/post/How_does_one_calculate_utility_values_using_QOL_instruments/attachment/59d63d62c49f478072ea872e/AS%3A273759624204295%401442280788202/download/Brazier_2004.pdf)
  MOST <- ifelse(
    is.na(PF_lvl) | is.na(RL_lvl) | is.na(SF_lvl) | is.na(PAIN_lvl) | is.na(MH_lvl) | is.na(VIT_lvl),
    NA_integer_,
    as.integer(
      (PF_lvl == 3) |
        (RL_lvl %in% c(3,4)) |
        (SF_lvl %in% c(4,5)) |
        (PAIN_lvl %in% c(4,5)) |
        (MH_lvl %in% c(4,5)) |
        (VIT_lvl == 5)
    )
  )
  
  # Compute decrements
  d_pf   <- ifelse(is.na(PF_lvl), NA_real_, dec_PF[as.character(PF_lvl)])
  d_rl   <- ifelse(is.na(RL_lvl), NA_real_, ifelse(RL_lvl == 1, 0.000, dec_RL234))
  d_sf   <- ifelse(is.na(SF_lvl), NA_real_, dec_SF[as.character(SF_lvl)])
  d_pain <- ifelse(is.na(PAIN_lvl), NA_real_, dec_PAIN[as.character(PAIN_lvl)])
  d_mh   <- dec_MH(MH_lvl)
  d_vit  <- dec_VIT(VIT_lvl)
  
  # MOST decrement (model 4)
  dec_MOST <- 0.077
  
  utility <- 1.0 - (d_pf + d_rl + d_sf + d_pain + d_mh + d_vit) - ifelse(is.na(MOST), NA_real_, MOST * dec_MOST)
  
  # SF-6D state code (PF-RL-SF-PAIN-MH-VIT)
  sf6d_state <- paste0(PF_lvl, RL_lvl, SF_lvl, PAIN_lvl, MH_lvl, VIT_lvl)
  
  # Return
  out <- data.frame(
    PF_lvl = PF_lvl,
    RL_lvl = RL_lvl,
    SF_lvl = SF_lvl,
    PAIN_lvl = PAIN_lvl,
    MH_lvl = MH_lvl,
    VIT_lvl = VIT_lvl,
    MOST = MOST,
    sf6d_state = sf6d_state,
    utility_sg = utility,
    stringsAsFactors = FALSE
  )
  out
}

sf_snack<-cbind(sf_snack,
                sf12_to_sf6d(sf_snack))

summary(sf_snack)



# 3. Merge other SNAC-K variables with SF-12 variables ----
med_hist_w1<-read_sav("diagnosis and anamnesis wave 1-3/wave 1/SNAC-K wave 1 anamnesis clean.sav") %>% 
  as.data.frame()

npr_before2017<-read_sav("npr and val/Patientregistry from SoS 1968-2016_C1_C2.sav") %>% 
  as.data.frame()

val_2001_2012<-read_sav("npr and val/ORV_val_SNAC-K_C1_C2.sav") %>% 
  as.data.frame() %>% 
  mutate(year=as.numeric(substr(bdat,1,4)),
         vardniva=ifelse(vardniva=="",NA,vardniva)) %>% 
  rename(lopnr=Löpnr)

phy_diag_w1<-read_sav("diagnosis and anamnesis wave 1-3/wave 1/SNAC-K wave 1_diseases_ 14 diagosis.sav") %>% 
  as.data.frame()

nurse_int_w1<-read_sav("Nurse wave 1-3/wave1/SNAC-K nurse wave1.sav") %>% 
  as.data.frame()

nutri<-read_dta("Malnutrition and HCRU SNACK.dta") %>% 
  as.data.frame()

nutri_attr_cov<-read_dta("Malnutrition and HCRU SNACK extra variables.dta") %>% 
  as.data.frame()


med_hist_w1<-med_hist_w1 %>% 
  select(Löpnr,PH1K42,PH1K44,PH1K49,PH1K52,PH1K56,PH1K64,PH1K73,PH1K85,PH1K87,PH1K91) %>% 
  set_names(c("lopnr","mi","hf","stroke","pd","copd",
              "ra","op","cancer","depression","dementia")) %>% 
  mutate(across(mi:dementia, ~ if_else(.x %in% c(8, 9), NA, .x)))

npr_dis_w1<-npr_before2017 %>% 
  select(Löpnr,INDATUMA,DIAGNOS) %>% 
  rename(lopnr=Löpnr) %>%
  mutate(INDATUMA=ymd(INDATUMA)) %>% 
  right_join(nutri_attr_cov %>% select(lopnr,date1)) %>% 
  filter(INDATUMA<=date1&INDATUMA>=date1-365.25*5) %>% 
  mutate(icd_version=ifelse(year(INDATUMA)<=1996,9,10))

val_dis_w1<-val_2001_2012 %>% 
  select(lopnr,bdat,diag1:diag8) %>% 
  mutate(pv_date=ymd(bdat)) %>% 
  right_join(nutri_attr_cov %>% select(lopnr,date1)) %>% 
  filter(pv_date<=date1&pv_date>=date1-365.25*5)


# import ICD codes to corresponding diseases
icd_codes<-read_excel("Malnutrition statistical analysis/Malnutrition-study/ICD codes_malnutrition study_snack.xlsx") %>% 
  as.data.frame()

icd_codes<-icd_codes %>%
  as.data.frame() %>%
  mutate(icd10_vector = ifelse(
    icd10 == "C00–C97", 
    list(paste0("C", sprintf("%02d", 0:97))),  
    lapply(icd10, function(x) str_trim(str_split(x, ",")[[1]]))  
  ),
  icd9_vector=lapply(icd9, function(x) str_trim(str_split(x, ",")[[1]]))  
  )


# diseases in NPR
for (i in seq_along(icd_codes$disease)) {
  disease_name<-icd_codes$disease[i]   
  npr_dis_w1[[disease_name]]<-0
  
  for (j in seq_along(icd_codes$icd9_vector[[i]])){
    npr_dis_w1[[disease_name]][npr_dis_w1$icd_version==9&
                                 grepl(icd_codes$icd9_vector[[i]][j], npr_dis_w1$DIAGNOS)]<-1
  }
  
  for (k in seq_along(icd_codes$icd10_vector[[i]])){
    npr_dis_w1[[disease_name]][npr_dis_w1$icd_version==10&
                                 grepl(icd_codes$icd10_vector[[i]][k], npr_dis_w1$DIAGNOS)]<-1
  }
}


unique((npr_dis_w1 %>% filter(mi==1))$DIAGNOS)
unique((npr_dis_w1 %>% filter(pd==1))$DIAGNOS)


npr_dis_w1<-npr_dis_w1 %>% 
  group_by(lopnr) %>% 
  summarise(across(mi:dementia, ~ as.integer(any(. == 1))), .groups = "drop")


# diseases in VAL
for (i in seq_along(icd_codes$disease)) {
  disease_name<-icd_codes$disease[i]   
  
  val_dis_w1[[disease_name]]<-0
  for (j in seq_along(icd_codes$icd10_vector[[i]])){
    match_rows<-rowSums(sapply(val_dis_w1[, paste0("diag", 1:8)], 
                               function(col) grepl(icd_codes$icd10_vector[[i]][j], col)))>0
    val_dis_w1[[disease_name]][match_rows]<-1
  }
}

unique((val_dis_w1 %>% filter(mi==1))$diag1,(val_dis_w1 %>% filter(mi==1))$diag2)
unique((val_dis_w1 %>% filter(pd==1))$diag1,(val_dis_w1 %>% filter(pd==1))$diag2)

val_dis_w1<-val_dis_w1 %>% 
  group_by(lopnr) %>% 
  summarise(across(mi:dementia, ~ as.integer(any(. == 1))), .groups = "drop")


# diseases in SNAC-K physicians' diagnoses
for (i in seq_along(icd_codes$disease)) {
  disease_name<-icd_codes$disease[i]   
  icd_vector<-c(icd_codes$icd9_vector[[i]],icd_codes$icd10_vector[[i]])
  
  phy_diag_w1[[disease_name]]<-0
  for (j in seq_along(icd_vector)){
    match_rows<-rowSums(sapply(phy_diag_w1[, paste0("D1O14_", 1:14)], 
                               function(col) grepl(icd_vector[j], col)))>0
    phy_diag_w1[[disease_name]][match_rows]<-1
  }
}

summary(phy_diag_w1)

phy_diag_w1<-phy_diag_w1 %>% 
  rename(lopnr=Löpnr) %>% 
  group_by(lopnr) %>% 
  summarise(across(mi:dementia, ~ as.integer(any(. == 1))), .groups = "drop")

dis_w1<-rbind(med_hist_w1,
              npr_dis_w1,
              val_dis_w1,
              phy_diag_w1) %>% 
  group_by(lopnr) %>% 
  summarise(across(mi:dementia, ~ as.integer(any(. == 1))), .groups = "drop") %>% 
  mutate(across(c(mi:dementia), ~ if_else(is.na(.x), 0, .x)))


# Smell, taste chewing function, and ADL
smell_adl_w1<-nurse_int_w1 %>% 
  select(Löpnr,N1E110,N1E111,N1E76) %>% 
  set_names(c("lopnr","smell","taste","chew")) %>% 
  mutate(across(smell:chew, ~ if_else(.x %in% c(8, 9), NA, .x))) %>% 
  mutate(smell_imp=ifelse(smell %in% c(2,3,4),1,0),
         taste_imp=ifelse(taste %in% c(2,3,4),1,0),
         chew_imp=ifelse(chew %in% c(2,3),1,0))



mna_sf<-sf_snack %>% 
  select(lopnr,PF_lvl,RL_lvl,SF_lvl,PAIN_lvl,MH_lvl,VIT_lvl) %>% 
  full_join(nutri %>% 
              select(lopnr,mna_screening1_cat,age1,sex,education,lives1,tobacco1)) %>% 
  full_join(dis_w1) %>% 
  full_join(smell_adl_w1 %>% 
              select(lopnr,smell_imp,taste_imp,chew_imp)) %>%
  mutate(across(c(mna_screening1_cat,sex,education,lives1,tobacco1), as.factor))
  


# descriptive table
table1::table1(~ .|age_group,data=mna_sf %>% 
                 mutate(age_group=factor(age1>=78,levels = c(0,1),
                                         labels = c("<78",">=78"))) %>% 
                 select(age1:chew_imp,mna_screening1_cat,PF_lvl:VIT_lvl,age_group) %>% 
                 mutate(across(mi:VIT_lvl, as.factor)))

table1::table1(~ .|mna_screening1_cat,data=mna_sf %>% 
                 filter(!is.na(mna_screening1_cat)) %>% 
                 select(mna_screening1_cat,PF_lvl:VIT_lvl) %>% 
                 mutate(across(c(mna_screening1_cat,PF_lvl:VIT_lvl), as.factor)))

table1::table1(~ .,data=mna_sf %>% 
                 select(PF_lvl:VIT_lvl) %>% 
                 mutate(across(PF_lvl:VIT_lvl, as.factor)))

write_dta(mna_sf,"Organized data/sf_snack_xx.dta")
