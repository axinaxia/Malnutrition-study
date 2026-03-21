library(haven)
library(readr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(flextable)
library(lubridate)
library(writexl)
library(XML)
library(Hmisc) 
library(data.table)

setwd("~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/")


# 1. Load data ----
# ******************************************************************************************
# Notes from Carl:
# ”CHD…” är TakeCare-uttaget
# ”Indextable” är det dataset vi läst samman från olika källor, 
# och det ligger upplagt i två olika format (dta + csv).
# övriga är slutenvårdsfil resp öppenvårdsfil från VAL
# ******************************************************************************************
takecare<-read_delim("SLSO data_20251124/CHD_115_SLSO_dnr_2024-0831_till_Carl_Willers.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  as.data.frame()

colnames(takecare)


indextable<-read_dta("SLSO data_20251124/indextable_2019_clean.dta") %>% 
  as.data.frame() %>% 
  rename(lopnr=lpnr)

colnames(indextable)

# change date of death
# SAS’s default date origin is January 1, 1960
indextable<-indextable %>% 
  mutate(death_date=as.Date(DodDatum,origin="1960-01-01"))


# öppenvård
val_ovr<-read_csv("SLSO data_20251124/val_ovr.csv") %>% 
  as.data.frame()

colnames(val_ovr)

# slutenvård
val_slv<-read_csv("SLSO data_20251124/val_slv.csv") %>% 
  as.data.frame()

colnames(val_slv)

# sol
sol<-read_sas("SLSO data_20251124/ut_r_sol_23656_2022.sas7bdat") %>% 
  as.data.frame()


# export variable names to excel for labeling and selection
# write_xlsx(
#   rbind(
#     as.data.frame(colnames(takecare)) %>% set_names("variable") %>% 
#       mutate(data="takecare"),
#     as.data.frame(colnames(indextable)) %>% set_names("variable") %>% 
#       mutate(data="indextable"),
#     as.data.frame(colnames(val_ovr)) %>% set_names("variable") %>% 
#       mutate(data="val_ovr"),
#     as.data.frame(colnames(val_slv)) %>% set_names("variable") %>% 
#       mutate(data="val_slv")
#   ),
#   "SLSO data_20251124/SLSO data variables.xlsx"
# )



# 2. Define study population and start of observational period ----
# start of the observational period defined by "Inlagd_den" in Takecare
study_pop<-takecare %>% 
  select(lopnr,Inlagd_den,Kon,AlderIn) %>% 
  mutate(index_date=as.Date(Inlagd_den)) %>% 
  group_by(lopnr) %>% 
  slice_min(index_date) %>%
  ungroup()

n_distinct(study_pop$lopnr) 
length(study_pop$lopnr) 


# 3. Malnutrition defined by MNA ----
# check MNA data
summary(takecare %>% select(Fodointag_1,contains("MNA")))
table(takecare$MNA_varde)
table(takecare$MNAIrisk)
table(takecare$MNAIrisk,takecare$MNA_varde)

# recalculate MNA score and define MNA status
takecare<-takecare %>% 
  rename(Fodointag_MNA=Fodointag_1) %>% 
  mutate(MNA_new=Fodointag_MNA+Viktforlust_MNA+Rorlighet_MNA+Psykisktstress_MNA+
           Neuropsykologiska_MNA+BMI_MNA,
         mna_cat=case_when(
           MNA_new>=12 ~ 1, # normal nutritional status
           MNA_new>=8 & MNA_new<=11 ~ 2, # at risk of malnutrition
           MNA_new<=7 ~ 3, # malnourished
           TRUE ~ NA
         ),
         mna_cat=factor(mna_cat,levels=c(1,2,3),
                        labels=c("Normal nutritional status",
                                 "At risk of malnutrition",
                                 "Malnourished")))

table(takecare$mna_cat,takecare$MNA_new)
table((study_pop %>% 
  left_join(takecare %>% 
              select(lopnr,Inlagd_den,mna_cat)))$mna_cat,useNA = "ifany")

# 4. Cleaning covariates data ----
# 4.1. SES data ----
study_pop<-study_pop %>% 
  left_join(indextable %>% 
              distinct(lopnr,civil_status,edu_level,ensamboende))

table(study_pop$Kon,useNA = "ifany")
table(study_pop$civil_status,useNA = "ifany")
table(study_pop$edu_level,useNA = "ifany")
table(study_pop$ensamboende,useNA = "ifany")

study_pop<-study_pop %>% 
  mutate(age=AlderIn,
         sex=ifelse(Kon=="M",0,1),
         across(civil_status:ensamboende, ~ as.numeric(ifelse(.x=="",NA,.x))),
         edu_level=factor(edu_level,levels=1:5,
                          labels=c("Primary school", "Lower secondary school", 
                                   "Upper secondary school", 
                                   "Post-secondary school", 
                                   "Higher than post-secondary school")),
         civil_status=factor(civil_status,levels=1:4,
                             labels=c("Widowed", "Never married", 
                             "Divorced", "Married"))) %>% 
  select(-c(AlderIn,Kon))



# 4.2. ADL data ----
# check Katz and Barthel data
summary(takecare %>% select(Katz_summa:TermID_6498))
summary(rowSums(takecare %>% select(TermID_6493:TermID_6498)))
summary(takecare %>% select(Barthel_varde_Forsta:Rorlighet_trappa_Forsta))
summary(rowSums(takecare %>% select(Fodointag_Forsta:Rorlighet_trappa_Forsta)))


takecare<-takecare %>%
  mutate(adl_bi_cat=case_when(
      Barthel_varde_Forsta==100 ~ 1,  # Independent
      Barthel_varde_Forsta>=91 & Barthel_varde_Forsta<=99 ~ 2,  # Slightly dependent
      Barthel_varde_Forsta>=61 & Barthel_varde_Forsta<=90 ~ 3,  # Moderately dependent
      Barthel_varde_Forsta>=21 & Barthel_varde_Forsta<=60 ~ 4,  # Severely dependent
      Barthel_varde_Forsta<=20 ~ 5,  # Dependent
      TRUE ~ NA_real_)
  )



# check mobility data
summary(takecare %>% select(RMI_varde_Forsta))


# 4.3. Comorbidity at index date ---- 
# Load ICD codes for diseases of interest
disease_icd<-read_excel("Malnutrition statistical analysis/Malnutrition-study/ICD codes for diseases in SLSO.xlsx")
disease_icd<-disease_icd %>%
  mutate(icd=ifelse(disease == "cancer",
                    paste(sprintf("C%02d", 0:97), collapse = ","),icd))

  
# combine val and takecare diagnosis (3-digit icd codes)
comorb_index<-takecare %>% 
  mutate(Inlagd_den=as.Date(Inlagd_den),
         diagnos=substr(Huvuddiagnos,1,3)) %>% 
  select(lopnr,diagnos,Inlagd_den) %>% 
  right_join(study_pop %>% select(lopnr,index_date)) %>% 
  filter(!is.na(diagnos),
         Inlagd_den<=index_date) %>%
  distinct(lopnr,diagnos) %>% 
  rbind(val_ovr %>% 
          select(lopnr,BDAT,contains("diag")) %>% 
          pivot_longer(cols=contains("diag"), names_to="diag_num", values_to="diagnos") %>% 
          mutate(ov_date=as.Date(as.character(BDAT),format="%Y%m%d")) %>% 
          right_join(study_pop %>% select(lopnr,index_date)) %>% 
          filter(!is.na(diagnos),
                 ov_date<=index_date) %>%
          distinct(lopnr,diagnos)) %>% 
  rbind(val_slv %>% 
          select(lopnr,INDAT,contains("diag")) %>% 
          pivot_longer(cols=contains("diag"), names_to="diag_num", values_to="diagnos") %>% 
          mutate(sv_date=as.Date(as.character(INDAT),format="%Y%m%d")) %>% 
          right_join(study_pop %>% select(lopnr,index_date)) %>% 
          filter(!is.na(diagnos),
                 sv_date<=index_date) %>%
          distinct(lopnr,diagnos)) %>%
  distinct(lopnr,diagnos)



for (i in seq_along(disease_icd$disease)){
  disease_name<-disease_icd$disease[i]       
  icd_codes<-trimws(unlist(strsplit(disease_icd$icd[i], ",")))
  
  comorb_index[[disease_name]]<-0
  
  rows_inc<-comorb_index$diagnos %in% icd_codes
  
  comorb_index[rows_inc,disease_name]<-1
  
}

summary(comorb_index)


# sanity check
table((comorb_index %>% 
         filter(cancer==1))$diagnos)

table((comorb_index %>% 
         filter(stroke==1))$diagnos)

comorb_index<-comorb_index %>% 
  group_by(lopnr) %>%
  summarise(across(c(ihd:dem), ~ max(.x))) %>%
  ungroup

summary(comorb_index)


# 4.4. Main diagnosis at admission ---- 
# map the most frequent main diagnoses at admission
# import names for ICD codes
icd_xml<-xmlParse('Malnutrition statistical analysis/Malnutrition-study/icd10cm-tabular-2024.xml')

icd_xml_df<-xmlToDataFrame(nodes = getNodeSet(icd_xml, c("//name", "//desc"))) %>%
  mutate(desc=lead(text),
         text=gsub("\\.", "", text)) %>% 
  filter(nchar(text) %in% 3:5)

takecare_huvuddiag<-takecare %>% 
  select(lopnr,Huvuddiagnos,Inlagd_den) %>% 
  mutate(Inlagd_den=as.Date(Inlagd_den),
         diagnos=substr(Huvuddiagnos,1,3)) %>% 
  right_join(study_pop %>% 
               select(lopnr,index_date,age) %>% 
               mutate(age_group=as.factor(case_when(age<70~"<70",
                                                    age<80~"70-79",
                                                    age>=80~"80+")))) %>%
  filter(Inlagd_den<=index_date) %>% 
  select(lopnr,diagnos,age_group) %>% 
  left_join(icd_xml_df %>% 
              filter(nchar(text)==3) %>% 
              rename(diagnos=text)) %>% 
  filter(!is.na(diagnos))

unique((takecare_huvuddiag %>% 
  filter(is.na(desc)))$diagnos)

# F00 = Dementia in Alzheimer's disease
# G41 = Status Epilepticus
# I64 = Stroke, not specified as haemorrhage or infarction
# J46 = Status asthmaticus
# M82 = Osteoporosis in diseases classified elsewhere
# T00 = Superficial injuries involving multiple body regions
# T11 = Other injuries of upper limb, level unspecified
# T93 = Sequelae of injuries of the lower limb


takecare_huvuddiag<-takecare_huvuddiag %>% 
  mutate(desc=case_when(
    diagnos=="F00" ~ "Dementia in Alzheimer's disease",
    diagnos=="G41" ~ "Status Epilepticus",
    diagnos=="I64" ~ "Stroke, not specified as haemorrhage or infarction",
    diagnos=="J46" ~ "Status asthmaticus",
    diagnos=="M82" ~ "Osteoporosis in diseases classified elsewhere",
    diagnos=="T00" ~ "Superficial injuries involving multiple body regions",
    diagnos=="T11" ~ "Other injuries of upper limb, level unspecified",
    diagnos=="T93" ~ "Sequelae of injuries of the lower limb",
    TRUE ~ desc))

unique((takecare_huvuddiag %>% 
          filter(is.na(desc)))$diagnos)

table(takecare_huvuddiag$desc)


huvuddiag_freq<-table(takecare_huvuddiag$desc) %>% 
  as.data.frame() %>%
  slice_max(Freq,n=10) %>% 
  left_join(table((takecare_huvuddiag %>% 
                     filter(age_group=="<70"))$desc) %>% 
              as.data.frame() %>% 
              rename(Freq_80=Freq)) %>% 
  left_join(table((takecare_huvuddiag %>% 
                     filter(age_group=="70-79"))$desc) %>% 
              as.data.frame() %>% 
              rename(Freq_84=Freq)) %>% 
  left_join(table((takecare_huvuddiag %>% 
                     filter(age_group=="80+"))$desc) %>% 
              as.data.frame() %>% 
              rename(Freq_85=Freq)) %>% 
  mutate(perc=Freq/nrow(study_pop)*100,
         perc_80=Freq_80/nrow(study_pop %>% filter(age<80))*100,
         perc_84=Freq_84/nrow(study_pop %>% filter(age>=80&age<85))*100,
         perc_85=Freq_85/nrow(study_pop %>% filter(age>=85))*100,
         Freq=paste0(Freq," (",round(perc,2),")"),
         Freq_80=paste0(Freq_80," (",round(perc_80,2),")"),
         Freq_84=paste0(Freq_84," (",round(perc_84,2),")"),
         Freq_85=paste0(Freq_85," (",round(perc_85,2),")")) %>% 
  select(Var1,Freq_80,Freq_84,Freq_85,Freq)





# 5. Clean six-month HCRU data ----
# Use DRG-cost, MVO-cost, cost-per-visit from 2024
cpi<-read_excel("Malnutrition statistical analysis/Malnutrition-study/CPI.xlsx") %>% as.data.frame() %>% 
  mutate(year=as.numeric(year))


# 5.1. Social care ----
sol_hcru<-sol %>% 
  select(lopnr,PERIOD,BOFORM,HTJ,HTJTIM,DAGV,KORTMAN) %>% 
  mutate(sol_date=as.Date(as.character(paste0(PERIOD,"01")),format="%Y%m%d")) %>% 
  mutate(across(c(lopnr:KORTMAN), ~as.numeric(.x))) %>%
  right_join(study_pop %>% select(lopnr,index_date)) %>%
  filter(sol_date>=index_date,
         (year(sol_date)-year(index_date))*12+month(sol_date)-month(index_date)<=6) %>% 
  mutate(# number of days in institutionalization 
    inst_days=as.numeric(BOFORM==2)*31,
    
    # home help hours
    hhtim=case_when(HTJ==1 & HTJTIM<777 ~ HTJTIM,
                    as.numeric(BOFORM==2)==1 & HTJ==1 & HTJTIM==777 ~ 0,
                    as.numeric(BOFORM==2)==0 & HTJ==1 & HTJTIM==777 ~ 24*31,
                    T ~ 0),
    
    # days in short-term residence 
    shortterm_days=ifelse(KORTMAN %in% c('88', '99',' .','-1', '-2', '-3', '-7', '-9', '0', '00', ''), 0,as.numeric(KORTMAN)),
    shortterm_days=ifelse(shortterm_days>31,31,shortterm_days),
    
    #number of days of using social day activity 
    dagv=ifelse(DAGV=='1',1,0)*31) %>% 
  mutate(across(hhtim:dagv, ~ ifelse(BOFORM==2, 0, .x))) %>% 
  distinct_all()

# check duplicated records
# sol_hcru %>% 
#   add_count(lopnr,PERIOD) %>%    
#   filter(n>1) %>% 
#   View()


# split sol data into duplicated records and distinct records per period
sol_hcru_dist<-sol_hcru %>% 
  add_count(lopnr,PERIOD) %>%    
  filter(n==1)

sol_hcru_dup<-sol_hcru %>% 
  add_count(lopnr,PERIOD) %>%    
  filter(n>1)

# if one record indicate institutionalized, and another not in the same month, keep institutionalization
sol_hcru_dup<-sol_hcru_dup %>% 
  group_by(lopnr,PERIOD) %>% 
  mutate(inst_flag=max(inst_days)) %>% 
  ungroup %>% 
  filter(!(inst_flag==31 & inst_days==0)) %>% 
  select(-inst_flag)


# if short term residence >=31 days, set other social care as 0
# if short term residence <31 days, set the upper limit of other social care as the rest of days/hours in that month
sol_hcru_dup<-sol_hcru_dup %>% 
  mutate(across(c(hhtim,dagv), ~ ifelse(shortterm_days>=31, 0, .x)),
         dagv=ifelse(shortterm_days<31&
                       dagv>31-shortterm_days, 31-shortterm_days, dagv),
         hhtim=ifelse(shortterm_days<31&
                        hhtim>24*(31-shortterm_days), 24*(31-shortterm_days), hhtim))


# sum up the social care use for duplicated records per period
sol_hcru_dup<-sol_hcru_dup %>% 
  group_by(lopnr,PERIOD) %>% 
  summarise(inst_days=sum(inst_days),
            hhtim=sum(hhtim),
            shortterm_days=sum(shortterm_days),
            dagv=sum(dagv)) %>% 
  ungroup %>% 
  mutate(across(hhtim:dagv, ~ ifelse(inst_days==31, 0, .x)),
         hhtim=ifelse(hhtim>24*31,24*31,hhtim),
         dagv=ifelse(dagv>31,31,dagv))


sol_hcru_dedup<-bind_rows(sol_hcru_dist,
                               sol_hcru_dup) %>% 
  arrange(lopnr,PERIOD)


sol_costs<-sol_hcru_dedup %>%
  mutate(cost_homecare=hhtim*646, # from Emil's request to Ensolution (check email from 2026-03-04)
         cost_daytimeactivity=dagv*148400/365.25, 
         cost_shortterm=shortterm_days*3387, # from Emil's request to Ensolution (check email from 2026-03-04)
         cost_inst=inst_days*2566,
         sol_cost=cost_homecare+cost_daytimeactivity+cost_shortterm+cost_inst)



# 5.2. Primary care ----
# 5.2.1. Separate records for primary care from specialist care in val_ovr ----
# SPEC: 8xx = primärvård
# VARDNIVA: 01 = Primärvård 02 = Specialiserad vård
table(val_ovr$VARDNIVA,useNA = "ifany")
# View(val_ovr %>% filter(is.na(VARDNIVA)))

pv_hcru<-val_ovr %>% 
  filter(VARDNIVA=="01"|(is.na(VARDNIVA)&grepl("^8",SPEC))) %>% 
  select(lopnr,BDAT,KLIN,SPEC,DRG,BTYP,contains("VDG")) %>% 
  mutate(pv_date=as.Date(as.character(BDAT),format="%Y%m%d"),
         year=as.numeric(year(pv_date))) %>%
  right_join(study_pop %>% select(lopnr,index_date)) %>%
  filter(pv_date>=index_date,
         (year(pv_date)-year(index_date))*12+month(pv_date)-month(index_date)<=6) %>% 
  rename(btyp=BTYP,
         vdg=VDG1)


# check if there are several records with duplicated KLIN and SPEC on the same day for the same individual
# View(pv_hcru %>% add_count(lopnr,pv_date,KLIN,SPEC) %>%  filter(n>1))
# View(val_ovr %>% 
#        filter(VARDNIVA=="01"|(is.na(VARDNIVA)&grepl("^8",SPEC))) %>% add_count(lopnr,BDAT,KLIN,SPEC) %>%  filter(n>1))


pv_hcru<-data.table(pv_hcru %>% 
                      distinct(lopnr,pv_date,KLIN,SPEC,.keep_all = T))

# 5.2.2. Use cost per contact by occupation and type of visit from KPP ----
btyp_vdg<-read_excel("Malnutrition statistical analysis/Malnutrition-study/btyp and vdg.xlsx",
                     sheet = "unit cost") %>% 
  data.table



btyp_vdg<-btyp_vdg[
  , .(btyp = unlist(strsplit(btyp, ","))),
  by = .(vdg, cost_2024)
][
  , c("vdg_from", "vdg_to") := {
    x <- tstrsplit(vdg, "-", fixed = TRUE)
    lo <- as.numeric(x[[1]])
    hi <- as.numeric(fifelse(is.na(x[[2]]), x[[1]], x[[2]]))
    .(lo, hi)
  }
][
  , vdg := NULL
]


pv_hcru[, vdg := as.numeric(vdg)]


pv_hcru[btyp_vdg,
        on = .(btyp, vdg >= vdg_from, vdg <= vdg_to),
        cost_2024 := i.cost_2024]

summary(pv_hcru)

pv_hcru<-pv_hcru %>% 
  left_join(btyp_vdg %>% 
              filter(is.na(vdg_from)) %>% 
              select(btyp,cost_2024),
            by="btyp") %>% 
  mutate(cost_2024=ifelse(!is.na(cost_2024.x),cost_2024.x,cost_2024.y)) %>% 
  select(-c(cost_2024.x,cost_2024.y)) %>% 
  filter(!is.na(cost_2024)) %>% 
  rename(pv_cost=cost_2024)




# 5.3. Specialist care ----
# 5.3.1. Clean DRG weights and costs ----
drgcost<-read_excel("Malnutrition statistical analysis/Malnutrition-study/DRG_2012-2021.xlsx", 
                    n_max=1, col_types=c('skip', 'skip', rep('text',10 ))) %>%
  pivot_longer(cols=as.character(2012:2021), names_to='year', values_to='drgcost') %>% 
  mutate(year=as.numeric(year),
         drgcost=as.numeric(drgcost)) 

drgcost<-drgcost %>% left_join(cpi, by='year') %>% 
  mutate(drgcost= drgcost*(((cpi %>% filter(year==2024))$CPI)/CPI)) %>% # convert to 2024 SEK
  select(year, drgcost)


drg<-read_excel('Malnutrition statistical analysis/Malnutrition-study/DRG_2012-2021.xlsx', 
                col_types=c('text', 'text', rep('numeric',10)))  %>% 
  filter(!is.na(DRG)) %>% 
  pivot_longer(cols=as.character(2012:2021),names_to='year', values_to='weight') %>% 
  mutate(year=as.numeric(year)) %>%
  select(DRG, year, weight)

# Create a table "drgnew" with all DRGs and all years
# Impute missing DRG weights by rolling forward and backward
alldrgyr<-crossing(DRG=unique(drg$DRG), year=2012:2021)

#set as data tables and set keys
drg<-data.table(drg)
alldrgyr<-data.table(alldrgyr)
setkey(drg, DRG, year)
setkey(alldrgyr, DRG, year)

#create new dataset rolling forward
temp1<-(drg %>% drop_na())[alldrgyr, roll=T] %>% drop_na()
temp2<-(drg %>% drop_na())[alldrgyr, roll=-Inf] %>% drop_na()



#use new dataset to roll backward
drgnew<-rbind(temp1,temp2) %>% 
  arrange(DRG,year) %>% 
  distinct_all()

rm(temp1,temp2)

# check A03A, A16N, Z63O, A21N

# 5.3.2. Specialist outpatient care ----
ov_hcru<-val_ovr %>% 
  filter(VARDNIVA=="02"|(is.na(VARDNIVA)&!grepl("^8",SPEC))) %>% 
  select(lopnr,BDAT,KLIN,SPEC,DRG) %>% 
  mutate(ov_date=as.Date(as.character(BDAT),format="%Y%m%d"),
         year=as.numeric(year(ov_date))) %>%
  right_join(study_pop %>% select(lopnr,index_date)) %>%
  filter(ov_date>=index_date,
         (year(ov_date)-year(index_date))*12+month(ov_date)-month(index_date)<=6)

# check if there are several records with duplicated KLIN and SPEC on the same day for the same individual
# View(ov_hcru %>% add_count(lopnr,ov_date,KLIN,SPEC) %>%  filter(n>1))
# View(val_ovr %>% add_count(lopnr,BDAT,KLIN,SPEC,BTYP) %>%  
#        filter(n>1) %>% 
#        arrange(lopnr,BDAT) %>% 
#        select(lopnr,BDAT,KLIN,SPEC,contains(c("diag")),everything()))

ov_hcru<-ov_hcru %>% 
  distinct(lopnr,ov_date,KLIN,SPEC,.keep_all = T)


# check DRG code
ov_hcru<-ov_hcru %>% 
  mutate(drg_1=substr(DRG,1,1),
         drg_1rm=substr(DRG,2,nchar(DRG)))

table(ov_hcru$drg_1,useNA="ifany")

# use DRG codes after removing the first letter
ov_hcru<-ov_hcru %>% 
  select(-c(DRG,drg_1)) %>% 
  rename(DRG=drg_1rm)


# use DRG to calculate costs and KLIN in case of missing
psyk_ov<-read_excel("Malnutrition statistical analysis/Malnutrition-study/MVO by care_XX_202510.xlsx",
                    sheet = "Psyk_ov") %>% 
  as.data.frame %>% 
  select(-contains("_ave")) %>% 
  pivot_longer(
    cols = starts_with(c("mvo_", "cost_")),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) %>% 
  filter(!is.na(cost)) %>% 
  mutate(mvo=substr(mvo,1,3))

soma_ov<-read_excel("Malnutrition statistical analysis/Malnutrition-study/MVO by care_XX_202510.xlsx",
                    sheet = "Soma_ov") %>% 
  as.data.frame %>% 
  as.data.frame %>% 
  select(-contains("_ave")) %>% 
  pivot_longer(
    cols = starts_with(c("mvo_", "cost_")),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) %>% 
  filter(!is.na(cost)) %>% 
  mutate(mvo=substr(mvo,1,3))

mvo_ov<-psyk_ov %>% 
  mutate(vtype="psyk") %>% 
  rbind(soma_ov %>% 
          mutate(vtype="soma")) %>% 
  filter(year %in% c(2018,2019,2020)) %>% 
  group_by(year,mvo) %>% 
  summarise(cost=mean(cost)) %>%
  ungroup



# inflate to 2024 price
mvo_ov<-mvo_ov %>% 
  arrange(mvo,year) %>% 
  mutate(year=as.numeric(year)) %>% 
  left_join(cpi) %>% 
  mutate(cost=cost*(CPI/((cpi %>% filter(year==2024))$CPI))) %>% 
  select(-CPI)


ov_cost<-ov_hcru %>% 
  left_join(drgnew, by=c("DRG","year")) %>% 
  left_join(drgcost, by=c("year")) %>% 
  mutate(cost_drg=weight*drgcost) %>% 
  left_join(mvo_ov, by=c("year","KLIN"="mvo")) %>% 
  mutate(ov_cost=ifelse(is.na(cost_drg),cost,cost_drg))

summary(ov_cost)
  
# impute missingness with average MVO costs for inpatient care
ov_cost<-ov_cost %>% 
  left_join(mvo_ov %>% 
              filter(mvo=="ave") %>% 
              rename(cost_ave=cost)) %>% 
  mutate(ov_cost=ifelse(is.na(ov_cost),cost_ave,ov_cost))
summary(ov_cost)



# 5.3.3. Specialist inpatient care ----
sv_hcru<-val_slv %>% 
  filter(VARDNIVA=="02"|(is.na(VARDNIVA)&!grepl("^8",SPEC))) %>% 
  select(lopnr,INDAT,UTDAT,KLIN,DRG,SPEC,VDAGH) %>% 
  mutate(sv_date=as.Date(as.character(INDAT),format="%Y%m%d"),
         year=as.numeric(year(sv_date)),
         los=as.numeric(as.Date(as.character(UTDAT),format="%Y%m%d"))-as.numeric(sv_date)+1) %>%
  right_join(study_pop %>% select(lopnr,index_date)) %>%
  filter(sv_date>=index_date,
         (year(sv_date)-year(index_date))*12+month(sv_date)-month(index_date)<=6)

# check if there are several records with duplicated KLIN and SPEC on the same day for the same individual
# View(sv_hcru %>% add_count(lopnr,sv_date,KLIN,SPEC) %>%  filter(n>1))
# View(val_slv %>% add_count(INDAT,UTDAT,KLIN,SPEC,VDAGH) %>%  
#        filter(n>1) %>% 
#        arrange(lopnr,INDAT) %>% 
#        select(lopnr,INDAT,UTDAT,KLIN,SPEC,VDAGH,contains(c("diag")),everything()))

sv_hcru<-sv_hcru %>% 
  distinct(lopnr,sv_date,KLIN,SPEC,.keep_all = T)

# check overlapping between TakeCare data and VAL
# In theory, all records in TakeCare should be in VAL
sv_hcru<-sv_hcru %>% 
  full_join(takecare %>% 
              select(lopnr,Inlagd_den) %>% 
              mutate(takecare_date=as.Date((Inlagd_den))),
            by=c("lopnr","sv_date"="takecare_date"))

table(is.na(sv_hcru$Inlagd_den))

# check records that only exist in takecare but not VAL (none)
# View(sv_hcru %>% filter(!is.na(Inlagd_den),
#                         is.na(sv_date)))

# check records that only exist in VAL but not takecare 
# View(sv_hcru %>% filter(is.na(Inlagd_den),
#                         !is.na(sv_date)))


# Check SPEC (Den vårdande enhetens specialitetskod) in records that only exist in VAL but not takecare 
table((sv_hcru %>% filter(is.na(Inlagd_den),
                        !is.na(sv_date)))$SPEC)


# create a variable that indicates different types of inpatient care:
# 1=geriatric inpatient care - first admission, 2=geriatric inpatient care - re-admission
# 3=other non-geriatric inpatient care
sv_hcru<-sv_hcru %>% 
  mutate(takecare_date=as.Date((Inlagd_den)),
         sv_type=case_when(sv_date==index_date~1,
                        sv_date!=index_date&!is.na(takecare_date)~2,
                        is.na(takecare_date)~3))

table(sv_hcru$sv_type)

# check DRG code
sv_hcru<-sv_hcru %>% 
  mutate(drg_1=substr(DRG,1,1),
         drg_1rm=substr(DRG,2,nchar(DRG)))

table(sv_hcru$drg_1,useNA="ifany")


# use DRG codes after removing the first letter
sv_hcru<-sv_hcru %>% 
  select(-c(DRG,drg_1)) %>% 
  rename(DRG=drg_1rm)




psyk_sv<-read_excel("Malnutrition statistical analysis/Malnutrition-study/MVO by care_XX_202510.xlsx",
                    sheet = "Psyk_sv") %>% 
  as.data.frame %>% 
  select(-contains("_ave")) %>% 
  pivot_longer(
    cols = starts_with(c("mvo_", "cost_")),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) %>% 
  filter(!is.na(cost)) %>% 
  mutate(mvo=substr(mvo,1,3))

soma_sv<-read_excel("Malnutrition statistical analysis/Malnutrition-study/MVO by care_XX_202510.xlsx",
                    sheet = "Soma_sv") %>% 
  as.data.frame %>% 
  as.data.frame %>% 
  select(-contains("_ave")) %>% 
  pivot_longer(
    cols = starts_with(c("mvo_", "cost_")),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) %>% 
  filter(!is.na(cost)) %>% 
  mutate(mvo=substr(mvo,1,3))

mvo_sv<-psyk_sv %>% 
  mutate(vtype="psyk") %>% 
  rbind(soma_sv %>% 
          mutate(vtype="soma")) %>% 
  filter(year %in% c(2018,2019,2020)) %>% 
  group_by(year,mvo) %>% 
  summarise(cost=mean(cost)) %>%
  ungroup



# inflate to 2024 price
mvo_sv<-mvo_sv %>% 
  arrange(mvo,year) %>% 
  mutate(year=as.numeric(year)) %>% 
  left_join(cpi) %>% 
  mutate(cost=cost*(CPI/((cpi %>% filter(year==2024))$CPI))) %>% 
  select(-CPI)


sv_cost<-sv_hcru %>% 
  left_join(drgnew, by=c("DRG","year")) %>% 
  left_join(drgcost, by=c("year")) %>% 
  mutate(cost_drg=weight*drgcost) %>% 
  left_join(mvo_sv, by=c("year","KLIN"="mvo")) %>% 
  mutate(sv_cost=ifelse(is.na(cost_drg),cost,cost_drg))

summary(sv_cost)
# View(sv_cost %>% filter(is.na(sv_cost)))

# impute missingness with average MVO costs for inpatient care
sv_cost<-sv_cost %>% 
  left_join(mvo_sv %>% 
              filter(mvo=="ave") %>% 
              rename(cost_ave=cost)) %>% 
  mutate(sv_cost=ifelse(is.na(sv_cost),cost_ave,sv_cost),
         sv_cost_m2=los*19453*((cpi %>% filter(year==2024))$CPI)/((cpi %>% filter(year==2023))$CPI))
summary(sv_cost)

# check cost per day based on DRG-method cost estimation
sv_cost %>% 
  summarise(cost=sum(sv_cost,na.rm = T),
            los=sum(los,na.rm = T)) %>% 
  mutate(ave=cost/los)





# 5.4. Drug use ----
# !!! Refine later: Use TLV data to calculate drug costs from TakeCare




# 5.5. Combine all HCRU and costs ----
# 5.5.1. Combine HCRU ----
all_hcru<-sol_hcru_dedup %>% 
  select(lopnr,inst_days:dagv) %>% 
  group_by(lopnr) %>% 
  summarise(across(inst_days:dagv,~sum(.x)),.groups="drop") %>% 
  full_join(pv_hcru %>% 
              select(lopnr) %>% 
              group_by(lopnr) %>% 
              summarise(pv_num=n(),.groups="drop")) %>% 
  full_join(ov_hcru %>% 
              select(lopnr) %>% 
              group_by(lopnr) %>% 
              summarise(ov_num=n(),.groups="drop")) %>% 
  full_join(sv_cost %>% 
              select(lopnr,los) %>% 
              group_by(lopnr) %>% 
              summarise(totalsv_los=sum(los),.groups="drop")) %>% 
  full_join(sv_cost %>% 
              filter(sv_type==1) %>% 
              select(lopnr,los) %>% 
              group_by(lopnr) %>% 
              summarise(geri1st_los=sum(los),.groups="drop")) %>% 
  full_join(sv_cost %>% 
              filter(sv_type==2) %>% 
              select(lopnr,los) %>% 
              group_by(lopnr) %>% 
              summarise(gerireadm_los=sum(los),.groups="drop")) %>% 
  full_join(sv_cost %>% 
              filter(sv_type==3) %>% 
              select(lopnr,los) %>% 
              group_by(lopnr) %>% 
              summarise(othersv_los=sum(los),.groups="drop")) %>% 
  full_join(sv_cost %>% 
              filter(sv_type!=1) %>% 
              select(lopnr,los) %>% 
              group_by(lopnr) %>% 
              summarise(gerireadm_othersv_los=sum(los),.groups="drop")) %>%
  mutate(across(everything(), ~ replace(.x, is.na(.x), 0)))
  
  
summary(all_hcru)


all_hcru %>% 
  summarise(across(inst_days:gerireadm_othersv_los, ~ mean(.x))) %>% 
  View()

# compare with Carl's paper
# higher pv_num (36 vs 27), ov_num (7.8 vs 4.8), gerireadm_othersv_los (7.6 vs 3.7)
# much lower social service hours (153 vs 432)
# check



# 5.5.2. Combine costs ----
all_costs<-sol_costs %>% # social care costs
  select(lopnr,contains("cost")) %>% 
  group_by(lopnr) %>% 
  summarise(across(cost_homecare:sol_cost,~sum(.x)),.groups="drop") %>% 
  full_join(pv_hcru %>% # primary care costs
              select(lopnr,pv_cost) %>% 
              group_by(lopnr) %>% 
              summarise(pv_cost=sum(pv_cost),.groups="drop")) %>% 
  full_join(ov_cost %>% # specialist outpatient care costs
              select(lopnr,ov_cost) %>% 
              group_by(lopnr) %>% 
              summarise(ov_cost=sum(ov_cost),.groups="drop")) %>% 
  full_join(sv_cost %>% # total inpatient care costs
              select(lopnr,sv_cost,sv_cost_m2) %>% 
              group_by(lopnr) %>% 
              summarise(totalsv_cost=sum(sv_cost),
                        totalsv_cost_m2=sum(sv_cost_m2),
                        .groups="drop")) %>% 
  full_join(sv_cost %>% # geriatric inpatient care costs - first admission
              filter(sv_type==1) %>% 
              select(lopnr,sv_cost,sv_cost_m2) %>% 
              group_by(lopnr) %>% 
              summarise(geri1st_cost=sum(sv_cost),
                        geri1st_cost_m2=sum(sv_cost_m2),
                        .groups="drop")) %>% 
  full_join(sv_cost %>% # geriatric inpatient care costs - re-admission
              filter(sv_type==2) %>% 
              select(lopnr,sv_cost,sv_cost_m2) %>% 
              group_by(lopnr) %>% 
              summarise(gerireadm_cost=sum(sv_cost),
                        gerireadm_cost_m2=sum(sv_cost_m2),
                        .groups="drop")) %>% 
  full_join(sv_cost %>% # other inpatient care costs
              filter(sv_type==3) %>% 
              select(lopnr,sv_cost,sv_cost_m2) %>% 
              group_by(lopnr) %>% 
              summarise(othersv_cost=sum(sv_cost),
                        othersv_cost_m2=sum(sv_cost_m2),
                        .groups="drop"))

summary(all_costs)

all_costs<-all_costs %>%
  mutate(across(everything(), ~ replace(.x, is.na(.x), 0)),
         total_cost=sol_cost+pv_cost+ov_cost+totalsv_cost)

summary(all_costs)


# Sanity check
# compare with Carl's paper's results (2023 Euro)
summary(all_costs)
all_costs %>% 
  summarise(across(cost_homecare:total_cost, ~ mean(.x)/10.6)) %>% 
  View()

# initial inpatient stay costs was much lower than Carl's estimate (17237 Euro), check
# use DRG to calculate costs in all takecare records
takecare %>% 
  select(lopnr,Inlagd_den,DRG_Vikt) %>% 
  mutate(gericare_first_date=as.Date(Inlagd_den),
         DRG_Vikt=as.numeric(sub(",", ".",DRG_Vikt, fixed = TRUE)),
         year=as.numeric(year(gericare_first_date))) %>% 
  left_join(drgcost) %>% 
  mutate(gericare_first_cost=DRG_Vikt*drgcost) %>% 
  group_by(lopnr) %>% 
  summarise(gericare_first_cost=sum(gericare_first_cost),.groups = "drop") %>% 
  summary



# use DRG to calculate costs in first admission records from takecare records
takecare %>% 
  select(lopnr,Inlagd_den,DRG_Vikt) %>% 
  right_join(study_pop %>% 
               select(lopnr,Inlagd_den)) %>% 
  mutate(gericare_first_date=as.Date(Inlagd_den),
         DRG_Vikt=as.numeric(sub(",", ".",DRG_Vikt, fixed = TRUE)),
         year=as.numeric(year(gericare_first_date))) %>% 
  left_join(drgcost) %>% 
  mutate(gericare_first_cost=DRG_Vikt*drgcost) %>% 
  summary



# use KPP's unit cost per day i geriatrisk vård and length of stay all records from takecare records
takecare %>% 
  select(lopnr,Inlagd_den,Utskriven_den) %>% 
  mutate(gericare_first_date=as.Date(Inlagd_den),
         Utskriven_den=as.Date(Utskriven_den),
         days=as.numeric(Utskriven_den-gericare_first_date),
         costs=days*19453) %>% # 19453 sek/day in Carl's paper
  group_by(lopnr) %>% 
  summarise(costs=sum(costs),.groups = "drop") %>% 
  summary


# use KPP's unit cost per day i geriatrisk vård and length of stay in first admission records from takecare records
takecare %>% 
  select(lopnr,Inlagd_den,Utskriven_den) %>% 
  right_join(study_pop %>% 
               select(lopnr,Inlagd_den)) %>% 
  mutate(gericare_first_date=as.Date(Inlagd_den),
         Utskriven_den=as.Date(Utskriven_den),
         days=as.numeric(Utskriven_den-gericare_first_date),
         costs=days*19453) %>% # 19453 sek/day in Carl's paper
  group_by(lopnr) %>% 
  summarise(costs=sum(costs),.groups = "drop") %>% 
  summary



# compare with cost_vardtid
takecare %>% 
  select(lopnr,Inlagd_den,Utskriven_den) %>% 
  right_join(study_pop %>% 
               select(lopnr,Inlagd_den)) %>% 
  mutate(gericare_first_date=as.Date(Inlagd_den),
         Utskriven_den=as.Date(Utskriven_den),
         days=as.numeric(Utskriven_den-gericare_first_date),
         costs=days*19453/10.6) %>% 
  left_join(indextable %>% select(lopnr,vardtid,datediff,cost_vardtid,cost_total_vardtid)) %>% 
  summary()




# 6. Combine all variables ----
mna_costs<-study_pop %>% 
  left_join(takecare %>% 
              select(lopnr,Inlagd_den,mna_cat,adl_bi_cat) %>% 
              mutate(index_date=as.Date(Inlagd_den))) %>% 
  left_join(comorb_index) %>% 
  mutate(across(ihd:dem, ~ as.factor(ifelse(is.na(.x),0,.x)))) %>%
  left_join(indextable %>% select(lopnr,death_date,contains("death"))) %>% 
  left_join(all_costs) %>% 
  mutate(fu_death=ifelse(!is.na(death_date),as.numeric(death_date-index_date),
                         180),
         fu_death=pmin(fu_death,180),
         death=!is.na(death_date)&as.numeric(death_date-index_date)<=180)


summary(mna_costs)

mna_costs<-mna_costs %>% 
  mutate(across(c(sol_cost:othersv_cost),
                ~ pmin(.x, quantile(.x[.x>0], 0.95, na.rm = TRUE)),
                .names="{.col}_trim")) %>% 
  mutate(total_cost_trim=sol_cost_trim+pv_cost_trim+ov_cost_trim+totalsv_cost_trim)

write_dta(mna_costs,"Organized data/slso_nutri_hcru_cov_xx.dta")


# 7. Summary of the study population ----
label(mna_costs$age)<-"Age"
label(mna_costs$sex)<-"Sex"
label(mna_costs$edu_level)<-"Education"
label(mna_costs$ensamboende)<-"Living alone"
label(mna_costs$mna_cat)<-"Nutritional status (MNA)"
label(mna_costs$ihd)<-"Ischemic heart disease"
label(mna_costs$hf)<-"Heart failure"
label(mna_costs$stroke)<-"Stroke"
label(mna_costs$pd)<-"Parkinson's disease"
label(mna_costs$copd)<-"Chronic obstructive pulmonary disease"
label(mna_costs$ra)<-"Rheumatoid arthritis"
label(mna_costs$op)<-"Osteoporosis"
label(mna_costs$cancer)<-"Cancer"
label(mna_costs$depression)<-"Depression"
label(mna_costs$dem)<-"Dementia"



table1<-table1::table1(~age+factor(sex)+edu_level+factor(ensamboende)+mna_cat+factor(adl_bi_cat)+
                         ihd+hf+stroke+pd+copd+ra+op+cancer+depression+dem|age_group,
                       data = mna_costs %>% 
                         mutate(age_group=as.factor(case_when(age<70~"<70 years",
                                                              age<80~"70-79 years",
                                                              age>=80~"80+ years")))) %>% 
  as.data.frame()

colnames(table1)[1]<-"Characteristics"
class(table1)<-"data.frame"
table1<-table1 %>% 
  filter(!grepl("0|Male|Median",Characteristics))

table1[]<-lapply(table1, function(x) gsub("%", "", x))




