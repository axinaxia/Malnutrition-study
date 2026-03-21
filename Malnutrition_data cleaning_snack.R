library(tidyverse)
library(haven)
library(lubridate)
library(readxl)
library(lubridate)

setwd("/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/")

# 1. load data ----
# 1.1. HCRU data ----
# VAL database (all types of care visits), only available for 2001-2012
val_2001_2012<-read_sav("npr and val/ORV_val_SNAC-K_C1_C2.sav") %>% 
  as.data.frame() %>% 
  mutate(year=as.numeric(substr(bdat,1,4)),
         vardniva=ifelse(vardniva=="",NA,vardniva)) %>% 
  rename(lopnr=Löpnr)



# View(val_2001_2012)

# NPR before 2017
npr_before2017<-read_sav("npr and val/Patientregistry from SoS 1968-2016_C1_C2.sav") %>% 
  as.data.frame()
# View(npr_before2017)

# Outpatient care from NPR between 2017-2023
ov_after2016<-read_sav("npr and val/PAR_OV_2017-2023_C1_C2.sav") %>% 
  as.data.frame()
# View(ov_after2016)

# Inpatient care from NPR between 2017-2023
sv_after2016<-read_sav("npr and val/PAR_SV_2017-2023_C1_C2.sav") %>% 
  as.data.frame()
# View(sv_after2016)


# drug register
# only available after 2005-05-11, PDR started in 2005
drug<-read_sav("LMED_SNAC_K_Ph5_7_C1_C2.sav") %>% 
  as.data.frame() %>% 
  mutate(EDATUM=ymd(EDATUM))


# 1.2. Malnutrition, attrition, and covariates data ----
nutri_attr_cov<-read_dta("Malnutrition and HCRU SNACK extra variables.dta") %>% 
  as.data.frame()
# View(nutri_attr_cov)

nutri<-read_dta("Malnutrition and HCRU SNACK.dta") %>% 
  as.data.frame()


# Medical history interview
med_hist_w1<-read_sav("diagnosis and anamnesis wave 1-3/wave 1/SNAC-K wave 1 anamnesis clean.sav") %>% 
  as.data.frame()


# SNAC-K physician diagnosis
phy_diag_w1<-read_sav("diagnosis and anamnesis wave 1-3/wave 1/SNAC-K wave 1_diseases_ 14 diagosis.sav") %>% 
  as.data.frame()

# nurse interview on smell, ADL, etc
nurse_int_w1<-read_sav("Nurse wave 1-3/wave1/SNAC-K nurse wave1.sav") %>% 
  as.data.frame()


# 1.3. Death information collected by SNAC-K coordinator ----
death<-read_sav("SNAC-K c1 c2 death date from coordinator.sav") %>% 
  as.data.frame() %>% 
  mutate(dödår_f7=dödår_f7+2000,
         # assume an approximate death date as the first day of the week reported
         death_date_appro=as.Date(paste(dödår_f7,dödvecka_f7, 1, sep = "-"), format = "%Y-%W-%u"),
         dödsdatum_f7=as.Date(paste0("20", dödsdatum_f7), format = "%Y%m%d"),
         death_date_nurse=as.Date(ifelse(is.na(dödsdatum_f7),death_date_appro,dödsdatum_f7))) %>% 
  rename(lopnr=Löpnr)


# 2. Define follow-up and divide follow-up period into 1-year interval ----
#**************************************************************************************
# Define follow-up period
# 1. Follow until: Study withdrawal, death, or end of observational period
# 2. End of observational period for analysis of primary care use: 2011-12-31
# 3. End of observational period for analysis of other HCRU: 2019-12-31
# Meaning of participate status: 
# 1=participated, 2=refused, 4=relative refused,
# 8=no contact, 9=moved, 11=died, 12=cancelled testing
# Note: Some individuals lack data on death dates
#**************************************************************************************
attri<-nutri_attr_cov %>% 
  select(lopnr,date1:date6,status_wave1:status_wave6,DODSDAT) %>% 
  left_join(death %>% select(lopnr,death_date_nurse)) %>% 
  mutate(DODSDAT=ymd(DODSDAT),
         # impute missing death dates with the dates collected by nurses
         DODSDAT=as.Date(ifelse(is.na(DODSDAT),death_date_nurse,DODSDAT))) %>% 
  mutate(fu_date_max=as.Date(pmax(date1,date2,date3,date4,
                                  date5,date6,na.rm = T)),
         death=ifelse(status_wave2==11|status_wave3==11|
                        status_wave4==11|status_wave5==11|
                        status_wave6==11,1,0),
         death=ifelse(is.na(death),0,death),
         death_date=as.Date(ifelse(death==1,DODSDAT,NA)),
         fu_date_max=as.Date(ifelse(death==1,death_date,fu_date_max)),
         fu=as.numeric((fu_date_max-date1)/365.25),
         
         # follow-up time for primary care analysis
         pv_fu_date_max=as.Date(pmin(pmax(date1,date2,date3,date4,date5,na.rm = T),
                                     as.Date("2011-12-31"),na.rm = T)),
         pv_death_date=as.Date(ifelse((status_wave2==11|status_wave3==11|
                                         status_wave4==11|status_wave5==11)&
                                        death_date<=as.Date("2011-12-31"),death_date,NA)),
         # lopnr 155 has date as "2006-01-14" but death date as "2005-01-05", set maximum follow-up date as "2005-01-05"
         pv_fu_date_max=as.Date(ifelse(lopnr==155,as.Date("2005-01-05"),pv_fu_date_max)),
         pv_fu_date_max=as.Date(pmax(pv_fu_date_max,pv_death_date,na.rm = T)),
         pv_death=ifelse((status_wave2==11|status_wave3==11|
                            status_wave4==11|status_wave5==11)&
                           death_date<=as.Date("2011-12-31"),1,0),
         pv_death=ifelse(is.na(pv_death),0,pv_death),
         pv_fu=as.numeric((pv_fu_date_max-date1)/365.25)) 

summary(attri)

# set follow-up years for each individual
snack_fu_years<-attri %>% 
  select(lopnr,fu,date1) %>%
  mutate(fu_max=ceiling(fu)) %>%                    
  uncount(weights=fu_max,.id = "fu_year") %>% 
  mutate(fu_year=fu_year-1,
         hcru_year=as.numeric(year(date1))+fu_year) %>% 
  select(lopnr,date1,fu_year,fu,hcru_year)

n_distinct(snack_fu_years$lopnr) 


# set follow-up years for each individual for analyses of primary care
snack_pv_fu_years<-attri %>% 
  select(lopnr,pv_fu,date1) %>%
  mutate(fu_max=ceiling(pv_fu)) %>%                    
  uncount(weights=fu_max,.id = "fu_year") %>% 
  mutate(fu_year=fu_year-1,
         hcru_year=as.numeric(year(date1))+fu_year) %>% 
  select(lopnr,date1,fu_year,pv_fu,hcru_year)

n_distinct(snack_fu_years$lopnr) 



# 3. Formal care data cleaning, restrict to between 2001-2019 ----
#*********************************************************************************
# Summarise both number of visits and costs
# Use average costs per MVO to calculate costs (MVO in primary care is KLIN)
# Change the last year's HCRU to HCRU/actual fu time in the last year interval
# Avoid extreme estimates by restricting to fu-fu_year_max>=1 moth
# Merge records for fu-fu_year_max<1 moth into previous follow-up year
# For people with fu<1 moth, change first year's HCRU to HCRU/fu
#*********************************************************************************
# 3.1. Average costs per MVO and CPI ----
cpi<-read_excel("Malnutrition statistical analysis/Malnutrition-study/CPI.xlsx") %>% as.data.frame()

# average costs per MVO for specialist outpatient care and inpatient care
# for psychiatric care, MVO only available for 2018 and 2019
psyk_sv<-read_excel("Malnutrition statistical analysis/Malnutrition-study/MVO by care_XX_202510.xlsx",
                sheet = "Psyk_sv") %>% 
  as.data.frame

psyk_ov<-read_excel("Malnutrition statistical analysis/Malnutrition-study/MVO by care_XX_202510.xlsx",
                    sheet = "Psyk_ov") %>% 
  as.data.frame

psyk_npr<-psyk_sv %>% 
  mutate(vtype="sv") %>% 
  rbind(psyk_ov %>% 
          mutate(vtype="ov"))

# use costs per MVO for 2018 as proxy for 2001-2017, adjusting for inflation
mvo_psyk_npr<-psyk_npr %>% 
  select(-contains("_ave")) %>% 
  pivot_longer(
    cols = starts_with(c("mvo_", "cost_")),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) %>% 
  filter(!is.na(cost)) %>% 
  mutate(mvo=substr(mvo,1,3))

mvo_psyk_npr_2001_2011<-mvo_psyk_npr %>%
  filter(year==2018) %>%
  select(vtype,mvo,cost) %>%
  crossing(year=as.character(2001:2017)) %>% 
  left_join(cpi) %>% 
  mutate(cost=cost*(CPI/((cpi %>% filter(year==2018))$CPI)))

mvo_psyk_npr<-mvo_psyk_npr %>% 
  rbind(mvo_psyk_npr_2001_2011 %>% 
          select(-CPI)) %>% 
  arrange(year,mvo) %>% 
  mutate(year=as.numeric(year))


# for somatic disease care, MVO available for 2012-2019
soma_sv<-read_excel("Malnutrition statistical analysis/Malnutrition-study/MVO by care_XX_202510.xlsx",
                    sheet = "Soma_sv") %>% 
  as.data.frame

soma_ov<-read_excel("Malnutrition statistical analysis/Malnutrition-study/MVO by care_XX_202510.xlsx",
                    sheet = "Soma_ov") %>% 
  as.data.frame

soma_npr<-soma_sv %>% 
  mutate(vtype="sv") %>% 
  rbind(soma_ov %>% 
          mutate(vtype="ov"))

# use costs per MVO for 2012 as proxy for 2001-2011, adjusting for inflation
mvo_soma_npr<-soma_npr %>% 
  select(-contains("_ave")) %>% 
  pivot_longer(
    cols = starts_with(c("mvo_", "cost_")),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) %>% 
  filter(!is.na(cost)) %>% 
  mutate(mvo=substr(mvo,1,3))

mvo_soma_npr_2001_2011<-mvo_soma_npr %>%
  filter(year==2012) %>%
  select(vtype,mvo,cost) %>%
  crossing(year=as.character(2001:2011)) %>% 
  left_join(cpi) %>% 
  mutate(cost=cost*(CPI/((cpi %>% filter(year==2012))$CPI)))

mvo_soma_npr<-mvo_soma_npr %>% 
  rbind(mvo_soma_npr_2001_2011 %>% 
          select(-CPI)) %>% 
  arrange(year,mvo) %>% 
  mutate(year=as.numeric(year))

mvo_npr<-mvo_psyk_npr %>% 
  rbind(mvo_soma_npr) %>% 
  arrange(year,mvo,vtype) %>% 
  group_by(year,mvo,vtype) %>% 
  summarise(cost=mean(cost)) %>%
  ungroup



# average costs per MVO for primary care (MVO only available for 2019)
# assume primary care costs per MVO are the same across the years, adjusting for inflation
mvo_pv<-read_excel("Malnutrition statistical analysis/Malnutrition-study/MVO by care_XX_202510.xlsx",
                    sheet = "PV") %>% 
  as.data.frame %>% 
  select(mvo_2019,cost_2019) %>% 
  rename(mvo=mvo_2019,
         cost=cost_2019) %>% 
  mutate(mvo=substr(mvo,1,3),
         year=2019)

mvo_pv_2001_2018<-mvo_pv %>% 
  select(mvo, cost) %>%
  crossing(year=2001:2018) %>% 
  mutate(year=as.character(year)) %>% 
  left_join(cpi) %>% 
  mutate(cost=cost*(CPI/((cpi %>% filter(year==2019))$CPI)))
  
mvo_pv<-mvo_pv %>% 
  rbind(mvo_pv_2001_2018 %>% 
          select(-CPI)) %>% 
  arrange(year,mvo) %>% 
  mutate(year=as.numeric(year))



# 3.2. Primary care ----
# **********************************************************************************
# VARDNIVA: 01 = Primärvård 02 = Specialiserad vård
# VARDNIVA for 2001 and 2002 are missing
# treat records between 2001 and 2002 as specialist care
# **********************************************************************************
# 3.2.1. Remove records that are in NPR between 2001 and 2002 ----
pv_hcru2001_2002<-val_2001_2012 %>% 
  filter(year %in% c(2001,2002)) %>%
  mutate(pv_date=ymd(bdat)) %>% 
  left_join(npr_before2017 %>% 
              select(Löpnr,INDATUMA,MVO) %>% 
              rename(lopnr=Löpnr) %>% 
              filter(as.numeric(substr(INDATUMA,1,4))<=2002),
            relationship = "many-to-many") %>% 
  mutate(INDATUMA=ymd(INDATUMA),
         dup_record=ifelse(pv_date==INDATUMA&klin==MVO,1,0),
         dup_record=ifelse(is.na(dup_record),0,dup_record)) %>% 
  filter(dup_record==0) %>% 
  select(-c(INDATUMA,dup_record,MVO))
  
pv_hcru2003_2011<-val_2001_2012 %>% 
  filter(year>2002) %>%
  mutate(pv_date=ymd(bdat)) %>% 
  filter(vardniva=="01")
  

pv_hcru<-pv_hcru2001_2002 %>%
  rbind(pv_hcru2003_2011) %>%  
  distinct(lopnr,klin,mott,spec,bdat,.keep_all = T) %>% 
  right_join(attri %>% select(lopnr,date1,pv_fu_date_max)) %>% 
  filter(pv_date>=date1&pv_date<=pv_fu_date_max) %>% 
  mutate(hcru_year=as.numeric(year(pv_date)),
         fu_year=floor(as.numeric((pv_date-date1)/365.25)))


  
# 3.2.2. Number of primary care visits per year ----
pv_num<-pv_hcru %>% 
  select(lopnr,pv_date,fu_year) %>%
  group_by(lopnr,fu_year) %>% 
  summarise(pv_num=n()) %>% 
  ungroup %>% 
  right_join(snack_pv_fu_years) %>% 
  group_by(lopnr) %>% 
  mutate(pv_num=ifelse(is.na(pv_num),0,pv_num)) %>% 
  ungroup %>% 
  arrange(lopnr,fu_year)

summary(pv_num)
summary(pv_num$pv_num)


pv_num<-pv_num %>% 
  group_by(lopnr) %>% 
  mutate(fu_year_max=max(fu_year)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year),
         pv_num=ifelse(last_year==1&(pv_fu-fu_year_max)>=(1/12)|pv_fu<(1/12),
                       round(pv_num/(pv_fu-fu_year_max),0),
                       pv_num),
         fu_year=ifelse(last_year==1&(pv_fu-fu_year_max)<(1/12)&pv_fu>=(1/12),
                          fu_year-1,
                          fu_year))%>% 
  group_by(lopnr,fu_year) %>% 
  summarise(pv_num=sum(pv_num)) %>%
  ungroup %>% 
  mutate(pv_num=round(pv_num,0))


# 3.2.3. Costs of primary care per year ----
pv_costs<-pv_hcru %>% 
  select(lopnr,klin,pv_date,hcru_year,fu_year) %>% 
  left_join(mvo_pv %>% 
              rename(klin=mvo,
                     hcru_year=year))

# use average costs per MVO to impute missing costs per MVO 
pv_costs<-pv_costs %>% 
  left_join(mvo_pv %>% 
              filter(mvo=="ave") %>% 
              select(hcru_year=year,cost_ave=cost)) %>% 
  mutate(cost=ifelse(is.na(cost),cost_ave,cost)) %>% 
  select(-cost_ave) %>% 
  group_by(lopnr,hcru_year,fu_year) %>% 
  summarise(cost=sum(cost)) %>% 
  ungroup %>% 
  right_join(snack_pv_fu_years) %>% 
  group_by(lopnr) %>% 
  mutate(cost=ifelse(is.na(cost),0,cost)) %>% 
  ungroup

pv_costs<-pv_costs %>% 
  group_by(lopnr) %>% 
  mutate(fu_year_max=max(fu_year)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year),
         cost=ifelse(last_year==1&(pv_fu-fu_year_max)>=(1/12)|pv_fu<(1/12),
                       round(cost/(pv_fu-fu_year_max),0),
                       cost),
         fu_year=ifelse(last_year==1&(pv_fu-fu_year_max)<(1/12)&pv_fu>=(1/12),
                        fu_year-1,
                        fu_year)) %>% 
  group_by(lopnr,hcru_year,fu_year) %>% 
  summarise(cost=sum(cost)) %>%
  ungroup %>% 
  arrange(lopnr,fu_year)


# convert costs to 2024 SEK
pv_costs<-pv_costs %>% 
  left_join(cpi %>% 
              mutate(hcru_year=as.numeric(year))) %>% 
  mutate(cost=cost*(((cpi %>% filter(year==2024))$CPI)/CPI)) %>% 
  group_by(lopnr,fu_year) %>% 
  summarise(cost=sum(cost)) %>%
  mutate(cost=ifelse(is.na(cost),0,cost)) %>%
  ungroup %>% 
  mutate(cost=round(cost,0))

summary(pv_costs)




# 3.3. Outpatient care ----
# outpatient care MVO
ov_hcru<-ov_after2016 %>% 
  select(Löpnr,MVO,INDATUMA) %>%
  mutate(hcru_year=as.numeric(substr(INDATUMA,1,4)),
         INDATUMA=ymd(INDATUMA)) %>% 
  bind_rows(npr_before2017 %>% 
              filter(is.na(UTDATUM)) %>% # assume records without UTDATUM as outpatient visits
              select(Löpnr,MVO,INDATUMA) %>%
              mutate(hcru_year=as.numeric(substr(INDATUMA,1,4)),
                     INDATUMA=ymd(INDATUMA)) %>% 
              mutate(MVO=as.numeric(MVO))) %>% 
  arrange(Löpnr,INDATUMA) %>% 
  rename(lopnr=Löpnr) %>% 
  right_join(attri %>% select(lopnr,date1,fu_date_max)) %>% 
  filter(INDATUMA>=date1&INDATUMA<=fu_date_max) %>% 
  mutate(fu_year=floor(as.numeric((INDATUMA-date1)/365.25)))




# 3.3.1. Number of outpatient visits per year ----
ov_num<-ov_hcru %>% 
  select(lopnr,INDATUMA,fu_year) %>%
  group_by(lopnr,fu_year) %>% 
  summarise(ov_num=n()) %>% 
  ungroup %>% 
  right_join(snack_fu_years) %>% 
  group_by(lopnr) %>% 
  mutate(ov_num=ifelse(is.na(ov_num),0,ov_num)) %>% 
  ungroup %>% 
  arrange(lopnr,fu_year)

summary(ov_num)
summary(ov_num$ov_num)


ov_num<-ov_num %>% 
  group_by(lopnr) %>% 
  mutate(fu_year_max=max(fu_year)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year),
         ov_num=ifelse(last_year==1&(fu-fu_year_max)>=(1/12)|fu<(1/12),
                       round(ov_num/(fu-fu_year_max),0),
                       ov_num),
         fu_year=ifelse(last_year==1&(fu-fu_year_max)<(1/12)&fu>=(1/12),
                        fu_year-1,
                        fu_year))%>% 
  group_by(lopnr,fu_year) %>% 
  summarise(ov_num=sum(ov_num)) %>%
  ungroup %>% 
  mutate(ov_num=round(ov_num,0))


# 3.3.2. Costs of outpatient care per year ----
ov_costs<-ov_hcru %>% 
  select(lopnr,MVO,INDATUMA,hcru_year,fu_year) %>% 
  left_join(mvo_npr %>% 
              filter(vtype=="ov") %>% 
              rename(MVO=mvo,
                     hcru_year=year) %>% 
              mutate(MVO=as.numeric(MVO)))


# use average costs per MVO to impute missing costs per MVO 
ov_costs<-ov_costs %>% 
  left_join(mvo_npr %>% 
              filter(mvo=="ave",
                     vtype=="ov") %>% 
              select(hcru_year=year,cost_ave=cost)) %>% 
  mutate(cost=ifelse(is.na(cost),cost_ave,cost)) %>% 
  select(-cost_ave) %>% 
  group_by(lopnr,hcru_year,fu_year) %>% 
  summarise(cost=sum(cost)) %>% 
  ungroup %>% 
  right_join(snack_fu_years) %>% 
  group_by(lopnr) %>% 
  mutate(cost=ifelse(is.na(cost),0,cost)) %>% 
  ungroup



ov_costs<-ov_costs %>% 
  group_by(lopnr) %>% 
  mutate(fu_year_max=max(fu_year)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year),
         cost=ifelse(last_year==1&(fu-fu_year_max)>=(1/12)|fu<(1/12),
                     round(cost/(fu-fu_year_max),0),
                     cost),
         fu_year=ifelse(last_year==1&(fu-fu_year_max)<(1/12)&fu>=(1/12),
                        fu_year-1,
                        fu_year)) %>%  
  group_by(lopnr,hcru_year,fu_year) %>% 
  summarise(cost=sum(cost)) %>%
  ungroup %>% 
  arrange(lopnr,fu_year)

# convert costs to 2024 SEK
ov_costs<-ov_costs %>% 
  left_join(cpi %>% 
              mutate(hcru_year=as.numeric(year))) %>% 
  mutate(cost=cost*(((cpi %>% filter(year==2024))$CPI)/CPI)) %>% 
  group_by(lopnr,fu_year) %>% 
  summarise(cost=sum(cost)) %>%
  mutate(cost=ifelse(is.na(cost),0,cost)) %>%
  ungroup %>% 
  mutate(cost=round(cost,0))



# 3.4. Inpatient care ----
sv_hcru<-sv_after2016 %>% 
  select(Löpnr,MVO,INDATUMA,UTDATUMA) %>%
  mutate(hcru_year=as.numeric(substr(INDATUMA,1,4)),
         INDATUMA=ymd(INDATUMA),
         UTDATUMA=ymd(UTDATUMA)) %>% 
  bind_rows(npr_before2017 %>% 
              filter(!is.na(UTDATUM)) %>% # assume records without UTDATUM as outpatient visits
              select(Löpnr,MVO,INDATUMA,UTDATUMA) %>%
              mutate(hcru_year=as.numeric(substr(INDATUMA,1,4)),
                     INDATUMA=ymd(INDATUMA),
                     UTDATUMA=ymd(UTDATUMA)) %>% 
              mutate(MVO=as.numeric(MVO))) %>% 
  arrange(Löpnr,INDATUMA) %>% 
  rename(lopnr=Löpnr) %>% 
  right_join(attri %>% select(lopnr,date1,fu_date_max)) %>% 
  filter(INDATUMA>=date1&INDATUMA<=fu_date_max) %>% 
  mutate(los=as.numeric(UTDATUMA-INDATUMA),
         fu_year=floor(as.numeric((INDATUMA-date1)/365.25)))

summary(sv_hcru)

# 3.4.1. Inpatient care visits and length of stay ----
sv_num<-sv_hcru %>% 
  select(lopnr,INDATUMA,fu_year) %>%
  group_by(lopnr,fu_year) %>% 
  summarise(sv_num=n()) %>% 
  ungroup %>% 
  right_join(snack_fu_years) %>% 
  group_by(lopnr) %>% 
  mutate(sv_num=ifelse(is.na(sv_num),0,sv_num)) %>% 
  ungroup %>% 
  arrange(lopnr,fu_year)

summary(sv_num)
summary(sv_num$sv_num)

sv_num<-sv_num %>% 
  group_by(lopnr) %>% 
  mutate(fu_year_max=max(fu_year)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year),
         sv_num=ifelse(last_year==1&(fu-fu_year_max)>=(1/12)|fu<(1/12),
                       round(sv_num/(fu-fu_year_max),0),
                       sv_num),
         fu_year=ifelse(last_year==1&(fu-fu_year_max)<(1/12)&fu>=(1/12),
                        fu_year-1,
                        fu_year))%>% 
  group_by(lopnr,fu_year) %>% 
  summarise(sv_num=sum(sv_num)) %>%
  ungroup %>% 
  mutate(sv_num=round(sv_num,0))



# 3.4.2. Costs of inpatient care per year ----
sv_costs<-sv_hcru %>% 
  select(lopnr,MVO,INDATUMA,hcru_year,fu_year) %>% 
  left_join(mvo_npr %>% 
              filter(vtype=="sv") %>% 
              rename(MVO=mvo,
                     hcru_year=year) %>% 
              mutate(MVO=as.numeric(MVO)))

# use average costs per MVO to impute missing costs per MVO 
sv_costs<-sv_costs %>% 
  left_join(mvo_npr %>% 
              filter(mvo=="ave",
                     vtype=="sv") %>% 
              select(hcru_year=year,cost_ave=cost)) %>% 
  mutate(cost=ifelse(is.na(cost),cost_ave,cost)) %>% 
  select(-cost_ave) %>% 
  group_by(lopnr,hcru_year,fu_year) %>% 
  summarise(cost=sum(cost)) %>% 
  ungroup %>% 
  right_join(snack_fu_years) %>% 
  group_by(lopnr) %>% 
  mutate(cost=ifelse(is.na(cost),0,cost)) %>% 
  ungroup

sv_costs<-sv_costs %>% 
  group_by(lopnr) %>% 
  mutate(fu_year_max=max(fu_year)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year),
         cost=ifelse(last_year==1&(fu-fu_year_max)>=(1/12)|fu<(1/12),
                     round(cost/(fu-fu_year_max),0),
                     cost),
         fu_year=ifelse(last_year==1&(fu-fu_year_max)<(1/12)&fu>=(1/12),
                        fu_year-1,
                        fu_year)) %>% 
  group_by(lopnr,hcru_year,fu_year) %>% 
  summarise(cost=sum(cost)) %>%
  ungroup %>% 
  arrange(lopnr,fu_year)


# convert costs to 2024 SEK
sv_costs<-sv_costs %>% 
  left_join(cpi %>% 
              mutate(hcru_year=as.numeric(year))) %>% 
  mutate(cost=cost*(((cpi %>% filter(year==2024))$CPI)/CPI)) %>% 
  group_by(lopnr,fu_year) %>% 
  summarise(cost=sum(cost)) %>%
  mutate(cost=ifelse(is.na(cost),0,cost)) %>%
  ungroup %>% 
  mutate(cost=round(cost,0))

summary(sv_costs)


# 3.5. Drug use ----
# only data after 2005 are available
drug_hcru<-drug %>% 
  select(lopnr=Löpnr,EDATUM,TKOST) %>% 
  filter(TKOST>0) %>% 
  mutate(hcru_year=as.numeric(substr(EDATUM,1,4))) %>% 
  right_join(attri %>% select(lopnr,date1,fu_date_max)) %>% 
  filter(EDATUM>=date1&EDATUM<=fu_date_max) %>% 
  mutate(fu=as.numeric((fu_date_max-date1)/365.25),
         fu_year=as.integer((EDATUM-date1)/365.25))




# 4. Combine all HCRU and costs ----
all_hcru<-ov_num %>% 
  left_join(ov_costs) %>% 
  rename(ov_cost=cost) %>%
  full_join(sv_num %>% 
              left_join(sv_costs %>% 
                          rename(sv_cost=cost))) %>% 
  mutate_all(~ replace_na(., 0)) %>% 
  full_join(pv_num %>% 
              left_join(pv_costs %>% 
                          rename(pv_cost=cost)))

n_distinct(all_hcru$lopnr)
summary(all_hcru)


# 5. Covariate data cleaning ----
# 5.1. Cleaning diseases ----
#**************************************************************************************
# 1. Diseases considered in the study: MI, HF, Stroke, PD, COPD, RA, 
#    osteoporosis, cancer, depression, dementia
# 2. Identifying diseases from SNAC-K physicians' diagnosis, medical history interview, 
#    NPR (5 years within baseline), and VAL databasen
# 3. 1=Yes, 2=No, 8=No response, 9=Don't know
# 4. In NPR, ICD-9 was used <=1996
#**************************************************************************************
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
  summarise(across(mi:dementia, ~ as.integer(any(. == 1))), .groups = "drop")


# 5.2. Smell, taste chewing function, and ADL ----
#**************************************************************************************
# Meaning of answers:
# Appetite (Has the proband in the last 3 months been bothered by any of the following symptoms):
# 1. Poor appetite 2. Weight loss 3. Excess Weight 4. Nausea 5. Dry mouth 
# 6. Stomach pains 7. Diarrhea 8. Constipation

# Smell: 1. I have a normal sense of smell 2. My sense of smell is slightly reduced 
# 3. My sense of smell is greatly reduced but not completely gone 
# 4. I have completely lost my sense of smell 
# 5. My sense of smell is better than average and very sensitive 
# 8. No response 9. Don't know

# Tastes: 1. I have a normal sense of taste 2. My sense of taste is slightly reduced 
# 3. My sense of taste is greatly reduced but not completely gone 
# 4. I have completely lost my sense of taste 
# 5. My sense of taste is better than average and very sensitive 
# 8. No reponse 9. DOn't know

# Chew: 1. Yes, without trouble 2. Yes, but must be careful 
# 3. No, not at all 8. No response 9. Don't know
#**************************************************************************************
smell_adl_w1<-nurse_int_w1 %>% 
  select(Löpnr,N1E110,N1E111,N1E76) %>% 
  set_names(c("lopnr","smell","taste","chew")) %>% 
  mutate(across(smell:chew, ~ if_else(.x %in% c(8, 9), NA, .x))) %>% 
  mutate(smell_imp=ifelse(smell %in% c(2,3,4),1,0),
         taste_imp=ifelse(taste %in% c(2,3,4),1,0),
         chew_imp=ifelse(chew %in% c(2,3),1,0))

appetite<-nutri_attr_cov %>% 
  select(lopnr,PH1G14A_1:PH1G14A_8) %>% 
  mutate(poor_appetite=ifelse(PH1G14A_1==1|PH1G14A_2==1|
                                PH1G14A_3==1|PH1G14A_4==1|
                                PH1G14A_5==1|PH1G14A_6==1|
                                PH1G14A_7==1|PH1G14A_8==1,1,0))


# 6. Combine exposure data, HCRU data, covariate data, and attrition data ----
nutri_hcru_cov<-nutri_attr_cov %>% 
  mutate(bmi_w1=weight1/((height1/100)^2),
         bmi_cat_w1=case_when(bmi_w1<18.5~1,
                              bmi_w1<25~2,
                              bmi_w1<30~3,
                              bmi_w1>=30~4)) %>% 
  select(lopnr,bmi_cat_w1) %>% 
  left_join(nutri %>% select(lopnr,age1,sex,education,occupation,lives1,tobacco1,
                             mna_screening1_cat,glim_malnutr1) %>% 
              mutate(sex=factor(sex,levels = 0:1,labels = c("Male","Female")),
                     education=factor(education,levels = 0:2,
                                      labels = c("Elementary","High school","University")),
                     occupation=factor(occupation,levels = 0:1,
                                       labels = c("Manual workder","Non-manual worker")),
                     lives1=factor(lives1,levels = c(1,0),labels = c("Not alone","Alone")),
                     tobacco1=factor(tobacco1,levels = 0:2,
                                     labels = c("Never","Former","Current")),
                     mna_screening1_cat=factor(mna_screening1_cat,levels = 0:2,
                                               labels = c("Normal","At risk","Malnourished")),
                     glim_malnutr1=factor(glim_malnutr1,levels = 0:2,
                                          labels = c("No malnutrition","Moderate malnutrition","Severe malnutrition")))) %>% 
  left_join(dis_w1) %>% 
  left_join(smell_adl_w1 %>% select(lopnr,contains("imp"))) %>% 
  left_join(appetite %>% select(lopnr,poor_appetite)) %>% 
  mutate(across(c(mi:poor_appetite), ~ if_else(is.na(.x), 0, .x)),
         across(c(mi:poor_appetite),~ factor(.x)),
         bmi_cat_w1=factor(bmi_cat_w1,levels = c(2,3,4,1),
                           labels = c("Normal weight","Overweight","Obese","Underweight")),
         pd_dem=as.factor(ifelse(pd==1|dementia==1,1,0)),
         ra_op=as.factor(ifelse(ra==1|op==1,1,0)),
         smell_taste_imp=as.factor(ifelse(smell_imp==1|taste_imp==1,1,0))) %>% 
  full_join(all_hcru) %>%
  left_join(attri %>% 
              select(lopnr,status_wave2,status_wave3,
                     date1,fu,death,pv_fu,pv_death,death_date,pv_death_date)) %>%
  mutate_at(vars(ov_num, ov_cost, sv_num, sv_cost), ~ replace_na(., 0)) %>%
  mutate(across(c(pv_num, pv_cost),~ if_else(fu_year<=pv_fu, replace_na(., 0), .))) %>% 
  mutate(total_cost=ov_cost+sv_cost+pv_cost)

table((nutri_hcru_cov %>% 
         filter(age1>=78))$status_wave2,useNA = "ifany")
  
table((nutri_hcru_cov %>% 
         filter(age1<78))$status_wave3,useNA = "ifany")

table1::table1(~ .,data=nutri_hcru_cov %>% 
                 select(lopnr,bmi_cat_w1:smell_taste_imp) %>% 
                 distinct_all())

# remove individuals who declined to participate after baseline
nutri_hcru_cov_fu<-nutri_hcru_cov %>% 
  filter(fu>0) %>% 
  filter(!(age1>=78&!status_wave2 %in% c(1,11)),
         !(age1<78&!status_wave3 %in% c(1,11))) %>% 
  mutate(death_year=as.numeric(year(death_date))-as.numeric(year(date1)),
         pv_death_year=as.numeric(year(pv_death_date))-as.numeric(year(date1)),
         death_in_year=ifelse(death_year-1>fu_year|is.na(death_year),0,1),
         pv_death_in_year=ifelse(pv_death_year-1>fu_year|is.na(pv_death_year),0,1)) %>% 
  select(-c(status_wave2,status_wave3))
  
n_distinct((nutri_hcru_cov %>% 
              filter(age1<78&!status_wave3 %in% c(1,11)))$lopnr)

n_distinct((nutri_hcru_cov %>% 
              filter(age1>=78&!status_wave2 %in% c(1,11)))$lopnr)



# 7. Sanity check of the compiled data ----
summary(nutri_hcru_cov_fu)
n_distinct(nutri_hcru_cov_fu$lopnr)



# check the distribution of costs each year 
ggplot(nutri_hcru_cov_fu %>% 
         filter(!is.na(total_cost)),aes(x=as.factor(fu_year),y=total_cost))+
  geom_boxplot()+
  labs(x="Follow-up year",y="Total healthcare costs (SEK)")+
  theme_minimal()

ggplot(nutri_hcru_cov_fu %>% 
         filter(!is.na(sv_cost)),aes(x=as.factor(fu_year),y=sv_cost))+
  geom_boxplot()+
  labs(x="Follow-up year",y="Inpatient care costs (SEK)")+
  theme_minimal()

ggplot(nutri_hcru_cov_fu %>% 
         filter(!is.na(ov_cost)),aes(x=as.factor(fu_year),y=ov_cost))+
  geom_boxplot()+
  labs(x="Follow-up year",y="Outpatient care costs (SEK)")+
  theme_minimal()

ggplot(nutri_hcru_cov_fu %>% 
         filter(!is.na(pv_cost)),aes(x=as.factor(fu_year),y=pv_cost))+
  geom_boxplot()+
  labs(x="Follow-up year",y="Primary care costs (SEK)")+
  theme_minimal()


# Replacing HCRU and costs with 95th percentile values to reduce the influence of extreme values
nutri_hcru_cov_fu<-nutri_hcru_cov_fu %>% 
  group_by(fu_year) %>%
  mutate(across(c(ov_num:pv_cost),
                ~ pmin(.x, quantile(.x[.x>0], 0.95, na.rm = TRUE)),
                .names="{.col}_trim")) %>% 
  mutate(total_cost_trim=ov_cost_trim+sv_cost_trim+pv_cost_trim) %>% 
  ungroup


summary(nutri_hcru_cov_fu %>% 
          select(contains("cost")))

write_dta(nutri_hcru_cov_fu,"Organized data/snack_nutri_hcru_cov_xx.dta")
