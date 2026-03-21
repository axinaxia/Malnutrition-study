library(haven)
library(tidyverse)
library(ggplot2)
library(readxl)
library(flextable)
library(ggsignif)
library(nlme)
library(cutpointr)
library(survival)
library(survminer)
library(flexsurv)
library(data.table)
library(scales)
library(officer)
library(XML)
library(RMySQL)
library(rstudioapi)
library(lubridate)


setwd("/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data")

# call SQL database
db_host<-"h1cogbase01.nvs.ki.se"
db_port<-3306
svedem_sql<-dbConnect(MySQL(), 
                      user=askForPassword(prompt = "Username"), 
                      password=askForPassword(prompt = "Password"),
                      host=db_host, 
                      port=db_port,
                      dbname="SVEDEM_2021")

dbListTables(svedem_sql)


# outpatient and inpatient care
npr_sv<-dbGetQuery(svedem_sql, 'SELECT * from SVEDEM_2021.PAR_SV;')
npr_ov<-dbGetQuery(svedem_sql, 'SELECT * from SVEDEM_2021.PAR_OV;')


# SveDem baseline data
svedem_pv<-dbGetQuery(svedem_sql, 'SELECT * from SVEDEM_2021.GRUNDREG__2;')
svedem_sv<-dbGetQuery(svedem_sql, 'SELECT * from SVEDEM_2021.GRUNDREG__3;')


# prescribd drug data
dpr<-dbGetQuery(svedem_sql, "SELECT LopNr, EDATUM, ATC, TKOST from SVEDEM_2021.LMED
                        WHERE EDATUM >= '2012-01-01' AND EDATUM <= '2020-12-31' AND TKOST>0;")


# death register data
dor<-dbGetQuery(svedem_sql, 'SELECT * from SVEDEM_2021.DORS;')


# social care data
sol<-dbGetQuery(svedem_sql, 'SELECT * from SVEDEM_2021.SOL;') %>% 
  select(-row_names) %>% 
  distinct(LopNr,LK,BOFORM,PERIOD,KORTMAN,HTJ,HTJTIM,BOSTO,DAGV)
  
# 1. Cleaning SveDem population ----
svedem<-rbind(svedem_pv %>% 
                dplyr::select(LopNr, d_YOB, SEX, DIAGNOS, MMSESR, MMSE=MMSESR_VARDE,BOENDEFORM,
                              BOENDEFORHALLANDE,DIAGNOSDATUM,REGISTRATION_DEFINITION_KEY,
                              LANGD,VIKT), 
              svedem_sv %>% 
                dplyr::select(LopNr, d_YOB, SEX, DIAGNOS, MMSESR, MMSE=MMSESR_VARDE,BOENDEFORM,
                              BOENDEFORHALLANDE,DIAGNOSDATUM,REGISTRATION_DEFINITION_KEY,
                              LANGD,VIKT)) %>%
  mutate(across(where(is.character), ~ifelse(. %in% c(".N", " ", ".A", ".E", "INGEN_KAND"), NA, .))) %>%
  mutate(across(where(is.numeric), ~ifelse(. %in% c(".N", " ", ".A", ".E", "INGEN_KAND"), NA, .))) %>%
  mutate(across(where(is.character), ~ifelse(. == "JA", 1, .))) %>%
  mutate(across(where(is.character), ~ifelse(. == "NEJ", 0, .))) %>%
  mutate(across(where(is.character), ~na_if(., ""))) 

# keep the latest diagnoses by specialist care
svedem<-svedem %>% 
  filter(!svedem$LopNr %in% ((svedem %>% 
                                filter(duplicated(LopNr)))$LopNr)) %>% 
  rbind(svedem %>% 
          filter(svedem$LopNr %in% ((svedem %>% 
                                       filter(duplicated(LopNr)))$LopNr)) %>% 
          filter(REGISTRATION_DEFINITION_KEY=="GrundregistreringSV"))

svedem<-svedem %>% 
  filter(!svedem$LopNr %in% ((svedem %>% 
                                filter(duplicated(LopNr)))$LopNr)) %>% 
  rbind(svedem %>% 
          filter(svedem$LopNr %in% ((svedem %>% 
                                       filter(duplicated(LopNr)))$LopNr)) %>% 
          arrange(LopNr,DIAGNOSDATUM) %>% 
          group_by(LopNr) %>% 
          slice_max(DIAGNOSDATUM,n=1) %>% 
          ungroup())


diags<-c('DEMENS_UNS','VASKULAR_DEMENS_INKL_SUBKORTIKAL_VASKULAR_DEMENS',
         'BLANDDEMENS_VID_ALZHEIMERS_SJUKDOM_VASKULAR_DEMENS','DEMENS_VID_ALZHEIMERS_SJUKDOM_SEN_DEBUT',
         'DEMENS_VID_ALZHEIMERS_SJUKDOM_TIDIG_DEBUT','DEMENS_VID_PARKINSONS_SJUKDOM',
         'FRONTOTEMPORAL_DEMENS','LEWY_BODY_DEMENS','LINDRIG_KOGNITIV_STORNING','OVRIG_DEMENS')

diaggrps<-c('Unspecified','VaD','Mixed AD/VaD','Late-onset AD',
            'Early-onset AD','PDD','FTD','LBD','MCI','Other')

# restrict to dementia diagnoses between 2013-01-01 till 2020-12-31
svedem<-svedem %>% 
  mutate(diaggrp=factor(DIAGNOS, levels=diags, labels=diaggrps),
         DIAGNOSDATUM=as.Date(DIAGNOSDATUM)) %>% 
  filter(DIAGNOSDATUM<as.Date('2020-12-31')&DIAGNOSDATUM>=as.Date('2013-01-01'))

svedem<-svedem %>% 
  mutate(bmi=VIKT/((LANGD/100)^2),
         bmi_cat=as.factor(case_when(bmi<18.5~1,
                                     bmi<25~2,
                                     bmi<30~3,
                                     bmi>=30~4)),
         age=year(DIAGNOSDATUM)-d_YOB,
         # define malnutrition: BMI<22 if age>=70, BMI<20 if age<70
         malnut=as.factor(ifelse((age>=70&bmi<22)|(age<70&bmi<20),1,0)),
         sabo=ifelse(grepl("SARS",BOENDEFORM),1,0),
         sabo=ifelse(is.na(BOENDEFORM),NA,sabo),
         state_base=case_when(MMSE>=26 ~'Very mild',
                              MMSE>=21 ~'Mild',
                              MMSE>=11 ~'Moderate',
                              MMSE<11  ~'Severe'))


# define follow-up time
# follow-up time defined as death or 2020-12-31
svedem<-svedem %>% 
  left_join(dor %>% 
              select(LopNr,DODSDAT) %>% 
              mutate(DODSDAT=ifelse(as.numeric(DODSDAT) %% 100==0,
                                    as.numeric(DODSDAT)+1,as.numeric(DODSDAT)),
                     death_date=ymd(as.character(DODSDAT)))) %>% 
  filter(is.na(death_date)|(!is.na(death_date)&death_date>DIAGNOSDATUM)) %>% 
  mutate(fu_date=pmin(death_date,as.Date("2020-12-31"),na.rm = T)) %>% 
  select(-c(DODSDAT)) %>% 
  mutate(fu=as.numeric((fu_date-DIAGNOSDATUM)/365.25),
         fu_death=as.numeric((death_date-DIAGNOSDATUM)/365.25),
         death=ifelse(!is.na(death_date)&death_date<=fu_date,1,0))

n_distinct(svedem$LopNr)
length(svedem$LopNr)

# generate a dataframe indicating each follow-up year for each individual
svedem_fu_years<-svedem %>%
  distinct(LopNr,fu,DIAGNOSDATUM,fu_date) %>%               
  mutate(fu_max=ceiling(fu)) %>%     
  uncount(weights=fu_max, .id = "fu_year_end") %>%  
  mutate(fu_year_start=fu_year_end-1) %>% 
  select(LopNr,DIAGNOSDATUM,fu_date,fu_year_start,fu_year_end,fu)

n_distinct(svedem_fu_years$LopNr)
length(svedem_fu_years$LopNr)

# check mean follow-up time
mean((svedem_fu_years %>% distinct(LopNr,fu))$fu)

# 2. Organize HCRU data ----
#*********************************************************************************
# Summarise both number of visits and costs
# Avoid extreme estimates by restricting to fu-fu_year_max>=1 month
# Merge records for fu-fu_year_max<1 month into previous follow-up year
# For people with fu<1 month, change first year's HCRU to HCRU/fu
#*********************************************************************************

cpi<-read_excel("Malnutrition statistical analysis/Malnutrition-study/CPI.xlsx") %>% as.data.frame() %>% 
  mutate(year=as.numeric(year))


# get costs (weight*costs per weight) per DRG for inpatient and outpatient costs estimation
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
setkey(drg, DRG, year )
setkey(alldrgyr, DRG, year)

#create new dataset rolling forward
temp<-drg[alldrgyr, roll=T]  %>% drop_na

#use new dataset to roll backward
setkey(temp, DRG, year)
drgnew<-temp[alldrgyr, roll=-Inf]  
rm(temp)


# DRG codes in "drgnew" aren't exhaustive, impute missing costs with mvo costs when costs are missing
# generate costs per MVO (medicinska veksamhetsområde)
mvocost<-npr_sv %>%
  select(LopNr,INDATUM,DRG,MVO) %>% 
  mutate(vtyp="sv") %>% 
  rbind(npr_ov %>%
          select(LopNr,INDATUM,DRG,MVO) %>% 
          mutate(vtyp="ov")) %>% 
  rename(lopnr=LopNr) %>% 
  mutate(year=year(as.Date(INDATUM))) %>% 
  distinct(year,DRG,MVO,vtyp) %>% 
  full_join(drgnew) %>% 
  full_join(drgcost) %>% 
  mutate(mvocost=weight*drgcost) %>%
  filter(!is.na(mvocost)) %>% 
  group_by(vtyp, MVO) %>%
  summarize(mvocost=mean(mvocost, na.rm=T)) %>% 
  ungroup %>% 
  filter(!MVO %in% c("-24"," 30"," 31"," 43"," 52","") & !is.na(MVO))




# 2.1. Drug use ----
drug_hcru<-svedem_fu_years %>% 
  distinct(LopNr,DIAGNOSDATUM,fu_date,fu) %>%
  left_join(dpr) %>% 
  mutate(hcru_date=as.Date(substr(EDATUM,1,10), format='%Y-%m-%d')) %>% 
  filter(hcru_date>=DIAGNOSDATUM&hcru_date<=fu_date) %>% # remove irrelevant records
  right_join(svedem_fu_years %>% 
               select(LopNr,fu_year_start,fu_year_end),
             relationship = "many-to-many") %>% 
  filter(hcru_date>=DIAGNOSDATUM+fu_year_start*365.25&
           hcru_date<=DIAGNOSDATUM+fu_year_end*365.25) %>% 
  arrange(LopNr,fu_year_start)


# drug number
drug_num<-drug_hcru %>% 
  group_by(LopNr,fu_year_start,fu_year_end) %>% 
  summarise(drug_num=n()) %>% 
  ungroup %>% 
  right_join(svedem_fu_years %>% 
               mutate() %>% 
               select(LopNr,fu_year_start,fu_year_end,fu)) %>% 
  mutate(drug_num=ifelse(is.na(drug_num),0,drug_num)) %>% 
  group_by(LopNr,fu_year_start,fu_year_end,fu) %>% 
  summarise(drug_num=sum(drug_num)) %>% 
  ungroup


drug_num<-drug_num %>% 
  group_by(LopNr) %>% 
  mutate(fu_year_max=max(fu_year_start)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year_start),
         drug_num=ifelse(last_year==1&(fu-fu_year_max)>=(1/12)|fu<(1/12),
                       round(drug_num/(fu-fu_year_max),0),
                       drug_num),
         fu_year_start=ifelse(last_year==1&(fu-fu_year_max)<(1/12)&fu>=(1/12),
                        fu_year_start-1,
                        fu_year_start))%>% 
  group_by(LopNr,fu_year_start,fu) %>% 
  summarise(drug_num=sum(drug_num)) %>%
  ungroup %>% 
  mutate(drug_num=round(drug_num,0),
         fu_year_end=fu_year_start+1)



summary(drug_num)
n_distinct(drug_num$LopNr)
length(drug_num$LopNr)
mean(drug_num$drug_num)


# drug costs
drug_costs<-drug_hcru %>% 
  group_by(LopNr,fu_year_start,fu_year_end) %>% 
  summarise(drug_costs=sum(TKOST)) %>% 
  ungroup %>% 
  right_join(svedem_fu_years %>% 
               select(LopNr,DIAGNOSDATUM,fu_year_start,fu_year_end,fu)) %>% 
  mutate(drug_costs=ifelse(is.na(drug_costs),0,drug_costs)) %>% 
  group_by(LopNr,DIAGNOSDATUM,fu_year_start,fu_year_end,fu) %>% 
  summarise(drug_costs=sum(drug_costs)) %>% 
  ungroup %>% 
  mutate(hcru_year=as.numeric(year(DIAGNOSDATUM))+fu_year_start) %>% 
  select(-DIAGNOSDATUM)

# convert to 2024 price
drug_costs<-drug_costs %>% 
  left_join(cpi, by=c('hcru_year'='year')) %>% 
  mutate(drug_costs_c=drug_costs*(((cpi %>% filter(year==2024))$CPI)/CPI)) %>% 
  select(-CPI)


drug_costs<-drug_costs %>% 
  group_by(LopNr) %>% 
  mutate(fu_year_max=max(fu_year_start)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year_start),
         drug_costs=ifelse(last_year==1&(fu-fu_year_max)>=(1/12)|fu<(1/12),
                         round(drug_costs/(fu-fu_year_max),0),
                         drug_costs),
         fu_year_start=ifelse(last_year==1&(fu-fu_year_max)<(1/12)&fu>=(1/12),
                              fu_year_start-1,
                              fu_year_start))%>% 
  group_by(LopNr,fu_year_start,fu) %>% 
  summarise(drug_costs=sum(drug_costs)) %>%
  ungroup %>% 
  mutate(drug_costs=round(drug_costs,0),
         fu_year_end=fu_year_start+1)

summary(drug_costs)
n_distinct(drug_costs$LopNr)
length(drug_costs$LopNr)
mean(drug_costs$drug_costs)


drug_costs %>% 
  summarise(sum_cost=sum(drug_costs),
            sum_fu=sum(fu_year_end-fu_year_start)) %>% 
  mutate(cost=sum_cost/sum_fu)




# 2.2. Inpatient care ----
sv_hcru<-svedem_fu_years %>% 
  distinct(LopNr,DIAGNOSDATUM,fu_date,fu) %>%
  left_join(npr_sv %>% 
              select(LopNr,MVO,DRG,INDATUMA)) %>% 
  mutate(hcru_date=ymd(INDATUMA),
         hcru_year=year(hcru_date)) %>% 
  filter(hcru_date>=DIAGNOSDATUM&hcru_date<=fu_date) %>% # remove irrelevant records
  right_join(svedem_fu_years %>% 
               select(LopNr,fu_year_start,fu_year_end,fu),
             relationship = "many-to-many") %>% 
  filter(hcru_date>=DIAGNOSDATUM+fu_year_start*365.25&
           hcru_date<=DIAGNOSDATUM+fu_year_end*365.25)


# number of inpatient visits
sv_num<-sv_hcru %>% 
  group_by(LopNr,hcru_year,fu_year_start,fu_year_end) %>% 
  summarise(sv_num=n()) %>% 
  ungroup %>% 
  right_join(svedem_fu_years %>% 
               select(LopNr,fu_year_start,fu_year_end,fu)) %>% 
  mutate(sv_num=ifelse(is.na(sv_num),0,sv_num)) %>% 
  group_by(LopNr,fu_year_start,fu_year_end,fu) %>% 
  summarise(sv_num=sum(sv_num)) %>% 
  ungroup

n_distinct(sv_num$LopNr)
length(sv_num$LopNr)
mean(sv_num$sv_num)


sv_num<-sv_num %>% 
  group_by(LopNr) %>% 
  mutate(fu_year_max=max(fu_year_start)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year_start),
         sv_num=ifelse(last_year==1&(fu-fu_year_max)>=(1/12)|fu<(1/12),
                         round(sv_num/(fu-fu_year_max),0),
                       sv_num),
         fu_year_start=ifelse(last_year==1&(fu-fu_year_max)<(1/12)&fu>=(1/12),
                              fu_year_start-1,
                              fu_year_start))%>% 
  group_by(LopNr,fu_year_start,fu) %>% 
  summarise(sv_num=sum(sv_num)) %>%
  ungroup %>% 
  mutate(sv_num=round(sv_num,0),
         fu_year_end=fu_year_start+1)


n_distinct(sv_num$LopNr)
length(sv_num$LopNr)
mean(sv_num$sv_num)


# inpatient costs
sv_costs<-sv_hcru %>% 
  left_join(drgnew %>% rename(hcru_year=year)) %>% 
  left_join(drgcost %>% rename(hcru_year=year)) %>%
  mutate(cost=weight*drgcost) %>% 
  left_join(mvocost %>% 
              filter(vtyp=="sv")) %>% 
  mutate(is.na(cost),mvocost) %>% 
  group_by(LopNr,hcru_year,fu_year_start,fu_year_end) %>% 
  summarise(sv_costs=sum(cost)) %>% 
  ungroup %>% 
  right_join(svedem_fu_years %>% 
               select(LopNr,fu_year_start,fu_year_end,fu)) %>% 
  mutate(sv_costs=ifelse(is.na(sv_costs),0,sv_costs)) %>% 
  group_by(LopNr,fu_year_start,fu_year_end,fu) %>% 
  summarise(sv_costs=sum(sv_costs)) %>% 
  ungroup

n_distinct(sv_costs$LopNr)
length(sv_costs$LopNr)
mean(sv_costs$sv_costs)

sv_costs %>% 
  summarise(sum_cost=sum(sv_costs),
            sum_fu=sum(fu_year_end-fu_year_start)) %>% 
  mutate(cost=sum_cost/sum_fu)



sv_costs<-sv_costs %>% 
  group_by(LopNr) %>% 
  mutate(fu_year_max=max(fu_year_start)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year_start),
         sv_costs=ifelse(last_year==1&(fu-fu_year_max)>=(1/12)|fu<(1/12),
                           round(sv_costs/(fu-fu_year_max),0),
                         sv_costs),
         fu_year_start=ifelse(last_year==1&(fu-fu_year_max)<(1/12)&fu>=(1/12),
                              fu_year_start-1,
                              fu_year_start))%>% 
  group_by(LopNr,fu_year_start,fu) %>% 
  summarise(sv_costs=sum(sv_costs)) %>%
  ungroup %>% 
  mutate(sv_costs=round(sv_costs,0),
         fu_year_end=fu_year_start+1)


n_distinct(sv_costs$LopNr)
length(sv_costs$LopNr)
mean(sv_costs$sv_costs)



# 2.3. Outpatient care ----
ov_hcru<-svedem_fu_years %>% 
  distinct(LopNr,DIAGNOSDATUM,fu_date,fu) %>%
  left_join(npr_ov %>% 
              select(LopNr,MVO,DRG,INDATUMA)) %>% 
  mutate(hcru_date=ymd(INDATUMA),
         hcru_year=year(hcru_date)) %>% 
  filter(hcru_date>=DIAGNOSDATUM&hcru_date<=fu_date) %>% # remove irrelevant records
  right_join(svedem_fu_years %>% 
               select(LopNr,fu_year_start,fu_year_end),
             relationship = "many-to-many") %>% 
  filter(hcru_date>=DIAGNOSDATUM+fu_year_start*365.25&
           hcru_date<=DIAGNOSDATUM+fu_year_end*365.25)


# number of outpatient visits
ov_num<-ov_hcru %>% 
  group_by(LopNr,hcru_year,fu_year_start,fu_year_end) %>% 
  summarise(ov_num=n()) %>% 
  ungroup %>% 
  right_join(svedem_fu_years %>% 
               select(LopNr,fu_year_start,fu_year_end,fu)) %>% 
  mutate(ov_num=ifelse(is.na(ov_num),0,ov_num)) %>% 
  group_by(LopNr,fu_year_start,fu_year_end,fu) %>% 
  summarise(ov_num=sum(ov_num)) %>% 
  ungroup

n_distinct(ov_num$LopNr)
length(ov_num$LopNr)
mean(ov_num$ov_num)

ov_num<-ov_num %>% 
  group_by(LopNr) %>% 
  mutate(fu_year_max=max(fu_year_start)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year_start),
         ov_num=ifelse(last_year==1&(fu-fu_year_max)>=(1/12)|fu<(1/12),
                        round(ov_num/(fu-fu_year_max),0),
                       ov_num),
         fu_year_start=ifelse(last_year==1&(fu-fu_year_max)<(1/12)&fu>=(1/12),
                              fu_year_start-1,
                              fu_year_start))%>% 
  group_by(LopNr,fu_year_start,fu) %>% 
  summarise(ov_num=sum(ov_num)) %>%
  ungroup %>% 
  mutate(ov_num=round(ov_num,0),
         fu_year_end=fu_year_start+1)


n_distinct(ov_num$LopNr)
length(ov_num$LopNr)
mean(ov_num$ov_num)


# outpatient costs
ov_costs<-ov_hcru %>% 
  left_join(drgnew %>% rename(hcru_year=year)) %>% 
  left_join(drgcost %>% rename(hcru_year=year)) %>%
  mutate(cost=weight*drgcost) %>% 
  left_join(mvocost %>% 
              filter(vtyp=="ov")) %>% 
  mutate(is.na(cost),mvocost) %>% 
  group_by(LopNr,hcru_year,fu_year_start,fu_year_end) %>% 
  summarise(ov_costs=sum(cost)) %>% 
  ungroup %>% 
  right_join(svedem_fu_years %>% 
               select(LopNr,fu_year_start,fu_year_end,fu)) %>% 
  mutate(ov_costs=ifelse(is.na(ov_costs),0,ov_costs)) %>% 
  group_by(LopNr,fu_year_start,fu_year_end,fu) %>% 
  summarise(ov_costs=sum(ov_costs)) %>% 
  ungroup

n_distinct(ov_costs$LopNr)
length(ov_costs$LopNr)
mean(ov_costs$ov_costs)

ov_costs %>% 
  summarise(sum_cost=sum(ov_costs),
            sum_fu=sum(fu_year_end-fu_year_start)) %>% 
  mutate(cost=sum_cost/sum_fu)


ov_costs<-ov_costs %>% 
  group_by(LopNr) %>% 
  mutate(fu_year_max=max(fu_year_start)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year_start),
         ov_costs=ifelse(last_year==1&(fu-fu_year_max)>=(1/12)|fu<(1/12),
                        round(ov_costs/(fu-fu_year_max),0),
                       ov_costs),
         fu_year_start=ifelse(last_year==1&(fu-fu_year_max)<(1/12)&fu>=(1/12),
                              fu_year_start-1,
                              fu_year_start))%>% 
  group_by(LopNr,fu_year_start,fu) %>% 
  summarise(ov_costs=sum(ov_costs)) %>%
  ungroup %>% 
  mutate(ov_costs=round(ov_costs,0),
         fu_year_end=fu_year_start+1)

n_distinct(ov_costs$LopNr)
length(ov_costs$LopNr)
mean(ov_costs$ov_costs)



# 2.4. Social care ----
# ***************************************************************************************************
# Comments from Socialstyrelsen (2025-12-02): 
# 1. If the records show that a person is living in residential care during the month, 
#    they are considered not to have received any home care services in the official statistics.
# 2. If a person receives social services from two municipalities in the same month, 
#    this may be because they moved during that month or because they are temporarily living 
#    in another location during that period, for example over the summer.
# ***************************************************************************************************


sol_hcru<-svedem_fu_years %>% 
  distinct(LopNr,DIAGNOSDATUM,fu_date,fu) %>% 
  left_join(sol) %>% 
  mutate(hcru_date=as.Date(paste0(PERIOD,'01'), format='%Y%m%d')) %>% 
  filter(hcru_date>=DIAGNOSDATUM&hcru_date<=fu_date) %>% # remove irrelevant records
  right_join(svedem_fu_years %>% 
               select(LopNr,fu_year_start,fu_year_end,fu),
             relationship = "many-to-many") %>% 
  filter(hcru_date>=DIAGNOSDATUM %m+% years(fu_year_start)&
           hcru_date<DIAGNOSDATUM %m+% years(fu_year_end)) %>% 
  group_by(LopNr,hcru_date) %>% 
  slice_min(fu_year_start) %>% 
  ungroup


n_distinct(sol_hcru$LopNr)
length(sol_hcru$LopNr)


# check duplicated records in SOL
sol_hcru %>% 
  add_count(LopNr,PERIOD) %>%    
  filter(n>1) %>% 
  View()

table(sol_hcru$KORTMAN)
table(sol_hcru$HTJTIM)
table(sol_hcru$BOSTO)
table(sol_hcru$DAGV)
table(sol_hcru$BOFORM)



sol_num_panel<-sol_hcru %>% 
  mutate(# number of days in institutionalization 
    inst_days=as.numeric(BOFORM==2)*31,
    
    # home help hours
    hhtim=case_when(HTJ==1 & HTJTIM<777 ~ HTJTIM,
                    as.numeric(BOFORM==2)==1 & HTJ==1 & HTJTIM==777 ~ 0,
                    as.numeric(BOFORM==2)==0 & HTJ==1 & HTJTIM==777 ~ 24*31,
                    T ~ 0),
    
    # days in short-term residence ??? (not knowing starting and ending dates)
    shortterm_days=ifelse(KORTMAN %in% c('88', '99',' .','-1', '-2', '-3', '-7', '-9', '0', '00', ''), 0,as.numeric(KORTMAN)),
    shortterm_days=ifelse(shortterm_days>31,31,shortterm_days),
    
    # number of days of receiving housing support
    support=ifelse(BOSTO=="1",1,0)*31,
    
    #number of days of using social day activity 
    dagv=ifelse(DAGV=='1',1,0)*31) %>% 
  mutate(across(hhtim:dagv, ~ ifelse(BOFORM==2, 0, .x))) %>% 
  distinct(LopNr,PERIOD,hcru_date,inst_days,hhtim,shortterm_days,support,dagv,
           fu_year_start,fu_year_end,fu)


# check duplicated records per period in SOL
sol_num_panel %>% 
  add_count(LopNr,PERIOD) %>%    
  filter(n>1) %>% 
  View()


# split sol data into duplicated records and distinct records per period
sol_num_panel_dist<-sol_num_panel %>% 
  add_count(LopNr,PERIOD) %>%    
  filter(n==1)

sol_num_panel_dup<-sol_num_panel %>% 
  add_count(LopNr,PERIOD) %>%    
  filter(n>1)

# if one record indicate institutionalized, and another not in the same month, keep institutionalization
sol_num_panel_dup<-sol_num_panel_dup %>% 
  group_by(LopNr,PERIOD) %>% 
  mutate(inst_flag=max(inst_days)) %>% 
  ungroup %>% 
  filter(!(inst_flag==31 & inst_days==0)) %>% 
  select(-inst_flag)


# if short term residence >=31 days, set other social care as 0
# if short term residence <31 days, set the upper limit of other social care as the rest of days/hours in that month
sol_num_panel_dup<-sol_num_panel_dup %>% 
  mutate(across(c(hhtim,support,dagv), ~ ifelse(shortterm_days>=31, 0, .x)),
         across(c(support,dagv), ~ ifelse(shortterm_days<31&
                                            .x>31-shortterm_days, 31-shortterm_days, .x)),
         hhtim=ifelse(shortterm_days<31&
                        hhtim>24*(31-shortterm_days), 24*(31-shortterm_days), hhtim))


# sum up the social care use for duplicated records per period
sol_num_panel_dup<-sol_num_panel_dup %>% 
  group_by(LopNr,hcru_date,fu_year_start,fu_year_end,fu) %>% 
  summarise(inst_days=sum(inst_days),
            hhtim=sum(hhtim),
            shortterm_days=sum(shortterm_days),
            support=sum(support),
            dagv=sum(dagv)) %>% 
  ungroup %>% 
  mutate(across(hhtim:dagv, ~ ifelse(inst_days==31, 0, .x)),
         hhtim=ifelse(hhtim>24*31,24*31,hhtim),
         shortterm_days=ifelse(shortterm_days>31,31,shortterm_days),
         support=ifelse(support>31,31,support),
         dagv=ifelse(dagv>31,31,dagv))


sol_num_panel_dedup<-bind_rows(sol_num_panel_dist,
                               sol_num_panel_dup) %>% 
  arrange(LopNr,hcru_date,fu_year_start,fu_year_end)

# check duplicated records per period again
sol_num_panel_dedup %>% 
  add_count(LopNr,PERIOD) %>%    
  filter(n>1) %>% 
  View()
  
n_distinct(sol_num_panel_dedup$LopNr)


sol_num<-sol_num_panel_dedup %>% 
  right_join(svedem_fu_years %>% 
               select(LopNr,fu_year_start,fu_year_end,fu)) %>% 
  mutate(across(inst_days:dagv,~ ifelse(is.na(.x),0,.x))) %>% 
  group_by(LopNr,fu_year_start,fu_year_end,fu) %>% 
  summarise(across(inst_days:dagv,sum)) %>% 
  ungroup 



sol_num<-sol_num %>% 
  group_by(LopNr) %>% 
  mutate(fu_year_max=max(fu_year_start)) %>% 
  ungroup %>% 
  mutate(last_year=as.numeric(fu_year_max==fu_year_start),
         fu_year_start=ifelse(last_year==1&(fu-fu_year_max)<(1/12)&fu>=(1/12),
                              fu_year_start-1,
                              fu_year_start)) %>% 
  mutate(across(inst_days:dagv, ~ ifelse(last_year==1&(fu-fu_year_max)>=(1/12)|fu<(1/12),
                                         round(.x/(fu-fu_year_max),0),
                                         .x))) %>% 
  group_by(LopNr,fu_year_start,fu) %>% 
  summarise(across(inst_days:dagv,sum)) %>%
  ungroup %>% 
  mutate(across(inst_days:dagv, ~ round(.x,0)),
         fu_year_end=fu_year_start+1)


sol_num<-sol_num %>% 
  mutate(across(c(inst_days,shortterm_days:dagv), ~ ifelse(.x>365.25,365.25, .x)),
         hhtim=ifelse(hhtim>24*365.25,24*365.25,hhtim))
  
n_distinct(sol_num$LopNr)

sol_costs<-sol_num %>%
  mutate(cost_homecare=hhtim*646, # from Emil's request to Ensolution (check email from 2026-03-04)
         cost_daytimeactivity=dagv*148400/365.25, 
         cost_shortterm=shortterm_days*3387, # from Emil's request to Ensolution (check email from 2026-03-04)
         cost_housingsupport=support*269, # from Xin's request to Ensolution (check email from 2026-03-09) 
         cost_inst=inst_days*2566, # from Emil's request to Ensolution (check email from 2026-03-04)
         cost_sol=cost_homecare+cost_daytimeactivity+cost_shortterm+
           cost_housingsupport+cost_inst)

  
sol_costs %>% 
  summarise(sum_cost=sum(cost_homecare),
            sum_fu=sum(fu_year_end-fu_year_start)) %>% 
  mutate(cost=sum_cost/sum_fu)

sol_costs %>% 
  summarise(sum_cost=sum(cost_daytimeactivity),
            sum_fu=sum(fu_year_end-fu_year_start)) %>% 
  mutate(cost=sum_cost/sum_fu)

sol_costs %>% 
  summarise(sum_cost=sum(cost_shortterm),
            sum_fu=sum(fu_year_end-fu_year_start)) %>% 
  mutate(cost=sum_cost/sum_fu)

sol_costs %>% 
  summarise(sum_cost=sum(cost_housingsupport),
            sum_fu=sum(fu_year_end-fu_year_start)) %>% 
  mutate(cost=sum_cost/sum_fu)

sol_costs %>% 
  summarise(sum_cost=sum(cost_inst),
            sum_fu=sum(fu_year_end-fu_year_start)) %>% 
  mutate(cost=sum_cost/sum_fu)

sol_costs %>% 
  summarise(sum_cost=sum(cost_sol),
            sum_fu=sum(fu_year_end-fu_year_start)) %>% 
  mutate(cost=sum_cost/sum_fu)



# 2.5. Combine all costs and hcru ----
all_costs<-drug_costs %>% 
  left_join(ov_costs) %>% 
  left_join(sv_costs) %>% 
  left_join(sol_costs %>% 
              select(LopNr,fu_year_start,fu_year_end,fu,contains("cost"))) %>% 
  select(LopNr,fu_year_start,fu_year_end,fu,everything())

summary(all_costs)

  
all_costs<-all_costs %>% 
  mutate(across(drug_costs:cost_sol, ~ ifelse(is.na(.x), 0, .x)),
         cost_total=drug_costs+ov_costs+sv_costs+cost_sol)



all_hcru<-drug_num %>% 
  left_join(ov_num) %>% 
  left_join(sv_num) %>% 
  left_join(sol_num) %>% 
  select(LopNr,fu_year_start,fu_year_end,fu,everything())

summary(all_hcru)


all_hcru<-all_hcru %>% 
  mutate(across(drug_num:dagv, ~ ifelse(is.na(.x), 0, .x)))






# 3. Organize covariate data ----
# *************************************************************************************
# Covariates to include:
# Living arrangement, myocardial infarction, heart failure, stroke, 
# Parkinson’s disease, chronic obstructive pulmonary disease, 
# rheumatoid arthritis or osteoporosis, cancer, depression
# *************************************************************************************
icd_codes<-read_excel("Malnutrition statistical analysis/Malnutrition-study/ICD and ATC codes for malnutrition study_svedem.xlsx") %>% 
  as.data.frame() %>% 
  mutate(icd_vector=lapply(icd, function(x) str_trim(str_split(x, ",")[[1]])) )


icd_codes<-icd_codes %>%
  as.data.frame() %>%
  mutate(icd_vector = ifelse(
    icd == "C00–C97", 
    list(paste0("C", sprintf("%02d", 0:97))),  
    lapply(icd, function(x) str_trim(str_split(x, ",")[[1]]))  
  ))


# identify comorbidities from NPR records
# restrict to records within 5 years prior to AD diagnosis
comorb_npr<-npr_sv %>% 
  select(LopNr,INDATUMA,DIAGNOS) %>% 
  rbind(npr_ov %>% 
          select(LopNr,INDATUMA,DIAGNOS)) %>% 
  right_join(svedem_fu_years %>% 
               distinct(LopNr,DIAGNOSDATUM), by="LopNr") %>%
  mutate(INDATUMA=ymd(INDATUMA)) %>% 
  filter(INDATUMA<=DIAGNOSDATUM&INDATUMA>=as.Date(DIAGNOSDATUM-365.25*5)) %>%
  separate_wider_delim(DIAGNOS, delim=' ', names_sep='_', too_few='align_start') %>% 
  pivot_longer(cols=starts_with('DIAGNOS_'), names_to='diagnr', values_to='diag', values_drop_na =T)  %>%
  mutate(diag4=substr(diag,1,4),
         diag3=substr(diag,1,3))


for (i in seq_along(icd_codes$disease)) {
  disease_name<-icd_codes$disease[i]       
  disease_codes<-icd_codes$icd_vector[[i]]  
  
  comorb_npr[[disease_name]]<-0
  
  comorb_npr[[disease_name]]<-ifelse(comorb_npr$diag3 %in% disease_codes | 
                                    comorb_npr$diag4 %in% disease_codes, 1, 0)
}


summary(comorb_npr)
colnames(comorb_npr)

table((comorb_npr %>% filter(mi==1))$diag)
table((comorb_npr %>% filter(hf==1))$diag)
table((comorb_npr %>% filter(stroke==1))$diag)
table((comorb_npr %>% filter(pd==1))$diag)
table((comorb_npr %>% filter(copd==1))$diag)
table((comorb_npr %>% filter(ra==1))$diag)
table((comorb_npr %>% filter(op==1))$diag)
table((comorb_npr %>% filter(cancer==1))$diag)
table((comorb_npr %>% filter(depression==1))$diag)



# identify comorbidities from use of relevant drugs
# restrict to records within 3 months prior to AD diagnosis
comorb_atc_codes<-read_excel("Malnutrition statistical analysis/Malnutrition-study/ICD and ATC codes for malnutrition study_svedem.xlsx",sheet = "disease") %>% 
  as.data.frame() %>% 
  filter(!is.na(atc)) %>%
  separate_wider_delim(atc, delim=',', names_sep='', too_few='align_start')


comorb_dpr<-dpr %>% 
  right_join(svedem_fu_years %>% 
               distinct(LopNr,DIAGNOSDATUM), by="LopNr") %>%
  mutate(EDATUM=ymd(substr(EDATUM,1,10))) %>% 
  filter(EDATUM<=DIAGNOSDATUM&EDATUM>=as.Date(DIAGNOSDATUM-31*3))


for (i in 1:length(comorb_atc_codes$disease)) {
  disease_name<-comorb_atc_codes$disease[i]
  
  atc_codes<-comorb_atc_codes %>%
    filter(disease == disease_name) %>%
    dplyr::select(atc1:atc6) %>%
    unlist() 
  
  atc_pattern<-paste0("^",atc_codes,collapse = "|")
  
  comorb_dpr[[paste(disease_name,"dpr",sep = "_")]]<-0
  comorb_dpr[[paste(disease_name,"dpr",sep = "_")]]<-ifelse(grepl(atc_pattern, comorb_dpr$ATC), 1, 0)
}



table((comorb_dpr %>% filter(mi_dpr==1))$ATC)
table((comorb_dpr %>% filter(hf_dpr==1))$ATC)
table((comorb_dpr %>% filter(pd_dpr==1))$ATC)
table((comorb_dpr %>% filter(cancer_dpr==1))$ATC)
table((comorb_dpr %>% filter(depression_dpr==1))$ATC)



summary(comorb_dpr)
colnames(comorb_dpr)



# combine comorbidities identified from NPR and DPR
comorb_npr_dpr<-comorb_npr %>% 
  group_by(LopNr) %>%
  summarise(across(mi:depression, max)) %>% 
  ungroup %>% 
  full_join(comorb_dpr %>% 
              group_by(LopNr) %>%
              summarise(across(mi_dpr:depression_dpr, max)) %>% 
              ungroup) %>% 
  mutate(mi=if_else(mi_dpr==1,1,mi,missing=mi),
         hf=if_else(hf_dpr==1,1,hf,missing=hf),
         pd=if_else(pd_dpr==1,1,pd,missing=pd),
         cancer=if_else(cancer_dpr==1,1,cancer,missing=cancer),
         depression=if_else(depression_dpr==1,1,depression,missing=depression))

summary(comorb_npr_dpr)
n_distinct(comorb_npr_dpr$LopNr)



comorb_npr_dpr<-comorb_npr_dpr %>% 
  select(-contains("_dpr")) %>% 
  right_join(svedem_fu_years %>% 
               distinct(LopNr)) %>% 
  group_by(LopNr) %>%
  mutate(across(mi:depression,~factor(replace_na(.x,0)))) %>% 
  mutate(any_comorb=factor(if_any(c(mi:depression),~.x==1)*1,levels = 0:1)) %>% 
  ungroup

summary(comorb_npr_dpr)




# 4. Combine all data ----
bmi_hcru_cov<-svedem %>% 
  select(LopNr,MMSE,BOENDEFORHALLANDE,BOENDEFORM,DIAGNOSDATUM,REGISTRATION_DEFINITION_KEY,
         diaggrp,bmi,bmi_cat,malnut,sabo,
         age,SEX,state_base,fu_death,fu,death) %>% 
  right_join(all_hcru) %>% 
  right_join(all_costs) %>%
  right_join(comorb_npr_dpr) %>% 
  mutate(death_in_year=ifelse(floor(fu_death)>fu_year_start|is.na(fu_death),0,1)) 

n_distinct(bmi_hcru_cov$LopNr)


# 5. Sanity check ----
bmi_hcru_cov %>%
  left_join(svedem %>% select(LopNr,state_base,diaggrp)) %>% 
  filter(fu_year_start==0,
         diaggrp %in% c("Late-onset AD","Early-onset AD")) %>% 
  group_by(state_base) %>% 
  summarise(mean_cost=mean(cost_total)) %>% 
  ungroup


ggplot(bmi_hcru_cov %>% 
         filter(!is.na(cost_total)),aes(x=as.factor(fu_year_start),y=cost_total))+
  geom_boxplot()+
  labs(x="Follow-up year",y="Total healthcare costs (SEK)")+
  theme_minimal()

ggplot(bmi_hcru_cov %>% 
         filter(!is.na(drug_costs)),aes(x=as.factor(fu_year_start),y=drug_costs))+
  geom_boxplot()+
  labs(x="Follow-up year",y="Drug costs (SEK)")+
  theme_minimal()

ggplot(bmi_hcru_cov %>% 
         filter(!is.na(ov_costs)),aes(x=as.factor(fu_year_start),y=ov_costs))+
  geom_boxplot()+
  labs(x="Follow-up year",y="Outpatient care costs (SEK)")+
  theme_minimal()

ggplot(bmi_hcru_cov %>% 
         filter(!is.na(sv_costs)),aes(x=as.factor(fu_year_start),y=sv_costs))+
  geom_boxplot()+
  labs(x="Follow-up year",y="Inpatient care costs (SEK)")+
  theme_minimal()

ggplot(bmi_hcru_cov %>% 
         filter(!is.na(cost_sol)),aes(x=as.factor(fu_year_start),y=cost_sol))+
  geom_boxplot()+
  labs(x="Follow-up year",y="Social care costs (SEK)")+
  theme_minimal()


# Replacing HCRU and costs with 95th percentile values to reduce the influence of extreme values
bmi_hcru_cov<-bmi_hcru_cov %>% 
  group_by(fu_year_start) %>% 
  mutate(across(c(drug_num:cost_total),
                ~ pmin(.x, quantile(.x[.x>0], 0.95, na.rm = TRUE)),
                .names="{.col}_trim")) %>% 
  ungroup %>%
  rename(sol_costs_trim=cost_sol_trim,
         total_costs_trim=cost_total_trim)


summary(bmi_hcru_cov %>% 
          select(contains("cost")))

hist((bmi_hcru_cov %>% filter(total_costs_trim>0))$total_costs_trim)
hist((bmi_hcru_cov %>% filter(drug_costs_trim>0))$drug_costs_trim)
hist((bmi_hcru_cov %>% filter(ov_costs_trim>0))$ov_costs_trim)
hist((bmi_hcru_cov %>% filter(sv_costs_trim>0))$sv_costs_trim)
hist((bmi_hcru_cov %>% filter(sol_costs_trim>0))$sol_costs_trim)

bmi_hcru_cov<-bmi_hcru_cov %>% 
  mutate(age_group=factor(case_when(age<70 ~ 1,
                                    age>=70 & age<80 ~ 2,
                                    age>=80 ~ 3),
                          levels=1:3,
                          labels=c("<70","70-79","80+")),
         bmi_cat=factor(bmi_cat, levels = 1:4,labels =c("Underweight","Normal weight",
                                         "Overweight","Obesity")),
         sabo=factor(sabo, levels=0:1, labels=c("Non-institutionalized","Institutionalized")))
  


write_dta(bmi_hcru_cov,"Organized data/svedem_nutri_hcru_cov_xx.dta")



# 6. Description of the study population ----
table1::table1(~.|age_group,data=bmi_hcru_cov %>% 
                 filter(fu_year_start==0) %>% 
                 select(age,SEX,malnut,diaggrp,BOENDEFORHALLANDE,sabo,mi:any_comorb,age_group))





