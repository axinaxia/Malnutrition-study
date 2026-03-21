library(tidyverse)
library(haven)
library(heemod)
library(mice)
library(readxl)



# 1. Number of people with malnutrition and costs ----
setwd("/Users/xin.xia/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/")

data_burden<-read_excel("Malnutrition statistical analysis/Malnutrition-study/data for estimating burden of malnutrition.xlsx",
                        sheet = "summary",col_names = FALSE) %>% 
  t() %>%
  as.data.frame() %>% 
  select(-c(V3,V9:V11)) %>% 
  setNames(c("age","pop","prop_geri","prev_mna2_pop",
             "prev_mna3_pop","prev_mna2_geri","prev_mna3_geri")) %>% 
  slice(-1) %>% 
  mutate(across(pop:prev_mna3_geri,as.numeric))


# estimate the number of people at risk of malnutrition or being malnourished
data_burden<-data_burden %>% 
  mutate(num_mna2=pop*(1-prop_geri)*prev_mna2_pop+pop*prop_geri*prev_mna2_geri,
         num_mna3=pop*(1-prop_geri)*prev_mna3_pop+pop*prop_geri*prev_mna3_geri,
         cost_mna2=pop*(1-prop_geri)*prev_mna2_pop*2348+pop*prop_geri*prev_mna2_geri*115292,
         cost_mna3=pop*(1-prop_geri)*prev_mna3_pop*2674+pop*prop_geri*prev_mna3_geri*164389)
  

data_burden %>% 
  summarise(pop=sum(pop),
            num_mna2=sum(num_mna2),
            num_mna3=sum(num_mna3),
            cost_mna2=sum(cost_mna2),
            cost_mna3=sum(cost_mna3))



# 2. Calculate annual transition probability ----
# four-state model: 1=no malnutrition, 2=at risk of malnutrition, 3=malnourished, and 4=death
# backward transitions are allowed

mna_snack<-read_dta("Malnutrition and HCRU SNACK.dta") %>% 
  as.data.frame() %>% 
  select(lopnr,age1,sex,contains("mna"))


attr_snack<-read_dta("Malnutrition and HCRU SNACK extra variables.dta") %>% 
  as.data.frame() %>% 
  select(lopnr,date1:date3,status_wave1:status_wave3,DODSDAT)

death<-read_sav("SNAC-K c1 c2 death date from coordinator.sav") %>% 
  as.data.frame() %>% 
  mutate(dödår_f7=dödår_f7+2000,
         # assume an approximate death date as the first day of the week reported
         death_date_appro=as.Date(paste(dödår_f7,dödvecka_f7, 1, sep = "-"), format = "%Y-%W-%u"),
         dödsdatum_f7=as.Date(paste0("20", dödsdatum_f7), format = "%Y%m%d"),
         death_date_nurse=as.Date(ifelse(is.na(dödsdatum_f7),death_date_appro,dödsdatum_f7))) %>% 
  rename(lopnr=Löpnr)

# 1=participated, 2=refused, 4=relative refused,
# 8=no contact, 9=moved, 11=died, 12=cancelled testing

mna_snack<-mna_snack %>% 
  left_join(attr_snack,by="lopnr") %>% 
  left_join(death %>% select(lopnr,death_date_nurse)) %>% 
  mutate(DODSDAT=ymd(DODSDAT),
         # impute missing death dates with the dates collected by nurses
         DODSDAT=as.Date(ifelse(is.na(DODSDAT),death_date_nurse,DODSDAT)))


mna_snack_state<-mna_snack %>% 
  # remove dropouts
  filter(!(age1<78&status_wave3 %in% c(2,4,8,9,12)),
         !(age1>=78&status_wave2 %in% c(2,4,8,9,12))) %>% 
  mutate(fu=case_when(age1<78&status_wave3!=11~as.numeric(date3-date1)/365.25,
                      age1<78&status_wave3==11~as.numeric(DODSDAT-date1)/365.25,
                      age1>=78&status_wave2!=11~as.numeric(date2-date1)/365.25,
                      age1>=78&status_wave2==11~as.numeric(DODSDAT-date1)/365.25),
         death=ifelse(age1<78&status_wave3==11|age1>=78&status_wave2==11,1,0),
         mna_state_w1=as.factor(mna_screening1_cat+1),
         mna_state_w2=as.factor(case_when(age1<78&status_wave3!=11~mna_screening3_cat+1,
                                age1<78&status_wave3==11~4,
                                age1>=78&status_wave2!=11~mna_screening2_cat+1,
                                age1>=78&status_wave2==11~4)),
         age2=age1+fu) %>% 
  select(lopnr,mna_state_w1,mna_state_w2,
         age1,age2,sex,fu)


# impute missingness in mna_state_w1 and mna_state_w2
imp_w1<-mice(mna_snack_state %>% 
            select(mna_state_w1,age1,sex,fu),maxit=0,seed=2026)
predM_w1<-imp_w1$predictorMatrix
predM_w1[,c("fu")]<-0
meth_w1<-imp_w1$method
meth_w1[c("mna_state_w1")]<-"polr"

imp_w1<-mice(mna_snack_state %>% 
            select(mna_state_w1,age1,sex,fu),
          predictorMatrix = predM_w1,method = meth_w1,
          m=20,maxit=20,seed=2025)



imp_w2<-mice(mna_snack_state %>% 
               select(mna_state_w2,age2,sex,fu),maxit=0,seed=2026)
predM_w2<-imp_w2$predictorMatrix
predM_w2[,c("fu")]<-0
meth_w2<-imp_w2$method
meth_w2[c("mna_state_w2")]<-"polr"

imp_w2<-mice(mna_snack_state %>% 
               select(mna_state_w2,age2,sex,fu),
             predictorMatrix = predM_w2,method = meth_w2,
             m=20,maxit=20,seed=2025)


# extract completed datasets
w1_list<-complete(imp_w1, action = "all")
w2_list<-complete(imp_w2, action = "all")

# merge each imputed dataset pair
merged_list<-Map(
  function(x, y) {
    bind_cols(x, y %>% select(mna_state_w2, age2))
  },
  w1_list, w2_list
)



# calculate transition probability
tp_imp<-lapply(seq_along(merged_list), function(i){
  dat<-merged_list[[i]]
  tp_table_temp<-table((dat %>% 
                     filter(age1<78))$mna_state_w1,
                  (dat %>% 
                     filter(age1<78))$mna_state_w2) %>% 
    as.data.frame() %>% 
    mutate(age_group=0) %>% 
    rbind(table((dat %>% 
                   filter(age1>=78))$mna_state_w1,
                (dat %>% 
                   filter(age1>=78))$mna_state_w2) %>% 
            as.data.frame() %>% 
            mutate(age_group=1)) %>% 
    set_names(c("mna_state_w1","mna_state_w2","n","age_group")) %>% 
    left_join(dat %>% 
                mutate(age_group=ifelse(age1<78,0,1)) %>% 
                group_by(age_group,
                         mna_state_w1) %>% 
                summarise(fu=sum(fu)) %>% 
                as.data.frame()) %>% 
    arrange(mna_state_w1,mna_state_w2) %>% 
    mutate(tp=n/fu,
           imp=i)
  
  return(tp_table_temp)
}) %>% 
  bind_rows()


tp_table<-tp_imp %>% 
  group_by(mna_state_w1,mna_state_w2) %>%
  summarise(tp=mean(tp),.groups = "drop") %>% 
  filter(!(mna_state_w1==1&mna_state_w2==3),
         !(mna_state_w1==3&mna_state_w2==1),
         !(mna_state_w1==1&mna_state_w2==1),
         !(mna_state_w1==2&mna_state_w2==2),
         !(mna_state_w1==3&mna_state_w2==3)) %>% 
  rename(state_start=mna_state_w1,
         state_next=mna_state_w2)



saveRDS(tp_table,"Malnutrition statistical analysis/Malnutrition-study/tp_table.rds")

