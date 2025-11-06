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

setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-KarolinskaInstitutet/GRP_Economic evaluations of malnutrition in Swedish older adults (KI Livsmedelsverket) - Documents/Individal level data analysis/Data/")
#setwd("C:/Users/xinxia/Karolinska Institutet/GRP_Economic evaluations of malnutrition in Swedish older adults (KI Livsmedelsverket) - Documents/Individal level data analysis/Data/")
source("Malnutrition statistical analysis/Malnutrition-study/snack_data cleaning.R")


# 1. Baseline characteristics ----
label(nutri_hcru_cov$age1)<-"Age"
label(nutri_hcru_cov$sex)<-"Sex"
label(nutri_hcru_cov$education)<-"Education"
label(nutri_hcru_cov$lives1)<-"Living alone"
label(nutri_hcru_cov$tobacco1)<-"Smoking status"
label(nutri_hcru_cov$mna_screening1_cat)<-"Nutritional status (MNA)"
label(nutri_hcru_cov$glim_malnutr1)<-"Malnutrition (GLIM)"
label(nutri_hcru_cov$bmi_cat_w1)<-"BMI category"
label(nutri_hcru_cov$mi)<-"Myocardial infarction"
label(nutri_hcru_cov$hf)<-"Heart failure"
label(nutri_hcru_cov$stroke)<-"Stroke"
label(nutri_hcru_cov$pd)<-"Parkinson's disease"
label(nutri_hcru_cov$copd)<-"Chronic obstructive pulmonary disease"
label(nutri_hcru_cov$ra)<-"Rheumatoid arthritis"
label(nutri_hcru_cov$op)<-"Osteoporosis"
label(nutri_hcru_cov$cancer)<-"Cancer"
label(nutri_hcru_cov$depression)<-"Depression"
label(nutri_hcru_cov$dementia)<-"Dementia"
label(nutri_hcru_cov$smell_imp)<-"Loss of smell"
label(nutri_hcru_cov$taste_imp)<-"Loss of taste"
label(nutri_hcru_cov$chew_imp)<-"Difficulty with chewing food"
label(nutri_hcru_cov$poor_appetite)<-"Poor appetite"


table1<-table1::table1(~age1+sex+education+lives1+tobacco1+mna_screening1_cat+glim_malnutr1+
                 bmi_cat_w1+mi+hf+stroke+pd+copd+ra+op+cancer+depression+dementia+
                 smell_imp+taste_imp+chew_imp|age_group,
               data = nutri_hcru_cov %>% 
                 filter(fu>0) %>% 
                 mutate(age_group=as.factor(ifelse(age1<78,"60-72","78+"))) %>%
                 distinct(lopnr,.keep_all = TRUE)) %>% 
  as.data.frame()

colnames(table1)[1]<-"Characteristics"
class(table1)<-"data.frame"
table1<-table1 %>% 
  filter(!grepl("0|Male|Median",Characteristics))

table1[]<-lapply(table1, function(x) gsub("%", "", x))

# 2. Impute missing data in variables with MICE ----
imp<-mice(nutri_hcru_cov %>% 
            filter(fu>0) %>% 
            distinct(lopnr,.keep_all = TRUE) %>% 
            select(lopnr,mna_screening1_cat,glim_malnutr1,age1,sex,education,lives1,tobacco1,
                   mi,hf,stroke,pd_dem,copd,ra_op,cancer,depression,
                   smell_taste_imp,chew_imp,fu,death),maxit=0,seed=2025)
predM<-imp$predictorMatrix
predM[,c("lopnr","fu","death")]<-0
meth<-imp$method
meth[c("mna_screening1_cat","glim_malnutr1","education","tobacco1")]<-"polr"
meth[c("fu","death")]<-""

nutri_hcru_cov_imp<-mice(nutri_hcru_cov %>% 
                           filter(fu>0) %>% 
                           select(lopnr,mna_screening1_cat,glim_malnutr1,age1,sex,
                                  bmi_cat_w1,education,lives1,tobacco1,
                                  mi,hf,stroke,pd_dem,copd,ra_op,cancer,depression,
                                  smell_taste_imp,chew_imp,fu,death),
                         predictorMatrix = predM,method = meth,
                         m=5,maxit=20,seed=2025)


# 3. Factors associated with malnutrition ----
covarlist<-c("education","lives1","tobacco1","bmi_cat_w1",
             "mi","hf","stroke","pd_dem","copd","ra_op","cancer",
             "depression","smell_taste_imp","chew_imp")

var_names<-c(
  "education"="Education",
  "lives1"="Living alone",
  "tobacco1"="Smoking status",
  "bmi_cat_w1"="BMI category",
  "mi"="Myocardial infarction",
  "hf"="Heart failure",
  "stroke"="Stroke",
  "pd_dem"="Parkinson's disease or dementia",
  "copd"="COPD",
  "ra_op"="Rheumatoid arthritis or osteoporosis",
  "cancer"="Cancer",
  "depression"="Depression",
  "smell_taste_imp"="Loss of smell ortaste",
  "chew_imp"="Difficulty with chewing food"
)

nutri_pred_sum<-data.frame(scale=character(0),nutri_status=numeric(0),pred=character(0),
                           term=character(0),ci=character(0),p=character(0))

for (s in c("mna_screening1_cat","glim_malnutr1")){
  for (i in seq_along(covarlist)){
    
    temp_formula_str<-paste0(s," ~ ", covarlist[i], " + age1 + sex")
    
    temp_nutri_pred<-with(nutri_hcru_cov_imp,multinom(as.formula(temp_formula_str)))
    
    temp_nutri_pred_sum<-summary(pool(temp_nutri_pred),conf.int = TRUE,conf.level = 0.95) %>% 
      as.data.frame() %>% 
      mutate(n=n(),
             nutri_status=ifelse(row_number()<=n/2,1,2)) %>% 
      filter(!term %in% c("(Intercept)","age1","sexFemale")) %>% 
      mutate(pred=covarlist[i],
             term=gsub(covarlist[i], "", term),
             ci=paste0(format(round(exp(estimate),digits = 2),nsmall=2),
                       " (",
                       format(round(exp(`2.5 %`),digits = 2),nsmall=2),
                       "-",
                       format(round(exp(`97.5 %`),digits = 2),nsmall=2),
                       ")"),
             p=ifelse(p.value<0.001,"<0.001",as.character(format(round(p.value,digits = 3),nsmall=3)))) %>% 
      mutate(scale=s) %>% 
      select(scale,nutri_status,pred,term,ci,p)
    
    nutri_pred_sum<-nutri_pred_sum %>% 
      rbind(temp_nutri_pred_sum)
  }
}


nutri_pred_results<-nutri_pred_sum %>% 
  mutate(pred=recode(pred, !!!var_names),
         nutri_status=case_when(scale=="mna_screening1_cat"&nutri_status==1~"At risk",
                                scale=="mna_screening1_cat"&nutri_status==2~"Malnourished",
                                scale=="glim_malnutr1"&nutri_status==1~"Moderate malnutrition",
                                scale=="glim_malnutr1"&nutri_status==2~"Severe malnutrition"),
         term=ifelse(term==1,"",paste0("-",term)),
         pred=paste0(pred,term)) %>% 
  select(-term) %>% 
  mutate(scale=ifelse(scale=="glim_malnutr1","Malnutrition (GLIM)","Nutritional status (MNA)")) %>% 
  set_names(c("Nutritional assessment scale",
              "Nutritional status",
              "Factor",
              "OR (95% CI)",
              "P-value"))


# 4. Average care visits by nutritional status ----
# function for summarising average care visits
annual_visits_95ci<-function(data,indices,scale,outcome,time) {
  data<-data %>% 
    group_by(lopnr,.data[[scale]],.data[[time]]) %>% 
    summarise(sum_outcome=sum(.data[[outcome]],na.rm = T),.groups="drop")
    
  d<-data[indices, ] %>% 
    filter(!is.na(sum_outcome),
           !is.na(.data[[time]]),
           !is.na(.data[[scale]]))
  
  temp_visits<-d %>% 
    group_by(.data[[scale]]) %>% 
    summarise(sum_visits=sum(sum_outcome),
              sum_length=sum(.data[[time]]),.groups = "drop") %>% 
    mutate(annual_visits=sum_visits/sum_length)
  
  return(temp_visits$annual_visits)
}


# 4.1. Average care visits by nutritional status defined by MNA ----
set.seed(2025)
mna_ov_visits_95ci<-tidy(boot(data = nutri_hcru_cov %>% filter(fu>0), 
                              statistic = annual_visits_95ci, 
                              scale="mna_screening1_cat",
                              outcome="ov_num",time="fu",
                              R = 1000, parallel = "multicore", ncpus = 4),
                         conf.int=TRUE,conf.method="perc") %>% 
  cbind(c(1,2,3))

mna_sv_visits_95ci<-tidy(boot(data = nutri_hcru_cov %>% filter(fu>0), 
                              statistic = annual_visits_95ci, 
                              scale="mna_screening1_cat",
                              outcome="sv_num",time="fu",
                              R = 1000, parallel = "multicore", ncpus = 4),
                         conf.int=TRUE,conf.method="perc") %>% 
  cbind(c(1,2,3))

mna_pv_visits_95ci<-tidy(boot(data = nutri_hcru_cov %>% filter(pv_fu>0), 
                              statistic = annual_visits_95ci, 
                              scale="mna_screening1_cat",
                              outcome="pv_num",time="pv_fu",
                              R = 1000, parallel = "multicore", ncpus = 4),
                         conf.int=TRUE,conf.method="perc") %>% 
  cbind(c(1,2,3))


# 4.2. Average care visits by nutritional status defined by GLIM ----
glim_ov_visits_95ci<-tidy(boot(data = nutri_hcru_cov %>% filter(fu>0), 
                              statistic = annual_visits_95ci, 
                              scale="glim_malnutr1",
                              outcome="ov_num",time="fu",
                              R = 1000, parallel = "multicore", ncpus = 4),
                         conf.int=TRUE,conf.method="perc") %>% 
  cbind(c(1,2,3))

glim_sv_visits_95ci<-tidy(boot(data = nutri_hcru_cov %>% filter(fu>0), 
                              statistic = annual_visits_95ci, 
                              scale="glim_malnutr1",
                              outcome="sv_num",time="fu",
                              R = 1000, parallel = "multicore", ncpus = 4),
                         conf.int=TRUE,conf.method="perc") %>% 
  cbind(c(1,2,3))

glim_pv_visits_95ci<-tidy(boot(data = nutri_hcru_cov %>% filter(pv_fu>0), 
                              statistic = annual_visits_95ci, 
                              scale="glim_malnutr1",
                              outcome="pv_num",time="pv_fu",
                              R = 1000, parallel = "multicore", ncpus = 4),
                         conf.int=TRUE,conf.method="perc") %>% 
  cbind(c(1,2,3))


# combine and visualize the results
mna_labels<-c("Normal","At risk","Malnourished")
glim_labels<-c("No malnutrition","Moderate malnutrition","Severe malnutrition")

for (i in c("ov","sv","pv")) {
  temp_mna<-get(paste0("mna_",i,"_visits_95ci")) %>% 
    rename_with(~ "cat",6) %>% 
    mutate(scale="MNA",
           cat=factor(cat,levels=1:3,
                      labels = str_wrap(mna_labels, width = 15)))
  
  temp_glim<-get(paste0("glim_",i,"_visits_95ci")) %>% 
    rename_with(~ "cat",6) %>% 
    mutate(scale="GLIM",
           cat=factor(cat,levels=1:3,
                      labels = str_wrap(glim_labels, width = 15)))
  
  temp_com<-bind_rows(temp_mna,temp_glim) %>% 
    select(statistic,conf.low,conf.high,cat,scale) %>% 
    rename(est=statistic,lb=conf.low,ub=conf.high) %>%
    mutate(scale=factor(scale,levels=c("MNA","GLIM")))
  
  assign(
    paste0("comb_",i,"_visits_plot"),
    ggplot(temp_com,aes(x=cat,y=est)) +
      geom_col(fill="steelblue",width=0.6) +
      geom_errorbar(aes(ymin=lb,ymax=ub),width=0.2) +
      facet_wrap(~ scale,ncol=2,scales="free_x") +
      labs(x="",y="Annual Visits and 95% CI",title="") +
      theme_minimal(base_size=12) +
      theme(panel.grid=element_blank(),
            axis.line=element_line(color="black"),
            panel.border=element_blank(),
            axis.title.x = element_text(size=10))
  )
}

pdf("Malnutrition statistical analysis/figure_1_combined_visits.pdf",
    width=6,height=10)
ggarrange(comb_ov_visits_plot,comb_sv_visits_plot,comb_pv_visits_plot,
          ncol=1,nrow=3,labels=c("A. Outpatient care visits",
                                 "B. Inpatient care visits",
                                 "C. Primary care visits"))
dev.off()




# 5. Associations of MNA and GLIM with mortality ----
mna_mort<-with(nutri_hcru_cov_imp,
               flexsurvreg(Surv(fu,death)~mna_screening1_cat+age1+sex,dist="weibullPH"))

mna_mort_adj<-with(nutri_hcru_cov_imp,
               flexsurvreg(Surv(fu,death)~mna_screening1_cat+age1+
                             education+lives1+tobacco1+mi+
                           hf+stroke+pd_dem+copd+ra_op+cancer+
                           depression+smell_taste_imp+chew_imp,dist="weibullPH"))

glim_mort<-with(nutri_hcru_cov_imp,
               flexsurvreg(Surv(fu,death)~glim_malnutr1+age1+sex,dist="weibullPH"))

glim_mort_adj<-with(nutri_hcru_cov_imp,
                   flexsurvreg(Surv(fu,death)~glim_malnutr1+age1+
                                 education+lives1+tobacco1+mi+
                                 hf+stroke+pd_dem+copd+ra_op+cancer+
                                 depression+smell_taste_imp+chew_imp,dist="weibullPH"))

nutri_mort_results<-summary(pool(mna_mort),conf.int = TRUE,conf.level = 0.95) %>%
  mutate(model=1,
         scale="mna") %>% 
  rbind(summary(pool(mna_mort_adj),conf.int = TRUE,conf.level = 0.95) %>%
          mutate(model=2,
                 scale="mna")) %>% 
  rbind(summary(pool(glim_mort),conf.int = TRUE,conf.level = 0.95) %>%
          mutate(model=1,
                 scale="glim")) %>% 
  rbind(summary(pool(glim_mort_adj),conf.int = TRUE,conf.level = 0.95) %>%
          mutate(model=2,
                 scale="glim")) %>% 
  filter(grepl("mna_",term)|grepl("glim_",term)) %>% 
  mutate(term=gsub("mna_screening1_cat", "", term),
         term=gsub("glim_malnutr1", "", term),
         term = trimws(term)) %>% 
  mutate(ci=paste0(format(round(exp(estimate),digits = 2),nsmall=2),
                   " (",
                   format(round(exp(`2.5 %`),digits = 2),nsmall=2),
                   "-",
                   format(round(exp(`97.5 %`),digits = 2),nsmall=2),
                   ")"),
         p=ifelse(p.value<0.001,"<0.001",as.character(format(round(p.value,digits = 3),nsmall=3)))) %>% 
  select(scale,model,term,ci)

writexl::write_xlsx(
  list(
    "table1" = table1,
    "pred" = nutri_pred_results,
    "mort" = nutri_mort_results
  ),
  path = "Malnutrition statistical analysis/Malnutrition-study/results_xx.xlsx"
)
