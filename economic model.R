library(tidyverse)
library(haven)
library(heemod)

setwd("~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/Malnutrition statistical analysis/Malnutrition-study/")

# get transition probabilities
tp_table<-readRDS("tp_table.rds")

get_tp<-function(s1,s2,data){
  data %>%
    filter(
      state_start == s1,
      state_next == s2) %>%
    pull(tp)
}

pairs<-list(
  c(1,2), c(1,4),
  c(2,1), c(2,3), c(2,4),
  c(3,2), c(3,4)
)

for (p in pairs) {
  var_name <- paste0("tp", p[1], p[2])
  assign(
    var_name,
    get_tp(p[1], p[2], tp_table)
  )
}



malnu_model<-function(n_pop,rr_red,rr_conv,rr_mort,cost_int_gen,cost_int_geri_add,n_cycles){
  
  # Input parameters and rationales specified in "Malnutrition model_input parameters.xlsx"
  # specify starting population
  n_state1=round(n_pop*0.68,digits = 0)
  n_state2=round(n_pop*0.28,digits = 0)
  n_state3=round(n_pop*0.04,digits = 0)
  
  para<-define_parameters(  
    # proportion of geriatric patients in each mna state
    prop_geri_state1=0.01,
    prop_geri_state2=0.04,
    prop_geri_state3=0.13,
    
    # costs of care for SOC for healthy older adults
    state1_cost_gen=27546,
    state2_cost_gen=29894,
    state3_cost_gen=30220,
    
    # costs of care for SOC for geriatric patients
    state1_cost_geri=596834,
    state2_cost_geri=712126,
    state3_cost_geri=761223,
    
    
    # utility values
    state1_utility=0.783,
    state2_utility=0.757,
    state3_utility=0.720,
    
    # disutility for geriatric patients
    geri_disutility=0.311,
    
    # continuously compounded discounting factor
    disc_fac=exp(-0.03*(model_time-1)),
    
    # costs associated with interventions for malnutrition
    cost_rx=dispatch_strategy(
      strat_soc=0,
      strat_int=cost_int_gen))
  

  # define transition matrix for no intervention
  mat_soc<-define_transition(
    C,tp12,0,tp14,
    tp21,C,tp23,tp24,
    0,tp32,C,tp34,
    0,0,0,1)
  
  # define transition matrix for intervention 
  mat_int<-define_transition(
    C,tp12*(1-rr_red),0,tp14*(1-rr_mort),
    tp21*(1+rr_conv),C,tp23*(1-rr_red),tp24*(1-rr_mort),
    0,tp32*(1+rr_conv),C,tp34*(1-rr_mort),
    0,0,0,1)
  
  # define costs and utility in states
  state1<-define_state(
    # costs of SOC - same for both strategies
    cost_care=((state1_cost_gen)*(1-prop_geri_state1)+
                 state1_cost_geri*prop_geri_state1)*disc_fac,
    
    # costs of treatments, 0 for SOC
    cost_rx=(cost_rx*(1-prop_geri_state1)+
               (cost_rx+cost_int_geri_add)*prop_geri_state1)*disc_fac,
    
    # total costs - costs of SOC + costs of treatments
    cost_total=((state1_cost_gen)*(1-prop_geri_state1)+
                  state1_cost_geri*prop_geri_state1+
                  cost_rx*(1-prop_geri_state1)+
                  (cost_rx+cost_int_geri_add)*prop_geri_state1)*disc_fac,
    
    utility=(state1_utility*(1-prop_geri_state1)+
               geri_disutility*prop_geri_state1)*disc_fac,
    
    life_year=1
  )
  
  state2<-define_state(
    # costs of SOC - same for both strategies
    cost_care=((state2_cost_gen)*(1-prop_geri_state2)+
                 state2_cost_geri*prop_geri_state2)*disc_fac,
    
    # costs of treatments, 0 for SOC
    cost_rx=(cost_rx*(1-prop_geri_state2)+
               (cost_rx+cost_int_geri_add)*prop_geri_state2)*disc_fac,
    
    # total costs - costs of SOC + costs of treatments
    cost_total=((state2_cost_gen)*(1-prop_geri_state2)+
                  state2_cost_geri*prop_geri_state2+
                  cost_rx*(1-prop_geri_state2)+
                  (cost_rx+cost_int_geri_add)*prop_geri_state2)*disc_fac,
    
    utility=(state2_utility*(1-prop_geri_state2)+
               geri_disutility*prop_geri_state2)*disc_fac,
    
    life_year=1
  )
  
  state3<-define_state(
    # costs of SOC - same for both strategies
    cost_care=((state3_cost_gen)*(1-prop_geri_state3)+
                 state3_cost_geri*prop_geri_state3)*disc_fac,
    
    # costs of treatments, 0 for SOC
    cost_rx=(cost_rx*(1-prop_geri_state3)+
               (cost_rx+cost_int_geri_add)*prop_geri_state3)*disc_fac,
    
    # total costs - costs of SOC + costs of treatments
    cost_total=((state3_cost_gen)*(1-prop_geri_state3)+
                  state3_cost_geri*prop_geri_state3+
                  cost_rx*(1-prop_geri_state3)+
                  (cost_rx+cost_int_geri_add)*prop_geri_state3)*disc_fac,
    
    utility=(state3_utility*(1-prop_geri_state3)+
               geri_disutility*prop_geri_state3)*disc_fac,
    
    life_year=1
  )
  
  # 0 for all costs and utility for death state
  state4<-define_state(
    cost_care=0,
    cost_rx=0,
    cost_total=0,
    utility=0,
    life_year=0
  )
  
  # define intervention
  strat_soc<-define_strategy(
    transition = mat_soc,
    state1,
    state2,
    state3,
    state4
  )
  
  strat_int<-define_strategy(
    transition = mat_int,
    state1,
    state2,
    state3,
    state4
  )
  
  res_mod<-run_model(
    strat_soc=strat_soc,
    strat_int=strat_int,
    parameters = para,
    init = c(n_state1,n_state2,n_state3,0),
    cycles=n_cycles,
    cost=cost_total,
    effect=utility
  )
  
  output<-res_mod$run_model %>% select(cost_care:life_year) %>% 
    rbind(res_mod$run_model %>% select(cost_care:life_year) %>% 
            summarise(across(everything(), ~ .[2] - .[1]))) %>% 
    as.data.frame() %>% 
    cbind(strategy=c("SoC","Intervention","Intervention vs SoC")) %>% 
    mutate(dc=ifelse(strategy=="Intervention vs SoC",cost_total/n_pop,NA),
           de=ifelse(strategy=="Intervention vs SoC",utility/n_pop,NA),
           ICER=ifelse(strategy=="Intervention vs SoC",cost_total/utility,NA)) %>% 
    mutate(across(c(cost_care,cost_rx,cost_total,utility,life_year,
                    dc,ICER),~ ifelse(is.na(.x), "-", format(round(.x, 0), big.mark = ","))),
           de=ifelse(is.na(de), "-", format(round(de, 2), nsmall = 2))) %>% 
    select(strategy,everything()) %>% 
    setnames(c("Strategy","Background costs of care",
               "Costs of intervention","Total costs",
               "Quality-Adjusted Life Year","Life years",
               "Incremental cost","Incremental effect","ICER"))
  
  return(output)
  
}


malnu_model(n_pop=1000,
            rr_red=0.2,rr_conv=0.2,rr_mort=0,
            cost_int_gen=10000,cost_int_geri_add=0,n_cycles=40)
