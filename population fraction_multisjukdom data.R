library(RClickhouse)
library(DBI)
library(dplyr)
library(dbplyr)
library(tidyverse)

setwd("~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Malnutrition project data analysis/Data/")


con<-dbConnect(RClickhouse::clickhouse(), 
                 host = "h1cogbase01.nvs.ki.se", 
                 port = 9000,
                 user='xin_xia',
                 database='multisjuka',
                 password=askpass::askpass("Please enter clickhouse password: "))

DBI::dbGetQuery(con, "SHOW TABLES FROM multisjuka")

tbl(con, in_schema("multisjuka", "multisjuka_flytt")) %>% 
  colnames()

tbl(con, in_schema("multisjuka", "multisjuka_husl")) %>% 
  colnames()

tbl(con, in_schema("multisjuka", "multisjuka_kon")) %>% 
  colnames()


tbl(con, in_schema("multisjuka", "multisjuka_kon")) %>%
  mutate(year = substr(bdat, 1, 4)) %>%
  count(year, name = "n") %>%
  arrange(year) %>%
  collect()

# multisjuka_kon only has 2010-2013 records

tbl(con, in_schema("multisjuka", "multisjuka_recept")) %>% 
  colnames()

tbl(con, in_schema("multisjuka", "multisjuka_varureg")) %>% 
  colnames()

tbl(con, in_schema("multisjuka", "multisjuka_ovr")) %>% 
  colnames()

tbl(con, in_schema("multisjuka", "multisjuka_slv")) %>% 
  colnames()


# check data amount in each dataset
# 277645345 observations
tbl(con, in_schema("multisjuka", "multisjuka_ovr")) %>% 
  summarise(n=n())


# 4785802 observations
tbl(con, in_schema("multisjuka", "multisjuka_slv")) %>% 
  summarise(n=n())


# 28558383
tbl(con, in_schema("multisjuka", "multisjuka_kon")) %>% 
  summarise(n=n())

# information from ethical permit (dnr: 2023-07166-02):
# Data från VAL-databaserna kommer att hämtas från 2010 och framåt. Data
# från VAL-databaserna kommer att hämtas från 2010 och fram till slutet av 2025.


# select people aged >=60 years (n=142067153)
tbl(con, in_schema("multisjuka", "multisjuka_ovr")) %>% 
  filter(alder>=60) %>% 
  summarise(n=n())


# n=2703405
tbl(con, in_schema("multisjuka", "multisjuka_slv")) %>% 
  filter(alder>=60) %>% 
  summarise(n=n())




# 1. Select random samples from ovr and slv to inspect the data ----
# use data from older adults and 2024 only
# ovr
ovr_sample<-tbl(con, in_schema("multisjuka", "multisjuka_ovr")) %>%
  filter(alder>=60,
         ar=="2024") %>% 
  distinct(lopnr) %>% 
  collect %>% 
  slice_sample(n=10000)

ovr_sample<-tbl(con, in_schema("multisjuka", "multisjuka_ovr")) %>% 
                  filter(alder>=60,
                         ar=="2024",
                         lopnr %in% ovr_sample$lopnr) %>% 
  collect


table(ovr_sample$koen)

# slv
slv_sample<-tbl(con, in_schema("multisjuka", "multisjuka_slv")) %>%
  filter(alder>=60,
         ar=="2024") %>% 
  distinct(lopnr) %>% 
  collect %>% 
  slice_sample(n=10000)

table(slv_sample$koen)

slv_sample<-tbl(con, in_schema("multisjuka", "multisjuka_slv")) %>% 
  filter(alder>=60,
         ar=="2024",
         lopnr %in% slv_sample$lopnr) %>% 
  collect
       
# variables with information on types of clinics:
# drg type: G-DRG Avser DRG-ersättning inom geriatriken
# EKVARDGREN: 
# From Megan Doheny' thesis: surgical = ’02’,psychiatric=’03’, geriatrician =’04’ and medical = ’05’
# Exempel på ny kod 2 = Akutsomatisk vård, 3 = Psykiatri, 4 = Primärvård
# Gammal kod (före 2012): 01 = Primärvård, 02 = Psykiatri, 03 = Geriatrik, 04 = Akutsjukvård, 05 = Övrigt
# Kombika: Avdelningsdelen ur verksamhetsidentiteten (Kombika) för vårdande mottagning/motsvarande.
# Inr =5tkn, Klin=3 tkn, Avd=1-3 tkn.
# Exempel:
# Kombika=11012311M08, Inr=110123, Klin=311, Avd=M08
# KLIN 241 = Geriatrisk vård, 249 = Geriatrisk rehabilitering




# Check if these variables gives consistent information about whether the records relate to geriatric care
ovr_sample<-ovr_sample %>% 
  mutate(klin=substr(kombika,6,8),
         drgtype=substr(drg,1,1))

table(ovr_sample$ekvardgren)
table(ovr_sample$drgtype,ovr_sample$ekvardgren,useNA = "ifany")
table(ovr_sample$klin,ovr_sample$ekvardgren,useNA = "ifany")
table(ovr_sample$drgtype,ovr_sample$klin,useNA = "ifany")

table((ovr_sample %>% filter(klin %in% c(241,249)))$ekvardgren,useNA = "ifany")



slv_sample<-slv_sample %>% 
  mutate(klin=substr(kombika,6,8))

table(slv_sample$ekvardgren)
table(slv_sample$drgtyp,slv_sample$ekvardgren,useNA = "ifany")
table(slv_sample$klin,slv_sample$ekvardgren,useNA = "ifany")
table(slv_sample$drgtyp,slv_sample$klin,useNA = "ifany")

table((slv_sample %>% filter(klin %in% c(241,249)))$ekvardgren,useNA = "ifany")


# 2. Use KOMBIKA to get SPEC ----
# Meaning of SPEC
# 0xx = uppgift saknas
# 1xx = paramedicin
# 2xx = medicinsk service, operation, IVA
# 3xx = medicin
# 4xx = kirurgi
# 5xx = psykiatri
# 6xx = geriatrik
# 7xx = barn
# 8xx = primärvård
# 9xx = övrigt

slv_full<-tbl(con, in_schema("multisjuka", "multisjuka_slv")) %>% 
  filter(alder>=60,
         substr(indat,1,4)=="2024") %>% 
  select(lopnr,alder,indat,kombika) %>% 
  collect

kombika_spec<-read_excel("Malnutrition statistical analysis/Malnutrition-study/kombika_klin_spec_regions_stockholm.xlsx") %>% 
  as.data.frame() %>% 
  filter(source=="slv") %>% 
  rename(kombika=KOMBIKA) %>% 
  select(kombika,SPEC) %>% 
  filter(!is.na(SPEC)) %>%
  group_by(kombika) %>%
  mutate(num=row_number()) %>%
  pivot_wider(names_from=num,
              names_prefix = "SPEC",
              values_from=SPEC)


slv_full<-slv_full %>% 
  left_join(kombika_spec,by="kombika") %>% 
  mutate(geri=ifelse(grepl("^6",SPEC1)|grepl("^6",SPEC2),1,0),
         age_group=case_match(alder,
                              60:64 ~ 1,
                              65:69 ~ 2,
                              70:74 ~ 3,
                              75:79 ~ 4,
                              80:84 ~ 5,
                              85:89 ~ 6,
                              90:120 ~ 7,
                              .default = NA),
         klin=substr(kombika,6,8))


slv_full %>% 
  filter(geri==1) %>%
  distinct(lopnr,.keep_all = TRUE) %>%
  group_by(age_group) %>%
  summarise(n=n())



table((slv_full %>% filter(geri==1) %>%
         distinct(lopnr,.keep_all = TRUE))$klin)




