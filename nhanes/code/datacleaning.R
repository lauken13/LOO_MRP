  ## Data cleaning
library(here)
library(foreign) # read.xport()
library(tidyverse)
library(questionr) # recode.na()
library(GGally) # ggpairs()
library(forcats) #fct_relevel
library(survey)
library(dbarts)
# https://www.aihw.gov.au/reports/risk-factors/risk-factors-to-health/contents/high-blood-pressure
# https://www.mayoclinic.org/diseases-conditions/high-blood-pressure/symptoms-causes/syc-20373410

## 2017-2020 
# demographic - age, race, sex/gender, education, married status ####
demo1720 = read.xport(here('nhanes/data/NHANES 2017-20/P_DEMO.XPT'))  %>% # 15560 obs total
  filter(RIDAGEYR >= 18) %>% # 13218 for 6 y/o and above, 11233 for 12 y/o, 10195 for 16 y/o, 9693 for 18 y/o 
  mutate(age = cut(RIDAGEYR, breaks = c(18, 29, 39, 49, 59, 69, 80), right = F, labels = F),
         age = recode(as.factor(age), '1' = '18-29', '2' = '30-39', '3' = '40-49', '4' = '50-59', '5' = '60-69', '6' = '70-80'),
         ethnicity = recode(as.factor(RIDRETH3), '1' = 'MexAme', '2' =  'OtherHisp', '3' = 'NonHispWhite', '4' = 'NonHispBlack', '6' = 'NonHispAsian', '7' = 'Other'),    #'1-Mexican American; 2-Other Hispanic; 3-Non-Hispanic White; 4-Non-Hispanic Black; 6-Non-Hispanic Asian; 7-Other Race including Multi-Racial'
         gender = recode(as.factor(RIAGENDR), '1' = 'male', '2' = 'female'),  # '1-male; 2-female
         DMDEDUC2 = recode.na(as.factor(DMDEDUC2), c('7', '9')), #1-Less than 9th grade; 2-9-11th grade (Includes 12th grade with no diploma);3-High school graduate/GED or equivalent;4-Some college or AA degree;5-College graduate or above; 7-Refused; 9-Don't Know,
         educ = recode(as.factor(DMDEDUC2), '1' = 'less9th', '2' = '9-11thgrade', '3' = 'HSgrad', '4' = 'SomecollegeAA', '5' = 'Collegegrad'),
         marital_status = recode.na(as.factor(DMDMARTZ), c('77', '99')), 
        marital_status = recode(marital_status, '1' = 'married/living tgt', '2' = 'widowed/separated', 
                                  '3' = 'never married')) %>% 
  select(SEQN, age, ethnicity, gender, educ, marital_status)
# sum(demo1720$RIDAGEYR >= 18 & demo1720$RIDAGEYR < 25)


# outcome - high blood pressure 
bp = read.xport(here('nhanes/data/NHANES 2017-20/P_BPQ.XPT')) %>% 
  mutate(high_bp = recode(as.factor(BPQ020), '9' = '0'), # recode '9 - don't know' as 'no'
         high_bp = recode(high_bp, '2' = '0'),
         high_bp = fct_relevel(high_bp, c('0','1'))) %>% 
  select(SEQN, high_bp) 

# covariates - physical activity 
paq = read.xport(here('nhanes/data/NHANES 2017-20/P_PAQ.XPT')) %>%  # 18 y/0 and above
  mutate(phys_act = recode(as.factor(PAQ650), '2' = '0'), # '1 - yes to doing vigorous intensity for 10 mins'; '2 - no'
         phys_act = fct_relevel(phys_act, c('0','1'))) %>% 
  select(SEQN, phys_act) 

# covariates - overweight
mcq = read.xport(here('nhanes/data/NHANES 2017-20/P_MCQ.XPT')) %>% # 1 - doctor ever said overweight, 2 - no
  mutate(overweight = recode(as.factor(MCQ080), "9" = "2"),  # recode '9 - don't know' as no
         overweight = recode(overweight, '2' = '0'),
         overweight = fct_relevel(overweight, c('0','1'))) %>%
  select(SEQN, overweight)

# covariates - sodium (day1), potassium ####
# https://www.sciencedirect.com/science/article/pii/S000282239900423X?casa_token=oW171FSUo-YAAAAA:MMGugjEsy6PAlexkLyy2s8Z-rImUytgQA8rxNaTVEG80LaSY8gru1cH7R8_9n3vAwZQTcJm8Zfk
dr1 = read.xport(here('nhanes/data/NHANES 2017-20/P_DR1TOT.XPT')) %>% 
  mutate(sodium_intake1 = replace_na(DR1TSODI, 0),
         potassium_intake1 = replace_na(DR1TPOTA,0)) %>% 
  select(SEQN, sodium_intake1, potassium_intake1) 

# covariates - sodium (day2), potassium ####
# https://academic.oup.com/advances/article/4/3/368S/4591617
dr2 = read.xport(here('nhanes/data/NHANES 2017-20/P_DR2TOT.XPT')) %>% 
  mutate(sodium_intake2 = replace_na(DR2TSODI,0),
         potassium_intake2 = replace_na(DR2TPOTA,0)) %>%
  select(SEQN, sodium_intake2, potassium_intake2) 


# covariates - alcohol use ####
# https://www.niaaa.nih.gov/publications/brochures-and-fact-sheets/alcohol-facts-and-statistics
# 1 = alcohol consumption between 1–14 (men) or 1–7 (women) standard drinks per week; 2 = alcohol consumption exceeding 14 drinks (men) or 7 drinks (women) per week. 
alc = read.xport(here('nhanes/data/NHANES 2017-20/P_ALQ.XPT')) %>%  # 8965 obs - aged 12 and over sampled but data restricted to 18 and over
  left_join(., demo1720, by="SEQN") %>% 
  mutate(alcohol_use_day =  case_when(ALQ111 == '2' | ALQ121 == '0' ~ 0, # never drank alcohol and never in the last 12 months
                                      ALQ130 == "777" | ALQ130 == "999" ~ 999 , # combine refused and dont know
                                      ALQ130 == 1 | ALQ130 == 2 | ALQ130 == 3 | ALQ130 == 4 | ALQ130 == 5 
                                      | ALQ130 == 6 | ALQ130 == 7 | ALQ130 == 8 | ALQ130 == 9 | ALQ130 == 10
                                      | ALQ130 == 11 | ALQ130 == 12 | ALQ130 == 13 | ALQ130 == 14 | ALQ130 == 15 ~ ALQ130),
         alcohol_use_day = as.numeric(alcohol_use_day),
         alcohol_use_week = alcohol_use_day * 7,
         alc_exc = case_when(alcohol_use_week == 0 ~ 0,
                             gender == 'male' & alcohol_use_week >= 1 & alcohol_use_week <=14 ~ 1, 
                             gender == 'female' & alcohol_use_week >= 1 & alcohol_use_week <= 7 ~ 1, 
                             gender == 'male' & alcohol_use_week >14 ~ 2, 
                             gender == 'female' & alcohol_use_week >7 ~ 2), 
         alc_exc = recode(as.factor(alc_exc), '0' = 'no', '1' = 'moderate', '2' = 'excessive')) %>% # avg alcoholic drinks/day - past 12 mos
  select(SEQN, alc_exc) 

# covariates - bmi ####
bmi = read.xport(here('nhanes/data/NHANES 2017-20/P_BMX.XPT')) %>% 
  rename(BMI_raw = BMXBMI) %>% 
  drop_na(BMI_raw) %>% 
  mutate(BMI = cut(BMI_raw, breaks = c(11.8, 18.5, 25, 30, 35, 40, 92.6), right = F, labels=F),
         BMI = as.factor(BMI), 
         BMI_range = recode(BMI, '1' = "underweight", '2' = 'healthy', '3' = 'overweight', '4' = 'obeseI', '5' = 'obeseII', '6' = 'obeseIII')) %>% 
  select(SEQN, BMI_range)
# str(bmi$BMI_range)


# covariates - depression ####
dep = read.xport(here('nhanes/data/NHANES 2017-20/P_DPQ.XPT')) %>%  # same as alcohol use, aged 12 and over sampled but data restricted to 18 and over 
  mutate(DPQ010 = recode.na(as.factor(DPQ010), c('9', '7')),
         DPQ020 = recode.na(as.factor(DPQ020),  c('9', '7')),
         DPQ030 = recode.na(as.factor(DPQ030),  c('9', '7')),
         DPQ040 = recode.na(as.factor(DPQ040),  c('9', '7')),
         DPQ050 = recode.na(as.factor(DPQ050),  c('9', '7')),
         DPQ060 = recode.na(as.factor(DPQ060),  c('9', '7')),
         DPQ070 = recode.na(as.factor(DPQ070),  c('9', '7')),
         DPQ080 = recode.na(as.factor(DPQ080),  c('9', '7')),
         DPQ090 = recode.na(as.factor(DPQ090),  c('9', '7'))) %>% 
  mutate_if(is.factor, as.numeric) %>% 
  mutate(dep_score = DPQ010 + DPQ020 + DPQ030 + DPQ040 + DPQ050 +
           DPQ060 + DPQ070 + DPQ080 + DPQ090,
         dep_level = cut(dep_score, breaks = c(1, 9, 14, 19, 36), right = T, label=F),
         dep_severity = recode(dep_level, '1' = "mild", '2' = "moderate", '3' = "moderately severe", '4' = "severe"),
         dep_severity = as.factor(dep_severity)) %>% 
  select(SEQN, dep_severity)

# covariates - diabetes ####
diab = read.xport(here('nhanes/data/NHANES 2017-20/P_DIQ.XPT')) %>% 
  mutate(diabetes = case_when(DIQ010 == '9' | DIQ010 == '2'~ 0,
                              DIQ010 == '1' ~ 1,
                              DIQ010 == '3' ~ 2),
         diabetes = recode(as.factor(diabetes), '0' = 'no', '1' = 'yes', '2' = 'borderline')) %>%  # ever told have diabetes - 9 - don't know - code as no; 3 is borderline
  select(SEQN, diabetes)

# covariates - pregnancy ####
preg = read.xport(here('nhanes/data/NHANES 2017-20/P_RHQ.XPT')) %>% # 5314 - female only - can impute 0 for men? 
  mutate(preg_fem = case_when(RHD143 == "2" ~ 0,
                              RHD143 == "1" ~ 1,
                              RHD143 == "9" ~ 999),
         preg_fem = as.factor(preg_fem)) %>%  # are you pregnant now - 9 - don't know - code as no
  select(SEQN, preg_fem)

# covariates - trouble sleeping ####
sleep = read.xport(here('nhanes/data/NHANES 2017-20/P_SLQ.XPT')) %>% # Ever told doctor had trouble sleeping?
  mutate(trb_sleep = case_when(SLQ050 == "7" | SLQ050 == "9" | SLQ050 == "2" ~ 0,
                               SLQ050 == "1" ~ 1),
         trb_sleep = as.factor(trb_sleep)) %>%  # 7 - refused to answer; 9 - don't know
  select(SEQN, trb_sleep)

# covariates - tobacco ####
tob = read.xport(here('nhanes/data/NHANES 2017-20/P_SMQRTU.XPT')) %>% # Smoked any tobacco product last 5 days? 
  mutate(tob_last5 = case_when(SMDANY == '2' ~ 0, 
                               SMDANY == '1' ~ 1),
         tob_last5 = as.factor(tob_last5)) %>% 
  select(SEQN, tob_last5) 

# covariates - cigarette ####
cig = read.xport(here('nhanes/data/NHANES 2017-20/P_SMQ.XPT')) %>% # Do you now smoke cigarettes or smoke at least 100 cigarettes in life?
  mutate(cig_now = case_when(SMQ040 == '1' | SMQ040 == '2' ~ 1,  # recode 'some days' as 'occasional' smoker = 1
                             SMQ040 == '3' | SMD030 == '0' | SMQ020 == '2' | SMQ621 == '1' ~ 0, 
                             SMQ020 == '7' | SMQ020 == '9' ~ 999),
         cig_now = as.factor(cig_now)) %>%  # recode 'not at all - 3' as '0' so '1' is smoke and '0' is doesn't smoke
  select(SEQN, cig_now)

# covariates - 2nd hand smoke indoors ####
smk2a = read.xport(here('nhanes/data/NHANES 2017-20/P_SMQSHS.XPT')) %>% # Last 7-d someone smoked indoors 
  select(SEQN, SMQ858, SMQ862, SMQ868, SMQ872, SMQ876, SMQ880, SMQ940) %>%  # at work, restaurant, bar, car, other indoor areas, e-cig
  mutate(smk2_ind = case_when(SMQ858 == 1 | SMQ862 == 1 | SMQ868 == 1 | SMQ872 == 1 | 
                                SMQ876 == 1 | SMQ880 == 1 |SMQ940 == 1 ~ 1, 
                              SMQ858 == 2 | SMQ862 == 2 | SMQ868 == 2 | SMQ872 == 2 | 
                                SMQ876 == 2 | SMQ880 == 2 |SMQ940 == 2 ~ 0,
                              TRUE ~ NA_real_ ),
         smk2_ind = as.factor(smk2_ind)) %>% 
  select(SEQN, smk2_ind)  
# summary(smk2a)

# covariates - 2nd hand smoke at home ####
smk2b = read.xport(here('nhanes/data/NHANES 2017-20/P_SMQFAM.XPT')) %>% # of people who live here smoke tobacco?
  mutate(smk2_home = case_when(SMD460 == '0' | SMD460 == '777' | SMD460 == '999' ~ '0', # 777 - refused, 999 - don't know
                               SMD460 == '1' | SMD460 == '2' ~ '1'),
         smk2_home = as.factor(smk2_home)) %>% # recode 2+ smokers to be same group as 1+
  select(SEQN, smk2_home)  

# covariates - poverty level ####
# https://ajph.aphapublications.org/doi/pdf/10.2105/AJPH.91.5.781
pov = read.xport(here('nhanes/data/NHANES 2017-20/P_INQ.XPT')) %>%
  mutate(pov_level = as.factor(INDFMMPC),
         pov_level = recode(pov_level, '1' = 'low income', 
                            '2' = 'middle income', 
                            '3' = 'high income', 
                            '7' = 'refused',
                            '9' = 'dont know')) %>% 
  select(SEQN, pov_level)

# nhdata ####
fulldat = left_join(demo1720, bp, by='SEQN') %>% 
  left_join(., paq, by='SEQN') %>% 
  left_join(., mcq, by='SEQN') %>% 
  left_join(., dep, by='SEQN') %>% 
  left_join(., dr1, by='SEQN') %>% 
  left_join(., dr2, by='SEQN') %>% 
  left_join(., bmi, by='SEQN') %>% 
  left_join(., alc, by='SEQN') %>% 
  left_join(., diab, by='SEQN') %>% 
  left_join(., preg, by='SEQN') %>% 
  left_join(., sleep, by='SEQN') %>% 
  left_join(., tob, by='SEQN') %>% 
  left_join(., cig, by='SEQN') %>% 
  left_join(., smk2a, by='SEQN') %>% 
  left_join(., smk2b, by='SEQN') %>% 
  left_join(., pov, by='SEQN') %>%  # demo1720 - 6960; dat1 - 3489; dat2 - 2317
  mutate(smk2_exp = ifelse(smk2_ind == 1 | smk2_home == 1, 1, 0), # exposure to smoke at work or at home
         smk2_exp = as.factor(smk2_exp), 
         smk_tobcig = ifelse(tob_last5 == 1 | cig_now == 1, 1, 0),  # smokes tobacco or cigarette 
         smk_tobcig = as.factor(smk_tobcig),  
         preg_yes = case_when(preg_fem == "999" ~ 999,
                              gender == 'male' | preg_fem == '0' ~ 0,
                              preg_fem == "1" ~ 1), # if male then no for preg
         preg_yes = recode(as.factor(preg_yes), '999' = 'dont know'), 
         sodium_avg = ifelse(sodium_intake1 == 0, sodium_intake2, ifelse(sodium_intake2 == 0, sodium_intake1, ((sodium_intake1 + sodium_intake2)/2) )), 
         potassium_avg = ifelse(potassium_intake1 == 0, potassium_intake2, ifelse(potassium_intake2 == 0, potassium_intake1, ((potassium_intake1 + potassium_intake2)/2))),
         sodium_lvl = case_when(is.na(sodium_avg) ~ NA_real_,
                                sodium_avg <= 1150 ~ 1,
                                sodium_avg > 1150 & sodium_avg <= 2300 ~ 2, 
                                sodium_avg > 2300 & sodium_avg <= 3450 ~ 3,
                                sodium_avg > 3450 ~ 4),
         sodium_lvl = recode(as.factor(sodium_lvl), '1' = 'low', '2' = 'intermediate', '3' = 'high', '4' = 'very high'),
         potassium_lvl = case_when(is.na(potassium_avg) ~ NA_real_,
                                   potassium_avg <= 2400 ~ 1, 
                                   potassium_avg > 2400 & potassium_avg <= 3600 ~ 2,
                                   potassium_avg > 3600 ~ 3),
         potassium_lvl = recode(as.factor(potassium_lvl), '1' = 'low', '2' = 'adequate','3' = 'high'),
         potassium_lvl = fct_relevel(potassium_lvl, c('low', 'adequate', 'high'))) %>% 
  select(-c(smk2_ind, smk2_home, tob_last5, cig_now, preg_fem, sodium_intake1, sodium_intake2, potassium_intake1, potassium_intake2,
            sodium_avg, potassium_avg, preg_yes)) 


saveRDS(fulldat, file=here("nhanes/data/fulldat.RDS"))

# attempt to include more variables ---------------------------------------------
fulldat = readRDS(file="nhanes/data/fulldat.rds") 

# elastography
lux = read.xport(here('nhanes/data/NHANES 2017-20/P_LUX.XPT')) %>%
  mutate(elst_status = recode(as.factor(LUAXSTAT), '1' = 'complete', '2' = 'partial', '3' = 'ineligible', '4' = 'not done')) %>%
  select(SEQN, elst_status)

# pesticide use
puq = read.xport(here('nhanes/data/NHANES 2017-20/P_PUQMEC.XPT')) %>%
  mutate(pest_use = recode(as.factor(PUQ100), '7' = '9'),
         pest_use = recode(pest_use, '1' = 'yes', '2' = 'no', '9' = 'dont know/refused')) %>%
  select(SEQN, pest_use)


# ever tested for AIDS
hsq = read.xport(here('nhanes/data/NHANES 2017-20/P_HSQ.XPT')) %>%
  mutate(hiv_test = as.factor(HSQ590),
         hiv_test = recode(hiv_test, '1' = 'yes', '2' = 'no', '9' = 'dont know')) %>%
  select(SEQN, hiv_test)

# dentist visit
ohq = read.xport(here('nhanes/data/NHANES 2017-20/P_OHQ.XPT')) %>%
  mutate(den_vst = recode(as.factor(OHQ030), '4' = '3', '6' = '5', '77' = '99'),
         den_vst = recode(as.factor(den_vst), '1' = 'less6mths', '2' = '6mthto1year', '3' = '1to3years', '5' = '3yrormore', '7' = 'never'),
         den_vst = recode.na((den_vst), '99')) %>%
  select(SEQN, den_vst)

# urine collected at test
ucf = read.xport(here('nhanes/data/NHANES 2017-20/P_UCFLOW.XPT')) %>%
  mutate(urn_vol = cut_number((URXVOL1),4),
         urn_vol = recode(as.factor(urn_vol), '[0,47]' = 'low', '(47,85]' = 'med', '(85,145]' = 'high', '(145,455]' = 'veryhigh')) %>%
  select(SEQN, urn_vol)

fulldat_ext = left_join(fulldat, lux, by='SEQN') %>% 
  left_join(., puq, by="SEQN") %>% # pesticide use
  left_join(., hsq, by="SEQN") %>% # HIV
  left_join(., ohq, by="SEQN") %>%
  left_join(., ucf, by="SEQN") %>% 
  na.omit()

# subsample  ------------------------------------------------------------
# urine sample ####
set.seed(523652) 
nhdata_env = read.xport(here('nhanes/data/NHANES 2017-20/P_UM.XPT')) %>% 
  select(SEQN, WTSAPRP) %>% 
  mutate(inclusion = ifelse(WTSAPRP==0 | is.na(WTSAPRP), 0, 1)) %>% # environmental subsample A weights
  select(-WTSAPRP) %>% 
  left_join(fulldat_ext, ., by="SEQN") %>% 
  mutate(inclusion = replace_na(inclusion, 0),
         inclusion = as.factor(inclusion))

# joining with full data to sample 1500 
env = nhdata_env %>% 
  filter(inclusion == 1) %>%
  sample_n(., size=1500, replace=F)

# join back selected individuals with main frame
dat1 = left_join(fulldat_ext, env[,c("SEQN", 'inclusion')], by="SEQN") %>% 
  mutate(incl_env = case_when(inclusion == '1' ~ 1,
                              is.na(inclusion) ~ 0)) %>% 
  select(-inclusion)

# voc sample ####
nhdata_voc = read.xport(here('nhanes/data/NHANES 2017-20/P_VOCWB.XPT')) %>% 
  select(SEQN, WTSVOCPR) %>% 
  mutate(inclusion = ifelse(WTSVOCPR==0 | is.na(WTSVOCPR), 0, 1)) %>% 
  select(-WTSVOCPR) %>% 
  left_join(fulldat_ext, ., by="SEQN") %>% 
  mutate(inclusion = replace_na(inclusion, 0),
         inclusion = as.factor(inclusion)) 

# joining with full data
voc = nhdata_voc %>% 
  filter(inclusion == 1) %>%
  sample_n(., size=1500, replace=F)

dat2 = left_join(dat1, voc[,c("SEQN", 'inclusion')], by="SEQN") %>% 
  mutate(incl_voc = case_when(inclusion == '1' ~ 1,
                              is.na(inclusion) ~ 0)) %>% 
  select(-inclusion)


# dietary subsample ####
nhdata_dr2 = read.xport(here('nhanes/data/NHANES 2017-20/P_DR2TOT.XPT')) %>% 
  select(SEQN, WTDR2DPP) %>% 
  mutate(inclusion = ifelse(WTDR2DPP==0 | is.na(WTDR2DPP), 0, 1)) %>% 
  select(-WTDR2DPP) %>% 
  left_join(fulldat_ext, ., by="SEQN") %>% 
  mutate(inclusion = replace_na(inclusion, 0),
         inclusion = as.factor(inclusion))

# joining with full data
dr2 = nhdata_dr2  %>% 
  filter(inclusion == 1) %>%
  sample_n(., size=1500, replace=F)

dat3 = left_join(dat2, dr2[,c("SEQN", 'inclusion')], by="SEQN") %>% 
  mutate(incl_dr2 = case_when(inclusion == '1' ~ 1,
                              is.na(inclusion) ~ 0)) %>% 
  select(-inclusion)


# fasting subsample ####
nhdata_fas = read.xport(here('nhanes/data/NHANES 2017-20/P_TRIGLY.XPT')) %>% 
  select(SEQN, WTSAFPRP) %>% 
  mutate(inclusion = ifelse(WTSAFPRP==0 | is.na(WTSAFPRP), 0, 1)) %>% 
  select(-WTSAFPRP) %>% 
  left_join(fulldat_ext, ., by="SEQN") %>% 
  mutate(inclusion = replace_na(inclusion, 0),
         inclusion = as.factor(inclusion))

# joining with full data
fas = nhdata_fas %>% 
  filter(inclusion == 1) %>%
  sample_n(., size=1500, replace=F)

dat4 = left_join(dat3, fas[,c("SEQN", 'inclusion')], by="SEQN") %>% 
  mutate(incl_fas = case_when(inclusion == '1' ~ 1,
                              is.na(inclusion) ~ 0)) %>% 
  select(-inclusion)


names(dat4)


# weights -----------------------------------------------------------------
# creating weights for the population
nhdata = dat4

nhsub_fas = nhdata %>% 
  filter(incl_fas == 1) 

nhsub_env = nhdata %>% 
  filter(incl_env == 1) 

nhsub_dr2 = nhdata %>% 
  filter(incl_dr2 == 1) 

nhsub_voc = nhdata %>% 
  filter(incl_voc == 1) 

# bart --------------------------------------------------------------------
# env ---------------------------------------------------------------------
x_env = nhdata %>% 
  select(-c(SEQN, high_bp, names(nhdata)[grep('incl_*', names(nhdata))])) 

y_env = as.numeric(nhdata$incl_env)

nhsub2_env = nhsub_env %>% 
  select(-c(SEQN, high_bp, names(nhsub_env)[grep('incl_*', names(nhsub_env))])) 

bartfit_env = bart(x_env, y_env, keeptrees=T) 
bartpred_env = predict(bartfit_env, newdata = nhsub2_env)
bartpred_med_env = apply(bartpred_env, 2, median)

hist(bartpred_med_env)
hist(as.numeric(nhdata$incl_env))

# dr2 ---------------------------------------------------------------------
x_dr2 = nhdata %>% 
  select(-c(SEQN, high_bp, names(nhdata)[grep('incl_*', names(nhdata))])) 

y_dr2 = as.numeric(nhdata$incl_dr2)

nhsub2_dr2 = nhsub_dr2 %>% 
  select(-c(SEQN, high_bp, names(nhsub_dr2)[grep('incl_*', names(nhsub_dr2))])) 

bartfit_dr2 = bart(x_dr2, y_dr2, keeptrees=T) 
bartpred_dr2 = predict(bartfit_dr2, newdata = nhsub2_dr2)
bartpred_med_dr2 = apply(bartpred_dr2,2,median)

hist(bartpred_med_dr2)
hist(as.numeric(nhdata$incl_dr2))

# voc ---------------------------------------------------------------------
x_voc = nhdata %>% 
  select(-c(SEQN, high_bp, names(nhdata)[grep('incl_*', names(nhdata))])) 

y_voc = as.numeric(nhdata$incl_voc)

nhsub2_voc= nhsub_voc %>% 
  select(-c(SEQN, high_bp, names(nhsub_voc)[grep('incl_*', names(nhsub_voc))])) 

bartfit_voc = bart(x_voc, y_voc, keeptrees=T) 
bartpred_voc = predict(bartfit_voc, newdata = nhsub2_voc)
bartpred_med_voc = apply(bartpred_voc,2,median)

hist(bartpred_med_voc)
hist(as.numeric(nhdata$incl_voc))

# fas ---------------------------------------------------------------------
x_fas = nhdata %>% 
  select(-c(SEQN, high_bp, names(nhdata)[grep('incl_*', names(nhdata))])) 

y_fas = as.numeric(nhdata$incl_fas)

nhsub2_fas = nhsub_fas %>% 
  select(-c(SEQN, high_bp, names(nhsub_fas)[grep('incl_*', names(nhsub_fas))])) 

bartfit_fas = bart(x_fas, y_fas, keeptrees=T) 
bartpred_fas = predict(bartfit_fas, newdata = nhsub2_fas)
bartpred_med_fas = apply(bartpred_fas,2,median)

hist(bartpred_med_fas)
hist(as.numeric(nhdata$incl_fas))

nhdata_full = nhdata %>% 
  mutate(wts_env = ifelse(incl_env ==1, 1/bartpred_med_env, 0),
         wts_voc = ifelse(incl_voc ==1, 1/bartpred_med_voc, 0),
         wts_dr2 = ifelse(incl_dr2 ==1, 1/bartpred_med_dr2, 0),
         wts_fas = ifelse(incl_fas ==1, 1/bartpred_med_fas, 0))

saveRDS(nhdata_full, file=here("nhanes/data/nhdata_full.rds"))


# lasso -------------------------------------------------------------------
library(glmnet)

dat4 = readRDS(file=here("nhanes/data/nhdata_full.rds"))

# checking correlation with inclusion
corIncl = c(2:28)
dat4 = dat4 %>%  
  mutate(across(corIncl, as.factor))
m2 = model.matrix(~., dat4[corIncl], contrasts.arg = lapply(dat4[,corIncl], contrasts, contrasts=FALSE)) # making model matrix without a baseline category for the variables
corM2 = cor(m2[,-1])

View(abs(corM2[63:89,84:89])) # ranking the variables 


# model matrix for lasso
names(dat4)
dat5 = dat4 %>% 
  select(-c(pest_use)) %>% # after looking at correlation 
  select(-c(BMI_range, alc_exc)) # correlated with overweight
names(dat5)

covInd = c(2:21)[-c(6)] # removing high bp 
m = model.matrix(~ . -1, data = dat5[,covInd], contrasts.arg = lapply(dat5[,covInd], contrasts, contrasts=FALSE))
y = as.numeric(dat5[,7]) - 1 # high bp

# fixing foldids
nfolds = 15
foldid = 1 + (1:nrow(m) %% nfolds) # no need to specify seed if setting fold ID
ysel = cbind(y, dat5$incl_dr2, dat5$incl_fas, dat5$incl_voc, dat5$incl_env)
coef_mat = NULL
for(i in 1:ncol(ysel)){
  (lmb = cv.glmnet(m,ysel[,i],alpha = 1, family = "binomial", intercept=T, type.measure ="auc",foldid=foldid)$lambda.1se)
  coef_vec = glmnet(x=m, y=ysel[,i], family="binomial", intercept=T, type.measure = "auc", lambda = lmb) %>% coef(.)
  coef_mat = cbind(coef_mat, coef_vec)
}
colnames(coef_mat) = c('high_bp', 'dietary', 'fas', 'voc', 'env')
coef_mat


# removing ignorable variables --------------------------------------------
dat6 = dat5 %>% 
  select(-c(dep_severity,smk2_exp,potassium_lvl, den_vst, incl_env, incl_voc, wts_env, wts_voc))

names(dat6)

covInd = c(2:17)[-c(6)] # removing high bp 
m = model.matrix(~ . -1, data = dat6[,covInd], contrasts.arg = lapply(dat6[,covInd], contrasts, contrasts=FALSE))
y = as.numeric(dat6[,7]) - 1 # high bp

# fixing foldids
nfolds = 15
foldid = 1 + (1:nrow(m) %% nfolds) # no need to specify seed if setting fold ID
ysel = cbind(y, dat6$incl_dr2, dat6$incl_fas)
coef_mat = NULL
for(i in 1:ncol(ysel)){
  (lmb = cv.glmnet(m,ysel[,i],alpha = 1, family = "binomial", intercept=T, type.measure ="auc",foldid=foldid)$lambda.1se)
  coef_vec = glmnet(x=m, y=ysel[,i], family="binomial", intercept=T, type.measure = "auc", lambda = lmb) %>% coef(.)
  coef_mat = cbind(coef_mat, coef_vec)
}
colnames(coef_mat) = c('high_bp', 'dietary', 'fas')
coef_mat

sort(abs(coef_mat[,1]), decreasing=T)

saveRDS(dat6, file=here("nhanes/data/nhanes_final.rds"))

# assessing generated sample ---------------------------------------------
dat6 = readRDS(file=here("nhanes/data/nhanes_final.rds"))
source(here("nhanes/code/gen_samp.R"), echo=TRUE)

nhanes_gen = join1
names(nhanes_gen)

covInd = c(2:17)[-c(6)] # removing high bp 
m = model.matrix(~ . -1, data = nhanes_gen[,covInd], contrasts.arg = lapply(nhanes_gen[,covInd], contrasts, contrasts=FALSE))
y = as.numeric(nhanes_gen[,7]) - 1 # high bp

# fixing foldids
nfolds = 15
foldid = 1 + (1:nrow(m) %% nfolds) # no need to specify seed if setting fold ID
ysel = cbind(y, nhanes_gen$incl_dr2, nhanes_gen$incl_fas, nhanes_gen$incl_gen)
coef_mat = NULL
for(i in 1:ncol(ysel)){
  (lmb = cv.glmnet(m,ysel[,i],alpha = 1, family = "binomial", intercept=T, type.measure ="auc",foldid=foldid)$lambda.1se)
  coef_vec = glmnet(x=m, y=ysel[,i], family="binomial", intercept=T, type.measure = "auc", lambda = lmb) %>% coef(.)
  coef_mat = cbind(coef_mat, coef_vec)
}
colnames(coef_mat) = c('high_bp', 'dietary', 'fas', 'gen')
coef_mat

sort(abs(coef_mat[,1]), decreasing=T)

# weights for generated sample ------------------------------------------------------------------
nhsub_gen = nhanes_gen %>% 
  filter(incl_gen == 1) 

# popn
x_gen = nhanes_gen %>% 
  select(-c(SEQN, high_bp, names(nhanes_gen)[grep('incl_*', names(nhanes_gen))], 
            names(nhanes_gen)[grep('wts_*', names(nhanes_gen))])) 

y_gen = as.numeric(nhanes_gen$incl_gen)


# sample
nhsub2_gen = nhsub_gen %>% 
  select(-c(SEQN, high_bp, names(nhsub_gen)[grep('incl_*', names(nhsub_gen))], 
            names(nhanes_gen)[grep('wts_*', names(nhanes_gen))]))  

bartfit_gen = bart(x_gen, y_gen, keeptrees=T) 
bartpred_gen = predict(bartfit_gen, newdata = nhsub2_gen)
bartpred_med_gen = apply(bartpred_gen,2,median)

hist(bartpred_med_gen)
hist(as.numeric(nhanes_gen$incl_gen))

nhanes_gen_full = nhanes_gen %>% 
  mutate(wts_gen = ifelse(incl_gen ==1, 1/bartpred_med_gen, 0))

# checking counts using the weights -----------------------------------
nhanes_gen_full %>% 
  group_by(gender) %>% 
  summarise(sum_n = n(),
            sum_wts_fas = sum(wts_fas),
            sum_wts_dr2 = sum(wts_dr2),
            sum_wts_gen = sum(wts_gen))

nhanes_gen_full %>% 
  group_by(ethnicity) %>% 
  summarise(sum_n = n(),
            sum_wts_fas = sum(wts_fas),
            sum_wts_dr2 = sum(wts_dr2),
            sum_wts_gen = sum(wts_gen))

nhanes_gen_full %>% 
  group_by(age) %>% 
  summarise(sum_n = n(),
            sum_wts_fas = sum(wts_fas),
            sum_wts_dr2 = sum(wts_dr2),
            sum_wts_gen = sum(wts_gen))


saveRDS(nhanes_gen_full, file=here("nhanes/data/nhanes_final_gen.rds"))

# for tabulating 
test = data.frame(matrix(coef_mat, ncol=4)) 
rownames(test) = rownames(coef_mat)
colnames(test) = colnames(coef_mat)

test %>% 
  signif(., 3) 

  
