  ## Data cleaning
library(here)
library(foreign) # read.xport()
library(tidyverse)
library(questionr) # recode.na()
library(GGally) # ggpairs()
library(forcats) #fct_relevel
# https://www.aihw.gov.au/reports/risk-factors/risk-factors-to-health/contents/high-blood-pressure
# https://www.mayoclinic.org/diseases-conditions/high-blood-pressure/symptoms-causes/syc-20373410

## 2017-2020 
# demographic - age, race, sex/gender, education, married status ####
demo1720 = read.xport(here('nhanes/data/NHANES 2017-20/P_DEMO.XPT'))  %>% # 15560 obs total
  filter(RIDAGEYR >= 18) %>% # 10195 for 16 y/o and above, 9693 for 18 y/o 
  mutate(age = cut(RIDAGEYR, breaks = c(18, 29, 39, 49, 59, 69, 80), right = F, labels = F),
         age = recode(as.factor(age), '1' = '18-29', '2' = '30-39', '3' = '40-49', '4' = '50-59', '5' = '60-69', '6' = '70-80'),
         ethnicity = recode(as.factor(RIDRETH3), '1' = 'MexAme', '2' =  'OtherHisp', '3' = 'NonHispWhite', '4' = 'NonHispBlack', '6' = 'NonHispAsian', '7' = 'Other'),    #'1-Mexican American; 2-Other Hispanic; 3-Non-Hispanic White; 4-Non-Hispanic Black; 6-Non-Hispanic Asian; 7-Other Race including Multi-Racial'
         gender = recode(as.factor(RIAGENDR), '1' = 'male', '2' = 'female'),  # '1-male; 2-female
         DMDEDUC2 = recode.na(as.factor(DMDEDUC2), c('7', '9')), #1-Less than 9th grade; 2-9-11th grade (Includes 12th grade with no diploma);3-High school graduate/GED or equivalent;4-Some college or AA degree;5-College graduate or above; 7-Refused; 9-Don't Know,
         educ = recode(as.factor(DMDEDUC2), '1' = 'less9th', '2' = '9-11thgrade', '3' = 'HSgrad', '4' = 'SomecollegeAA', '5' = 'Collegegrad'),
         marital_status = recode(as.factor(DMDMARTZ), '1' = 'married/living tgt', '2' = 'widowed/separated', 
                                  '3' = 'never married'),
         marital_status = recode.na(as.factor(DMDMARTZ), c('77', '99'))) %>% 
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
            sodium_avg, potassium_avg))


# subsample
# urine sample
urn = read.xport(here('nhanes/data/NHANES 2017-20/P_UM.XPT')) %>% 
  select(SEQN, WTSAPRP)
nhdata = left_join(fulldat, urn, by='SEQN') %>%  # 2224 obs
  mutate(inclusion = ifelse(WTSAPRP==0 | is.na(WTSAPRP) , 0, 1)) %>% 
  select(-c(WTSAPRP, preg_yes)) %>% 
  na.omit()

# 'sample' using urine sample only
nhsub= left_join(urn, nhdata, by="SEQN") %>% 
  na.omit() %>%  
  mutate(n_row = nrow(.),
    wts = WTSAPRP/sum(WTSAPRP)*n_row)
                        

saveRDS(nhdata, file="nhanes/data/clean_data.rds")
saveRDS(nhsub, file="nhanes/data/nhsub.rds")


