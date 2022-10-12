library(brms)
library(survey) # svydesign()
library(here)
library(tidyverse)
source(here('02-super popn approach/functions.R'))
nhnew = readRDS(file="nhanes/data/nhnew.rds")

nhsub_urn = nhnew %>% 
  filter(incl_urn == 1) 

nhsub_dr1 = nhnew %>% 
  filter(incl_dr1 == 1) 

nhsub_voc = nhnew %>% 
  filter(incl_voc == 1) 

nhsub_fas = nhnew %>% 
  filter(incl_fas == 1) 

# voc ---------------------------------------------------------------------
# fitting bias, precision and other variables ~ 5 mins to run
nhmodel_allvar_voc = brm(high_bp ~ (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                           overweight + (1|dep_severity) + (1|BMI_range) + (1|alc_exc) + trb_sleep +
                           (1|pov_level) + smk2_exp +  (1|sodium_lvl) + phys_act + (1|diabetes) +
                           (1|potassium_lvl) + smk_tobcig + (1|elst_status) + (1|pest_use) + hepA_ind +
                           hepB_ind + (1|hiv_ind) + asth_ind,
                         data = nhsub_voc,
                         seed = 2048,
                         chain = 2,  
                         backend = "cmdstanr", silent = 2,
                         cores = 4,
                         family = bernoulli(link = "logit"),
                         control = list(adapt_delta = 0.97),
                         file=here('nhanes/data/voc_allvar.rds'),
                         file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_voc = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                             phys_act + (1|BMI_range) + (1|alc_exc) + (1|diabetes) + (1|pov_level) + 
                            (1|sodium_lvl) + (1|pest_use) + hepA_ind + hepB_ind + asth_ind +
                             overweight + trb_sleep,
                           data = nhsub_voc,
                           seed = 2048,
                           chain = 2,  
                           backend = "cmdstanr", silent = 2,
                           cores = 4,
                           family = bernoulli(link = "logit"),
                           control = list(adapt_delta = 0.95),
                           file=here('nhanes/data/voc_biasprec.rds'),
                           file_refit="always")

# fitting bias only 
nhmodel_bias_voc = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                         phys_act + (1|BMI_range) + (1|alc_exc) + (1|diabetes) + (1|pov_level) + 
                         (1|sodium_lvl) + (1|pest_use) + hepA_ind + hepB_ind + asth_ind,
                       data = nhsub_voc,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/voc_bias.rds'),
                       file_refit="always")

# fitting precision only 
nhmodel_prec_voc = brm(high_bp ~ overweight + trb_sleep,
                       data = nhsub_voc,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/voc_prec.rds'),
                       file_refit="always")

# fitting none of the important variables
nhmodel_none_voc = brm(high_bp ~ smk_tobcig + (1|potassium_lvl) + (1|dep_severity) + smk2_exp + (1|elst_status) +
                         (1|hiv_ind),
                       data = nhsub_voc,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98), 
                       file=here('nhanes/data/voc_none.rds'),
                       file_refit="always")

# dr1 subsample -------------------------------------------------------------------
# fitting bias, precision and other variables ~ 5 mins to run
nhmodel_allvar_dr1 = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                           overweight  + (1|BMI_range) + (1|alc_exc) + trb_sleep + (1|pov_level) +
                           (1|sodium_lvl) + phys_act + (1|diabetes)   + (1|pest_use) + hepA_ind +
                           hepB_ind  + asth_ind + (1|dep_severity) + smk2_exp  + smk_tobcig +
                           (1|potassium_lvl) + (1|elst_status) + (1|hiv_ind),
                         data = nhsub_dr1,
                         seed = 2048,
                         chain = 2,  
                         backend = "cmdstanr", silent = 2,
                         cores = 4,
                         family = bernoulli(link = "logit"),
                         control = list(adapt_delta = 0.99),
                         file=here('nhanes/data/dr1_allvar.rds'),
                         file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_dr1 = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                             overweight  + (1|BMI_range) + (1|alc_exc) + trb_sleep + (1|pov_level) +
                             (1|sodium_lvl) + phys_act + (1|diabetes)   + (1|pest_use) + hepA_ind +
                             hepB_ind  + asth_ind + (1|dep_severity) + smk2_exp  + smk_tobcig +
                             (1|potassium_lvl) + (1|elst_status) + (1|hiv_ind),
                           data = nhsub_dr1,
                           seed = 2048,
                           chain = 2,  
                           backend = "cmdstanr", silent = 2,
                           cores = 4,
                           family = bernoulli(link = "logit"),
                           control = list(adapt_delta = 0.98),
                           file=here('nhanes/data/dr1_biasprec.rds'),
                           file_refit="always")

# fitting bias only 
nhmodel_bias_dr1 = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                         overweight  + (1|BMI_range) + (1|alc_exc) + trb_sleep + (1|pov_level) +
                         (1|sodium_lvl) + phys_act + (1|diabetes)   + (1|pest_use) + hepA_ind +
                         hepB_ind  + asth_ind,
                       data = nhsub_dr1,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/dr1_bias.rds'),
                       file_refit="always")

# fitting precision only 
nhmodel_prec_dr1 = brm(high_bp ~  (1|dep_severity) + smk2_exp  + smk_tobcig +  (1|potassium_lvl) + (1|elst_status) + 
                         (1|hiv_ind),
                       data = nhsub_dr1,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/dr1_prec.rds'),
                       file_refit="always")

# fitting none of the variables
nhmodel_none_dr1 = brm(high_bp ~ 1,
                       data = nhsub_dr1,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/dr1_none.rds'),
                       file_refit="always")

# urn subsample -----------------------------------------------------------
# fitting bias and precision models ~ 4 mins to run
nhmodel_allvar_urn = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                           overweight + (1|BMI_range)  +  (1|sodium_lvl)  + (1|diabetes)  + (1|pest_use) + 
                           hepA_ind + hepB_ind  + asth_ind + phys_act + (1|alc_exc) + 
                           trb_sleep  + (1|pov_level) + (1|dep_severity)+ (1|hiv_ind)  + smk2_exp + 
                           smk_tobcig + (1|potassium_lvl)  + (1|elst_status) ,
                         data = nhsub_urn,
                         seed = 2048,
                         chain = 2,  
                         backend = "cmdstanr", silent = 2,
                         cores = 4,
                         family = bernoulli(link = "logit"),
                         control = list(adapt_delta = 0.97),
                         file=here('nhanes/data/urn_allvar.rds'),
                         file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_urn = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                             overweight + (1|BMI_range)  +  (1|sodium_lvl)  + (1|diabetes)  + (1|pest_use) + 
                             hepA_ind + hepB_ind  + asth_ind + phys_act + (1|alc_exc) + 
                             trb_sleep  + (1|pov_level),
                           data = nhsub_urn,
                           seed = 2048,
                           chain = 2,  
                           backend = "cmdstanr", silent = 2,
                           cores = 4,
                           family = bernoulli(link = "logit"),
                           control = list(adapt_delta = 0.98),
                           file=here('nhanes/data/urn_biasprec.rds'),
                           file_refit="always")

# fitting bias only 
nhmodel_bias_urn = brm(high_bp ~ (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                         overweight + (1|BMI_range)  +  (1|sodium_lvl)  + (1|diabetes)  + (1|pest_use) + 
                         hepA_ind + hepB_ind  + asth_ind,
                       data = nhsub_urn,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.95),
                       file=here('nhanes/data/urn_bias.rds'),
                       file_refit="always")

# fitting precision only 
nhmodel_prec_urn = brm(high_bp ~ phys_act + (1|alc_exc) + trb_sleep  + (1|pov_level),
                       data = nhsub_urn,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.95),
                       file=here('nhanes/data/urn_prec.rds'),
                       file_refit="always")

# fitting non of bias and precision models 
nhmodel_none_urn = brm(high_bp ~  (1|dep_severity)+ (1|hiv_ind)  + smk2_exp +  smk_tobcig + (1|potassium_lvl)  + 
                         (1|elst_status) ,
                       data = nhsub_urn,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98), 
                       file=here('nhanes/data/urn_none.rds'),
                       file_refit="always")


# fas subsample -----------------------------------------------------------
# fitting all var models ~ 4 mins to run
nhmodel_allvar = brm(high_bp ~  (1|ethnicity) + gender + (1|educ) + phys_act + (1|BMI_range) + 
                       (1|alc_exc)  + (1|diabetes) + (1|pov_level) + (1|sodium_lvl) + (1|pest_use) + 
                       hepA_ind + hepB_ind  + asth_ind + (1|age) + (1|marital_status)  +
                       overweight + trb_sleep  + (1|dep_severity)  + (1|potassium_lvl) + (1|elst_status) +
                       smk2_exp + smk_tobcig + (1|hiv_ind),
                     data = nhsub_fas,
                     seed = 2048,
                     chain = 2,  
                     backend = "cmdstanr", silent = 2,
                     cores = 4,
                     family = bernoulli(link = "logit"),
                     control = list(adapt_delta = 0.97),
                     file=here('nhanes/data/fas_allvar.rds'),
                     file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec = brm(high_bp ~  (1|ethnicity) + gender + (1|educ) + phys_act + (1|BMI_range) + 
                         (1|alc_exc)  + (1|diabetes) + (1|pov_level) + (1|sodium_lvl) + (1|pest_use) + 
                         hepA_ind + hepB_ind  + asth_ind + (1|age) + (1|marital_status)  +
                         overweight + trb_sleep ,
                       data = nhsub_fas,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/fas_biasprec.rds'),
                       file_refit="always")

# fitting bias only 
nhmodel_bias_fas = brm(high_bp ~  (1|ethnicity) + gender + (1|educ) + phys_act + (1|BMI_range) + 
                         (1|alc_exc)  + (1|diabetes) + (1|pov_level) + (1|sodium_lvl) + (1|pest_use) + 
                         hepA_ind + hepB_ind  + asth_ind,
                       data = nhsub_fas,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/fas_bias.rds'),
                       file_refit="always")

# fitting precision only 
nhmodel_prec_fas = brm(high_bp ~  (1|age) + (1|marital_status) + overweight + trb_sleep ,
                       data = nhsub_fas,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/fas_prec.rds'),
                       file_refit="always")

# fitting non of bias and precision models 
nhmodel_none = brm(high_bp ~ (1|dep_severity)  + (1|potassium_lvl) + (1|elst_status) + smk2_exp + smk_tobcig +
                     (1|hiv_ind),
                   data = nhsub_fas,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98), 
                   file=here('nhanes/data/fas_none.rds'),
                   file_refit="always")


