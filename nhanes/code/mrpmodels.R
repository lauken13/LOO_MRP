library(brms)
library(survey) # svydesign()
library(here)
library(tidyverse)
source(here('02-super popn approach/functions.R'))
nhdata_full = readRDS(file="nhanes/data/nhdata_full.rds")

nhsub_env = nhdata_full %>% 
  filter(incl_env == 1) 

nhsub_dr2 = nhdata_full %>% 
  filter(incl_dr2 == 1) 

nhsub_voc = nhdata_full %>% 
  filter(incl_voc == 1) 

nhsub_fas = nhdata_full %>% 
  filter(incl_fas == 1) 

# voc ---------------------------------------------------------------------
# fitting bias, precision and other variables ~ 5 mins to run
nhmodel_allvar_voc = brm(high_bp ~ (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                       overweight + (1|dep_severity) + (1|BMI_range) + (1|alc_exc) + trb_sleep +
                       (1|pov_level) + smk2_exp +  (1|sodium_lvl) + phys_act + (1|diabetes) +
                       (1|potassium_lvl) + smk_tobcig,
                     data = nhsub_voc,
                     seed = 2048,
                     chain = 2,  
                     backend = "cmdstanr", silent = 2,
                     cores = 4,
                     family = bernoulli(link = "logit"),
                     control = list(adapt_delta = 0.97),
                     file=here('nhanes/data/nhmodel_allvar_voc.rds'),
                     file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_voc = brm(high_bp ~  (1|age) + (1|ethnicity) + (1|diabetes) + (1|marital_status) + phys_act + 
                             overweight + (1|BMI_range) + trb_sleep,
                       data = nhsub_voc,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.97),
                       file=here('nhanes/data/nhmodel_biasprec_voc.rds'),
                       file_refit="always")

# fitting bias only 
nhmodel_bias_voc = brm(high_bp ~   (1|age) + (1|ethnicity),
                   data = nhsub_voc,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98),
                   file=here('nhanes/data/nhmodel_bias_voc.rds'),
                   file_refit="always")

# fitting precision only 
nhmodel_prec_voc = brm(high_bp ~ (1|marital_status) + phys_act +  overweight + (1|BMI_range)+ (1|diabetes) +
                         trb_sleep,
                   data = nhsub_voc,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.99),
                   file=here('nhanes/data/nhmodel_prec_voc.rds'),
                   file_refit="always")

# inconsequential
nhmodel_inc_voc = brm(high_bp ~ (1|educ) + (1|alc_exc),
                   data = nhsub_voc,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98), 
                   file=here('nhanes/data/nhmodel_inc_voc.rds'),
                   file_refit="always")

# ignorable
nhmodel_ign_voc = brm(high_bp ~ gender + (1|pov_level) + smk2_exp + smk_tobcig + (1|dep_severity) +
                        (1|sodium_lvl) + (1|potassium_lvl),
                       data = nhsub_voc,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98), 
                       file=here('nhanes/data/nhmodel_ign_voc.rds'),
                       file_refit="always")

# dr2 subsample -------------------------------------------------------------------
# fitting bias, precision and other variables ~ 5 mins to run
nhmodel_allvar_dr2 = brm(high_bp ~  (1|age) +  (1|BMI_range) + (1|ethnicity) + (1|marital_status) + phys_act + 
                           overweight + (1|diabetes)  + trb_sleep +  (1|sodium_lvl) + (1|potassium_lvl) + 
                           gender + (1|educ) + (1|alc_exc) +  (1|pov_level) + smk2_exp + 
                           smk_tobcig + (1|dep_severity),
                     data = nhsub_dr2,
                     seed = 2048,
                     chain = 2,  
                     backend = "cmdstanr", silent = 2,
                     cores = 4,
                     family = bernoulli(link = "logit"),
                     control = list(adapt_delta = 0.99),
                     file=here('nhanes/data/nhmodel_allvar_dr2.rds'),
                     file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_dr2 = brm(high_bp ~ (1|age) +  (1|BMI_range) + (1|ethnicity) + (1|marital_status) + phys_act + 
                             overweight + (1|diabetes)  + trb_sleep,
                       data = nhsub_dr2,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/nhmodel_biasprec_dr2.rds'),
                       file_refit="always")

# fitting bias only 
nhmodel_bias_dr2 = brm(high_bp ~ (1|age) +  (1|BMI_range),
                   data = nhsub_dr2,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98),
                   file=here('nhanes/data/nhmodel_bias_dr2.rds'),
                   file_refit="always")

# fitting precision only 
nhmodel_prec_dr2 = brm(high_bp ~ (1|ethnicity) + (1|marital_status) + phys_act +  overweight + (1|diabetes)  + 
                         trb_sleep,
                   data = nhsub_dr2,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98),
                   file=here('nhanes/data/nhmodel_prec_dr2.rds'),
                   file_refit="always")

# inconsequential
nhmodel_inc_dr2 = brm(high_bp ~ (1|sodium_lvl) + (1|potassium_lvl),
                   data = nhsub_dr2,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98), 
                   file=here('nhanes/data/nhmodel_inc_dr2.rds'),
                   file_refit="always")

# ignorable 
nhmodel_ign_dr2 = brm(high_bp ~   gender + (1|educ) + (1|alc_exc) + (1|pov_level) + smk2_exp + 
                        smk_tobcig +(1|dep_severity) ,
                       data = nhsub_dr2,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.99), 
                       file=here('nhanes/data/nhmodel_ign_dr2.rds'),
                       file_refit="always")

# env subsample -----------------------------------------------------------
# fitting bias and precision models ~ 4 mins to run
nhmodel_allvar_env = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) +  overweight + 
                       (1|dep_severity) + (1|BMI_range) + (1|alc_exc) + (1|pov_level) +  smk2_exp +
                       (1|sodium_lvl)  + (1|potassium_lvl) + (1|marital_status) + phys_act + (1|diabetes) + 
                       trb_sleep + smk_tobcig,
                     data = nhsub_env,
                     seed = 2048,
                     chain = 2,  
                     backend = "cmdstanr", silent = 2,
                     cores = 4,
                     family = bernoulli(link = "logit"),
                     control = list(adapt_delta = 0.97),
                     file=here('nhanes/data/nhmodel_allvar_env.rds'),
                     file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_env = brm(high_bp ~ (1|age) + (1|ethnicity) + (1|marital_status) + phys_act + overweight + 
                             (1|BMI_range) + (1|diabetes) + trb_sleep,
                       data = nhsub_env,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/nhmodel_biasprec_env.rds'),
                       file_refit="always")

# fitting bias only 
nhmodel_bias_env = brm(high_bp ~ 1,
                   data = nhsub_env,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.95),
                   file=here('nhanes/data/nhmodel_bias_env.rds'),
                   file_refit="always")

# fitting precision only 
nhmodel_prec_env = brm(high_bp ~ (1|age) + (1|ethnicity) + (1|marital_status) + phys_act + overweight + 
                         (1|BMI_range) + (1|diabetes) + trb_sleep,
                   data = nhsub_env,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98),
                   file=here('nhanes/data/nhmodel_prec_env.rds'),
                   file_refit="always")

# inconsequential
nhmodel_inc_env = brm(high_bp ~ (1|educ),
                   data = nhsub_env,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98), 
                   file=here('nhanes/data/nhmodel_inc_env.rds'),
                   file_refit="always")

# ignorable
nhmodel_ign_env = brm(high_bp ~ gender + (1|dep_severity) + (1|alc_exc)  +  (1|pov_level) + smk2_exp + 
                        smk_tobcig + (1|sodium_lvl) + (1|potassium_lvl),
                      data = nhsub_env,
                      seed = 2048,
                      chain = 2,  
                      backend = "cmdstanr", silent = 2,
                      cores = 4,
                      family = bernoulli(link = "logit"),
                      control = list(adapt_delta = 0.98), 
                      file=here('nhanes/data/nhmodel_ign_env.rds'),
                      file_refit="always")

# fas subsample -----------------------------------------------------------
# fitting all var models ~ 4 mins to run
nhmodel_allvar_fas = brm(high_bp ~  (1|age) + (1|ethnicity)  + (1|educ) + phys_act + (1|dep_severity) +
                       (1|BMI_range) + (1|alc_exc)  + smk2_exp  + (1|potassium_lvl) + gender + 
                       (1|marital_status) + overweight  + (1|diabetes) + trb_sleep  + (1|pov_level) + 
                       (1|sodium_lvl) + smk_tobcig,
                     data = nhsub_fas,
                     seed = 2048,
                     chain = 2,  
                     backend = "cmdstanr", silent = 2,
                     cores = 4,
                     family = bernoulli(link = "logit"),
                     control = list(adapt_delta = 0.97),
                     file=here('nhanes/data/nhmodel_allvar_fas.rds'),
                     file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_fas = brm(high_bp ~  (1|age) +  (1|ethnicity) + (1|BMI_range) + (1|marital_status) + phys_act + 
                         overweight  + (1|diabetes) +  trb_sleep,
                       data = nhsub_fas,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/nhmodel_biasprec_fas.rds'),
                       file_refit="always")

# fitting bias only 
nhmodel_bias_fas = brm(high_bp ~   (1|age),
                   data = nhsub_fas,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98),
                   file=here('nhanes/data/nhmodel_bias_fas.rds'),
                   file_refit="always")

# fitting precision only 
nhmodel_prec_fas = brm(high_bp ~  (1|marital_status) + phys_act + overweight  + (1|diabetes) +  trb_sleep +
                         (1|ethnicity) + (1|BMI_range),
                   data = nhsub_fas,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98),
                   file=here('nhanes/data/nhmodel_prec_fas.rds'),
                   file_refit="always")

# fitting inconsequential
nhmodel_inc_fas = brm(high_bp ~ 1,
                      data = nhsub_fas,
                      seed = 2048,
                      chain = 2,  
                      backend = "cmdstanr", silent = 2,
                      cores = 4,
                      family = bernoulli(link = "logit"),
                      control = list(adapt_delta = 0.98), 
                      file=here('nhanes/data/nhmodel_inc_fas.rds'),
                      file_refit="always")

# fitting ignorable
nhmodel_ign_fas = brm(high_bp ~ gender + (1|educ) + (1|dep_severity) + (1|alc_exc)  + (1|pov_level) +
                        smk2_exp + smk_tobcig + (1|sodium_lvl) + (1|potassium_lvl),
                   data = nhsub_fas,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98), 
                   file=here('nhanes/data/nhmodel_ign_fas.rds'),
                   file_refit="always")

