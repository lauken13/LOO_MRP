library(brms)
library(survey) # svydesign()
library(here)
library(tidyverse)
source(here('02-super popn approach/functions.R'))
nhdata_full = readRDS(file="nhanes/data/nhdata_full.rds")

nhsub_urn = nhdata_full %>% 
  filter(incl_urn == 1) 

nhsub_dr1 = nhdata_full %>% 
  filter(incl_dr1 == 1) 

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
nhmodel_biasprec_voc = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                         overweight + (1|dep_severity) + (1|BMI_range) + (1|alc_exc) + (1|pov_level) +
                         smk2_exp +  (1|sodium_lvl) + phys_act + (1|diabetes) + (1|potassium_lvl) + 
                         trb_sleep,
                       data = nhsub_voc,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.95),
                       file=here('nhanes/data/nhmodel_biasprec_voc.rds'),
                       file_refit="always")

# fitting bias only 
nhmodel_bias_voc = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                     overweight + (1|dep_severity) + (1|BMI_range) + (1|alc_exc)  + (1|pov_level) + 
                     smk2_exp +  (1|sodium_lvl) + phys_act + (1|diabetes)  + (1|potassium_lvl) ,
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
nhmodel_prec_voc = brm(high_bp ~ trb_sleep,
                   data = nhsub_voc,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98),
                   file=here('nhanes/data/nhmodel_prec_voc.rds'),
                   file_refit="always")

# fitting none of the important variables
nhmodel_none_voc = brm(high_bp ~ smk_tobcig,
                   data = nhsub_voc,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98), 
                   file=here('nhanes/data/nhmodel_none_voc.rds'),
                   file_refit="always")

# dr1 subsample -------------------------------------------------------------------
# fitting bias, precision and other variables ~ 5 mins to run
nhmodel_allvar_dr1 = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                       overweight + (1|dep_severity) + (1|BMI_range) + (1|alc_exc) + trb_sleep +
                       (1|pov_level) + smk2_exp +  (1|sodium_lvl) + phys_act + (1|diabetes) +
                       (1|potassium_lvl) + smk_tobcig,
                     data = nhsub_dr1,
                     seed = 2048,
                     chain = 2,  
                     backend = "cmdstanr", silent = 2,
                     cores = 4,
                     family = bernoulli(link = "logit"),
                     control = list(adapt_delta = 0.99),
                     file=here('nhanes/data/nhmodel_allvar_dr1.rds'),
                     file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_dr1 = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                         overweight + (1|dep_severity) + (1|BMI_range) + (1|alc_exc) + trb_sleep +
                         (1|pov_level) + smk2_exp +  (1|sodium_lvl) + phys_act + (1|diabetes) +
                         (1|potassium_lvl),
                       data = nhsub_dr1,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/nhmodel_biasprec_dr1.rds'),
                       file_refit="always")

# fitting bias only 
nhmodel_bias_dr1 = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                     overweight + (1|dep_severity) + (1|BMI_range) + (1|alc_exc) + trb_sleep +
                     (1|pov_level) + smk2_exp +  (1|sodium_lvl) + phys_act + (1|diabetes),
                   data = nhsub_dr1,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98),
                   file=here('nhanes/data/nhmodel_bias_dr1.rds'),
                   file_refit="always")

# fitting precision only 
nhmodel_prec_dr1 = brm(high_bp ~ (1|potassium_lvl),
                   data = nhsub_dr1,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98),
                   file=here('nhanes/data/nhmodel_prec_dr1.rds'),
                   file_refit="always")

# fitting non of bias and precision models 
nhmodel_none_dr1 = brm(high_bp ~ smk_tobcig,
                   data = nhsub_dr1,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98), 
                   file=here('nhanes/data/nhmodel_none_dr1.rds'),
                   file_refit="always")



# urn subsample -----------------------------------------------------------
# fitting bias and precision models ~ 4 mins to run
nhmodel_allvar_urn = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) +  overweight + 
                       (1|dep_severity) + (1|BMI_range) + (1|alc_exc) + (1|pov_level) +  smk2_exp +
                       (1|sodium_lvl)  + (1|potassium_lvl) + (1|marital_status) + phys_act + (1|diabetes) + 
                       trb_sleep + smk_tobcig,
                     data = nhsub_urn,
                     seed = 2048,
                     chain = 2,  
                     backend = "cmdstanr", silent = 2,
                     cores = 4,
                     family = bernoulli(link = "logit"),
                     control = list(adapt_delta = 0.97),
                     file=here('nhanes/data/nhmodel_allvar_urn.rds'),
                     file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_urn = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) + 
                         overweight + (1|dep_severity) + (1|BMI_range) + (1|alc_exc) + (1|diabetes) +
                         (1|pov_level) +  smk2_exp +  (1|sodium_lvl)  + (1|potassium_lvl) + phys_act + 
                         trb_sleep ,
                       data = nhsub_urn,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('nhanes/data/nhmodel_biasprec_urn.rds'),
                       file_refit="always")

# fitting bias only 
nhmodel_bias_urn = brm(high_bp ~ (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) + 
                     overweight + (1|dep_severity) + (1|BMI_range) + (1|alc_exc) + (1|diabetes) +
                     (1|pov_level) +  smk2_exp +  (1|sodium_lvl)  + (1|potassium_lvl),
                   data = nhsub_urn,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.95),
                   file=here('nhanes/data/nhmodel_bias_urn.rds'),
                   file_refit="always")

# fitting precision only 
nhmodel_prec_urn = brm(high_bp ~ phys_act + trb_sleep ,
                   data = nhsub_urn,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.95),
                   file=here('nhanes/data/nhmodel_prec_urn.rds'),
                   file_refit="always")

# fitting non of bias and precision models 
nhmodel_none_urn = brm(high_bp ~ smk_tobcig,
                   data = nhsub_urn,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98), 
                   file=here('nhanes/data/nhmodel_none_urn.rds'),
                   file_refit="always")


# fas subsample -----------------------------------------------------------
# fitting all var models ~ 4 mins to run
nhmodel_allvar = brm(high_bp ~  (1|age) + (1|ethnicity)  + (1|educ) + phys_act + (1|dep_severity) +
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
nhmodel_biasprec = brm(high_bp ~  (1|ethnicity) + gender + (1|educ) + phys_act + 
                         overweight + (1|dep_severity) + (1|BMI_range) + (1|alc_exc)  + (1|pov_level) +
                         smk2_exp  + (1|sodium_lvl) + (1|age) + (1|marital_status) + (1|diabetes) + 
                         trb_sleep + (1|potassium_lvl),
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
nhmodel_bias_fas = brm(high_bp ~  (1|age) + (1|ethnicity) + gender + (1|educ) + (1|marital_status) +
                     phys_act + overweight +  (1|dep_severity) + (1|BMI_range) + (1|alc_exc)  + 
                     (1|pov_level) + smk2_exp  + (1|sodium_lvl) + (1|potassium_lvl),
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
nhmodel_prec_fas = brm(high_bp ~  (1|diabetes) + trb_sleep ,
                   data = nhsub_fas,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98),
                   file=here('nhanes/data/nhmodel_prec_fas.rds'),
                   file_refit="always")

# fitting non of bias and precision models 
nhmodel_none = brm(high_bp ~ smk_tobcig,
                   data = nhsub_fas,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.98), 
                   file=here('nhanes/data/nhmodel_none_fas.rds'),
                   file_refit="always")

