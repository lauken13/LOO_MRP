library(brms)
library(survey) # svydesign()
library(here)
library(tidyverse)
nhfinal = readRDS(file="code/nhanes/data/nhanes_final.rds")

nhsub_dr2 = nhfinal %>% 
  filter(incl_dr2 == 1) 

nhsub_fas = nhfinal %>% 
  filter(incl_fas == 1) 

# dr2 subsample -------------------------------------------------------------------
# fitting bias, precision and other variables ~ 5 mins to run
nhmodel_allvar_dr2 = brm(high_bp ~  (1|age) + (1|ethnicity)  + phys_act + overweight + (1|diabetes) +
                           (1|marital_status) + trb_sleep + (1|educ) + (1|pov_level) +  (1|sodium_lvl) + 
                           (1|elst_status) + (1|hiv_test) + (1|urn_vol) + gender  +  smk_tobcig,
                         data = nhsub_dr2,
                         seed = 2048,
                         chain = 2,  
                         backend = "cmdstanr", silent = 2,
                         cores = 4,
                         family = bernoulli(link = "logit"),
                         control = list(adapt_delta = 0.99),
                         file=here('code/nhanes/data/dr2_allvar.rds'),
                         file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_dr2 = brm(high_bp ~  (1|age) + (1|ethnicity)  + phys_act + overweight + (1|diabetes) + 
                             (1|marital_status) + trb_sleep,
                           data = nhsub_dr2,
                           seed = 2048,
                           chain = 2,  
                           backend = "cmdstanr", silent = 2,
                           cores = 4,
                           family = bernoulli(link = "logit"),
                           control = list(adapt_delta = 0.98),
                           file=here('code/nhanes/data/dr2_biasprec.rds'),
                           file_refit="always")

# fitting bias only 
nhmodel_bias_dr2 = brm(high_bp ~  (1|age) + (1|ethnicity)  + phys_act + overweight + (1|diabetes),
                       data = nhsub_dr2,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.99),
                       file=here('code/nhanes/data/dr2_bias.rds'),
                       file_refit="always")

# fitting precision only 
nhmodel_prec_dr2 = brm(high_bp ~ (1|marital_status) + trb_sleep,
                       data = nhsub_dr2,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('code/nhanes/data/dr2_prec.rds'),
                       file_refit="always")

# inconsequential
nhmodel_inc_dr2 = brm(high_bp ~ (1|educ) + (1|pov_level) +  (1|sodium_lvl) + (1|elst_status) + (1|hiv_test) + 
                        (1|urn_vol),
                      data = nhsub_dr2,
                      seed = 2048,
                      chain = 2,  
                      backend = "cmdstanr", silent = 2,
                      cores = 4,
                      family = bernoulli(link = "logit"),
                      control = list(adapt_delta = 0.98), 
                      file=here('code/nhanes/data/dr2_inc.rds'),
                      file_refit="always")

# ignorable 
nhmodel_ign_dr2 = brm(high_bp ~ gender  +  smk_tobcig,
                      data = nhsub_dr2,
                      seed = 2048,
                      chain = 2,  
                      backend = "cmdstanr", silent = 2,
                      cores = 4,
                      family = bernoulli(link = "logit"),
                      control = list(adapt_delta = 0.99), 
                      file=here('code/nhanes/data/dr2_ign.rds'),
                      file_refit="always")

# fas subsample -----------------------------------------------------------
# fitting all var models ~ 4 mins to run
nhmodel_allvar_fas = brm(high_bp ~  (1|ethnicity) + overweight + (1|age) + (1|marital_status)  + phys_act  +
                            (1|diabetes) + trb_sleep + (1|elst_status) +  (1|urn_vol) +  gender +  
                           (1|educ) + (1|pov_level) + smk_tobcig +  (1|sodium_lvl) + (1|hiv_test) ,
                         data = nhsub_fas,
                         seed = 2048,
                         chain = 2,  
                         backend = "cmdstanr", silent = 2,
                         cores = 4,
                         family = bernoulli(link = "logit"),
                         control = list(adapt_delta = 0.99),
                         file=here('code/nhanes/data/fas_allvar.rds'),
                         file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_fas = brm(high_bp ~   (1|ethnicity) + overweight + (1|age) + (1|marital_status)  + phys_act  +
                            (1|diabetes) + trb_sleep,
                           data = nhsub_fas,
                           seed = 2048,
                           chain = 2,  
                           backend = "cmdstanr", silent = 2,
                           cores = 4,
                           family = bernoulli(link = "logit"),
                           control = list(adapt_delta = 0.98),
                           file=here('code/nhanes/data/fas_biasprec.rds'),
                           file_refit="always")

# fitting bias only 
nhmodel_bias_fas = brm(high_bp ~ (1|ethnicity) + overweight,
                       data = nhsub_fas,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('code/nhanes/data/fas_bias.rds'),
                       file_refit="always")

# fitting precision only 
nhmodel_prec_fas = brm(high_bp ~  (1|age) + (1|marital_status)  + phys_act + (1|diabetes) + trb_sleep,
                       data = nhsub_fas,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('code/nhanes/data/fas_prec.rds'),
                       file_refit="always")

# fitting inconsequential
nhmodel_inc_fas = brm(high_bp ~ (1|elst_status) +  (1|urn_vol),
                      data = nhsub_fas,
                      seed = 2048,
                      chain = 2,  
                      backend = "cmdstanr", silent = 2,
                      cores = 4,
                      family = bernoulli(link = "logit"),
                      control = list(adapt_delta = 0.995), 
                      file=here('code/nhanes/data/fas_inc.rds'),
                      file_refit="always")

# fitting ignorable
nhmodel_ign_fas = brm(high_bp ~ gender +  (1|educ) + (1|pov_level) + smk_tobcig +  (1|sodium_lvl) + (1|hiv_test),
                      data = nhsub_fas,
                      seed = 2048,
                      chain = 2,  
                      backend = "cmdstanr", silent = 2,
                      cores = 4,
                      family = bernoulli(link = "logit"),
                      control = list(adapt_delta = 0.99), 
                      file=here('code/nhanes/data/fas_ign.rds'),
                      file_refit="always")

# gen subsample -----------------------------------------------------------
# fitting all var models ~ 4 mins to run
nhmodel_allvar_gen = brm(high_bp ~  (1|ethnicity) + overweight + (1|age) + (1|marital_status)  + phys_act  +
                           (1|diabetes) + trb_sleep + (1|elst_status) +  (1|urn_vol) +  gender +  
                           (1|educ) + (1|pov_level) + smk_tobcig +  (1|sodium_lvl) + (1|hiv_test) ,
                         data = nhsub_gen,
                         seed = 2048,
                         chain = 2,  
                         backend = "cmdstanr", silent = 2,
                         cores = 4,
                         family = bernoulli(link = "logit"),
                         control = list(adapt_delta = 0.99),
                         file=here('code/nhanes/data/gen_allvar.rds'),
                         file_refit="always")


# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec_gen = brm(high_bp ~   (1|ethnicity) + overweight + (1|age) + (1|marital_status)  + phys_act  +
                             (1|diabetes) + trb_sleep,
                           data = nhsub_gen,
                           seed = 2048,
                           chain = 2,  
                           backend = "cmdstanr", silent = 2,
                           cores = 4,
                           family = bernoulli(link = "logit"),
                           control = list(adapt_delta = 0.99),
                           file=here('code/nhanes/data/gen_biasprec.rds'),
                           file_refit="always")

# fitting bias only 
nhmodel_bias_gen = brm(high_bp ~ (1|age) + (1|ethnicity) + overweight,
                       data = nhsub_gen,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('code/nhanes/data/gen_bias.rds'),
                       file_refit="always")

# fitting precision only 
nhmodel_prec_gen = brm(high_bp ~ (1|marital_status)  + phys_act + (1|diabetes) + trb_sleep,
                       data = nhsub_gen,
                       seed = 2048,
                       chain = 2,  
                       backend = "cmdstanr", silent = 2,
                       cores = 4,
                       family = bernoulli(link = "logit"),
                       control = list(adapt_delta = 0.98),
                       file=here('code/nhanes/data/gen_prec.rds'),
                       file_refit="always")

# fitting inconsequential
nhmodel_inc_gen = brm(high_bp ~ (1|elst_status) +  gender + (1|pov_level) + (1|sodium_lvl),
                      data = nhsub_gen,
                      seed = 2048,
                      chain = 2,  
                      backend = "cmdstanr", silent = 2,
                      cores = 4,
                      family = bernoulli(link = "logit"),
                      control = list(adapt_delta = 0.995), 
                      file=here('code/nhanes/data/gen_inc.rds'),
                      file_refit="always")

# fitting ignorable
nhmodel_ign_gen = brm(high_bp ~ (1|educ) + (1|urn_vol) + smk_tobcig + (1|hiv_test),
                      data = nhsub_gen,
                      seed = 2048,
                      chain = 2,  
                      backend = "cmdstanr", silent = 2,
                      cores = 4,
                      family = bernoulli(link = "logit"),
                      control = list(adapt_delta = 0.99), 
                      file=here('code/nhanes/data/gen_ign.rds'),
                      file_refit="always")



