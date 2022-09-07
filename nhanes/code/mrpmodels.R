library(brms)
library(survey) # svydesign()
source(here::here('02-super popn approach/functions.R'))
nhdata = readRDS(file="nhanes/data/clean_data.rds")
nhsub = readRDS(file="nhanes/data/nhsub.rds") 

# fitting bias and precision models ~ 4 mins to run
nhmodel_biasprec = brm(high_bp ~  gender + overweight + trb_sleep + smk2_exp + smk_tobcig + 
                         (1|age) + (1|alc_exc) + (1|BMI_range) + (1|dep_severity) + (1|educ) +  
                         (1|ethnicity) + (1|marital_status) + (1|potassium_lvl) + (1|pov_level) + (1|sodium_lvl) + 
                         phys_act + (1|diabetes),
                      data = nhsub,
                      seed = 2048,
                      chain = 2,  
                      backend = "cmdstanr", silent = 2,
                      cores = 4,
                      family = bernoulli(link = "logit"),
                      control = list(adapt_delta = 0.9), 
                      file=here('nhanes/data/nhmodel_biasprec.rds'),
                      file_refit="always")

# fitting bias only 
nhmodel_bias = brm(high_bp ~  gender + overweight + trb_sleep + smk2_exp + smk_tobcig + 
                     (1|age) + (1|alc_exc) + (1|BMI_range) + (1|dep_severity) + (1|educ) +  
                     (1|ethnicity) + (1|marital_status) + (1|potassium_lvl) + (1|pov_level) + (1|sodium_lvl),
                   data = nhsub,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.9),
                   file=here('nhanes/data/nhmodel_bias.rds'),
                   file_refit="always")

# fitting precision only 
nhmodel_prec = brm(high_bp ~ phys_act + (1|diabetes),
                   data = nhsub,
                   seed = 2048,
                   chain = 2,  
                   backend = "cmdstanr", silent = 2,
                   cores = 4,
                   family = bernoulli(link = "logit"),
                   control = list(adapt_delta = 0.9),
                   file=here('nhanes/data/nhmodel_prec.rds'),
                   file_refit="always")
