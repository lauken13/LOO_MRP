## plotting the diff subsample
library(here)
library(loo)
library(tidyverse)
library(brms)
library(survey)
source(here('02-super popn approach/functions.R'))

# reading in data
nhdata_full = readRDS(file="nhanes/data/nhdata_full.rds")

nhsub_fas = nhdata_full %>% 
  filter(incl_fas == 1) %>% 
  select(-c(wts_env, wts_voc, wts_dr2,
            incl_env, incl_voc, incl_dr2))

nhsub_env = nhdata_full %>% 
  filter(incl_env == 1) %>% 
  select(-c(wts_fas, wts_voc, wts_dr2,
            incl_fas, incl_voc, incl_dr2))

nhsub_dr2 = nhdata_full %>% 
  filter(incl_dr2 == 1) %>% 
  select(-c(wts_env, wts_voc, wts_fas,
            incl_env, incl_voc, incl_fas))

nhsub_voc = nhdata_full %>% 
  filter(incl_voc == 1) %>% 
  select(-c(wts_env, wts_fas, wts_dr2,
            incl_env, incl_fas, incl_dr2))


# VOC ---------------------------------------------------------------------
nhmodel_allvar_voc = readRDS(file=here('nhanes/data/nhmodel_allvar_voc.rds'))
nhmodel_biasprec_voc = readRDS(file=here('nhanes/data/nhmodel_biasprec_voc.rds'))
nhmodel_bias_voc = readRDS(file=here('nhanes/data/nhmodel_bias_voc.rds'))
nhmodel_prec_voc = readRDS(file=here('nhanes/data/nhmodel_prec_voc.rds'))
nhmodel_inc_voc = readRDS(file=here('nhanes/data/nhmodel_inc_voc.rds'))
nhmodel_ign_voc = readRDS(file=here('nhanes/data/nhmodel_ign_voc.rds'))

loo_allvar_voc = loo(nhmodel_allvar_voc)
loo_biasprec_voc = loo(nhmodel_biasprec_voc)
loo_bias_voc = loo(nhmodel_bias_voc)
loo_prec_voc = loo(nhmodel_prec_voc)
loo_inc_voc = loo(nhmodel_inc_voc)
loo_ign_voc = loo(nhmodel_ign_voc)


loo_all_voc = list(loo_allvar_voc, loo_biasprec_voc, loo_bias_voc, loo_prec_voc, loo_inc_voc, loo_ign_voc)
modelnames =  c('allvar', 'biasprec', 'bias', 'prec', 'inconsequential', 'ignorable')
loo_tab_voc = loo_all_voc %>%  # extracting elpd_loo estimates
  lapply(., function(x)x[[1]][1,]) %>% 
  do.call(rbind, .) %>% 
  as.data.frame(.) %>% 
  mutate(model = modelnames) %>% 
  rename(elpd_loo = Estimate, 
         elpd_SE = SE)

# creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                     weights=~wts_voc, # including raked weights in the survey design
                     data=nhsub_voc)

# calculating loo_wtd
wtd_loo_tab_voc = lapply(loo_all_voc, function(x)loo_wtd(x,svy_rake)) %>%
  do.call(rbind,.) %>%
  data.frame(.) %>%
  mutate(model = modelnames) %>%
  rename(wtdElpd_loo = wtd_elpd_loo)

# make MRP estimates
popn_ps = nhdata_full %>% 
  group_by(age, ethnicity, gender, educ, marital_status, phys_act,   
           overweight, dep_severity, BMI_range, alc_exc, diabetes, trb_sleep,
           pov_level, smk2_exp, smk_tobcig, sodium_lvl, potassium_lvl) %>% 
  summarise(Nj = n()) %>% 
  ungroup() 

summary(popn_ps$Nj)

# getting prediction for MRP - for poststrat table
modelallvar_predict_voc = posterior_linpred(nhmodel_allvar_voc, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelallvar_popnest_voc = apply(modelallvar_predict_voc, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbiasprec_predict_voc = posterior_linpred(nhmodel_biasprec_voc, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbiasprec_popnest_voc = apply(modelbiasprec_predict_voc, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbias_predict_voc = posterior_linpred(nhmodel_bias_voc, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbias_popnest_voc = apply(modelbias_predict_voc, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelprec_predict_voc = posterior_linpred(nhmodel_prec_voc, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelprec_popnest_voc = apply(modelprec_predict_voc, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelinc_predict_voc = posterior_linpred(nhmodel_inc_voc, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelinc_popnest_voc = apply(modelinc_predict_voc, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelign_predict_voc = posterior_linpred(nhmodel_ign_voc, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelign_popnest_voc = apply(modelign_predict_voc, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


popnest_all_voc = list(modelallvar_popnest_voc, modelbiasprec_popnest_voc, modelbias_popnest_voc, modelprec_popnest_voc, modelinc_popnest_voc, modelign_popnest_voc)

popnest_tab_voc = lapply(popnest_all_voc, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames)

# intervalScr 
alph = 0.1
popnest_tab_voc = popnest_tab_voc %>%  
  mutate(mean_yObs = mean(as.numeric(nhdata_full$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)),
         sample = "VOC")


# dr2 ---------------------------------------------------------------------
nhmodel_allvar_dr2 = readRDS(file=here('nhanes/data/nhmodel_allvar_dr2.rds'))
nhmodel_biasprec_dr2 = readRDS(file=here('nhanes/data/nhmodel_biasprec_dr2.rds'))
nhmodel_bias_dr2 = readRDS(file=here('nhanes/data/nhmodel_bias_dr2.rds'))
nhmodel_prec_dr2 = readRDS(file=here('nhanes/data/nhmodel_prec_dr2.rds'))
nhmodel_inc_dr2 = readRDS(file=here('nhanes/data/nhmodel_inc_dr2.rds'))
nhmodel_ign_dr2 = readRDS(file=here('nhanes/data/nhmodel_ign_dr2.rds'))

loo_allvar_dr2 = loo(nhmodel_allvar_dr2)
loo_biasprec_dr2 = loo(nhmodel_biasprec_dr2)
loo_bias_dr2 = loo(nhmodel_bias_dr2)
loo_prec_dr2 = loo(nhmodel_prec_dr2)
loo_inc_dr2 = loo(nhmodel_inc_dr2)
loo_ign_dr2 = loo(nhmodel_ign_dr2)

loo_all_dr2 = list(loo_allvar_dr2, loo_biasprec_dr2, loo_bias_dr2, loo_prec_dr2, loo_inc_dr2, loo_ign_dr2)
loo_tab_dr2 = loo_all_dr2 %>%  # extracting elpd_loo estimates
  lapply(., function(x)x[[1]][1,]) %>% 
  do.call(rbind, .) %>% 
  as.data.frame(.) %>% 
  mutate(model = modelnames) %>% 
  rename(elpd_loo = Estimate, 
         elpd_SE = SE)

# creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                     weights=~wts_dr2, # including raked weights in the survey design
                     data=nhsub_dr2)

# calculating loo_wtd
wtd_loo_tab_dr2 = lapply(loo_all_dr2, function(x)loo_wtd(x,svy_rake)) %>%
  do.call(rbind,.) %>%
  data.frame(.) %>%
  mutate(model = modelnames) %>%
  rename(wtdElpd_loo = wtd_elpd_loo)


# getting prediction for MRP - for poststrat table
modelallvar_predict_dr2 = posterior_linpred(nhmodel_allvar_dr2, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelallvar_popnest_dr2 = apply(modelallvar_predict_dr2, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbiasprec_predict_dr2 = posterior_linpred(nhmodel_biasprec_dr2, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbiasprec_popnest_dr2 = apply(modelbiasprec_predict_dr2, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbias_predict_dr2 = posterior_linpred(nhmodel_bias_dr2, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbias_popnest_dr2 = apply(modelbias_predict_dr2, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelprec_predict_dr2 = posterior_linpred(nhmodel_prec_dr2, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelprec_popnest_dr2 = apply(modelprec_predict_dr2, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelinc_predict_dr2 = posterior_linpred(nhmodel_inc_dr2, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelinc_popnest_dr2 = apply(modelinc_predict_dr2, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelign_predict_dr2 = posterior_linpred(nhmodel_ign_dr2, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelign_popnest_dr2 = apply(modelign_predict_dr2, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

popnest_all_dr2 = list(modelallvar_popnest_dr2, modelbiasprec_popnest_dr2, modelbias_popnest_dr2, modelprec_popnest_dr2, modelinc_popnest_dr2, modelign_popnest_dr2)

popnest_tab_dr2 = lapply(popnest_all_dr2, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames)

# intervalScr 
alph = 0.1
popnest_tab_dr2 = popnest_tab_dr2 %>%  
  mutate(mean_yObs = mean(as.numeric(nhdata_full$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)),
         sample = "Dietary")

# env ---------------------------------------------------------------------
nhmodel_allvar_env = readRDS(file=here('nhanes/data/nhmodel_allvar_env.rds'))
nhmodel_biasprec_env = readRDS(file=here('nhanes/data/nhmodel_biasprec_env.rds'))
nhmodel_bias_env = readRDS(file=here('nhanes/data/nhmodel_bias_env.rds'))
nhmodel_prec_env = readRDS(file=here('nhanes/data/nhmodel_prec_env.rds'))
nhmodel_inc_env = readRDS(file=here('nhanes/data/nhmodel_inc_env.rds'))
nhmodel_ign_env = readRDS(file=here('nhanes/data/nhmodel_ign_env.rds'))

loo_allvar_env = loo(nhmodel_allvar_env)
loo_biasprec_env = loo(nhmodel_biasprec_env)
loo_bias_env = loo(nhmodel_bias_env)
loo_prec_env = loo(nhmodel_prec_env)
loo_inc_env = loo(nhmodel_inc_env)
loo_ign_env = loo(nhmodel_ign_env)

loo_all_env = list(loo_allvar_env, loo_biasprec_env, loo_bias_env, loo_prec_env, loo_inc_env, loo_ign_env)
loo_tab_env = loo_all_env %>%  # extracting elpd_loo estimates
  lapply(., function(x)x[[1]][1,]) %>% 
  do.call(rbind, .) %>% 
  as.data.frame(.) %>% 
  mutate(model = modelnames) %>% 
  rename(elpd_loo = Estimate, 
         elpd_SE = SE)

# creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                     weights=~wts_env, # including raked weights in the survey design
                     data=nhsub_env)

# calculating loo_wtd
wtd_loo_tab_env = lapply(loo_all_env, function(x)loo_wtd(x,svy_rake)) %>%
  do.call(rbind,.) %>%
  data.frame(.) %>%
  mutate(model = modelnames) %>%
  rename(wtdElpd_loo = wtd_elpd_loo)


# getting prediction for MRP - for poststrat table
modelallvar_predict_env = posterior_linpred(nhmodel_allvar_env, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelallvar_popnest_env = apply(modelallvar_predict_env, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbiasprec_predict_env = posterior_linpred(nhmodel_biasprec_env, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbiasprec_popnest_env = apply(modelbiasprec_predict_env, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbias_predict_env = posterior_linpred(nhmodel_bias_env, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbias_popnest_env = apply(modelbias_predict_env, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelprec_predict_env = posterior_linpred(nhmodel_prec_env, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelprec_popnest_env = apply(modelprec_predict_env, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelinc_predict_env = posterior_linpred(nhmodel_inc_env, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelinc_popnest_env = apply(modelinc_predict_env, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelign_predict_env = posterior_linpred(nhmodel_ign_env, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelign_popnest_env = apply(modelign_predict_env, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

popnest_all_env = list(modelallvar_popnest_env, modelbiasprec_popnest_env, modelbias_popnest_env, modelprec_popnest_env, modelinc_popnest_env, modelign_popnest_env)

popnest_tab_env = lapply(popnest_all_env, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames)

# intervalScr 
alph = 0.1
popnest_tab_env = popnest_tab_env %>%  
  mutate(mean_yObs = mean(as.numeric(nhdata_full$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)),
         sample = "Environ. A")

# Fasting -----------------------------------------------------------------
nhmodel_allvar_fas = readRDS(file=here('nhanes/data/nhmodel_allvar_fas.rds'))
nhmodel_biasprec_fas = readRDS(file=here('nhanes/data/nhmodel_biasprec_fas.rds'))
nhmodel_bias_fas = readRDS(file=here('nhanes/data/nhmodel_bias_fas.rds'))
nhmodel_prec_fas = readRDS(file=here('nhanes/data/nhmodel_prec_fas.rds'))
nhmodel_inc_fas = readRDS(file=here('nhanes/data/nhmodel_inc_fas.rds'))
nhmodel_ign_fas = readRDS(file=here('nhanes/data/nhmodel_ign_fas.rds'))

loo_allvar_fas = loo(nhmodel_allvar_fas)
loo_biasprec_fas = loo(nhmodel_biasprec_fas)
loo_bias_fas = loo(nhmodel_bias_fas)
loo_prec_fas = loo(nhmodel_prec_fas)
loo_inc_fas = loo(nhmodel_inc_fas)
loo_ign_fas = loo(nhmodel_ign_fas)

loo_all_fas = list(loo_allvar_fas, loo_biasprec_fas, loo_bias_fas, loo_prec_fas, loo_inc_fas, loo_ign_fas)
loo_tab_fas = loo_all_fas %>%  # extracting elpd_loo estimates
  lapply(., function(x)x[[1]][1,]) %>% 
  do.call(rbind, .) %>% 
  as.data.frame(.) %>% 
  mutate(model = modelnames) %>% 
  rename(elpd_loo = Estimate, 
         elpd_SE = SE)

# creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                     weights=~wts_fas, # including raked weights in the survey design
                     data=nhsub_fas)

# calculating loo_wtd
wtd_loo_tab_fas = lapply(loo_all_fas, function(x)loo_wtd(x,svy_rake)) %>%
  do.call(rbind,.) %>%
  data.frame(.) %>%
  mutate(model = modelnames) %>%
  rename(wtdElpd_loo = wtd_elpd_loo)


# getting prediction for MRP - for poststrat table
modelallvar_predict_fas = posterior_linpred(nhmodel_allvar_fas, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelallvar_popnest_fas = apply(modelallvar_predict_fas, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbiasprec_predict_fas = posterior_linpred(nhmodel_biasprec_fas, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbiasprec_popnest_fas = apply(modelbiasprec_predict_fas, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbias_predict_fas = posterior_linpred(nhmodel_bias_fas, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbias_popnest_fas = apply(modelbias_predict_fas, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelprec_predict_fas = posterior_linpred(nhmodel_prec_fas, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelprec_popnest_fas = apply(modelprec_predict_fas, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelinc_predict_fas = posterior_linpred(nhmodel_inc_fas, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelinc_popnest_fas = apply(modelinc_predict_fas, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelign_predict_fas = posterior_linpred(nhmodel_ign_fas, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelign_popnest_fas = apply(modelign_predict_fas, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

popnest_all_fas = list(modelallvar_popnest_fas, modelbiasprec_popnest_fas, modelbias_popnest_fas, modelprec_popnest_fas, modelinc_popnest_fas, modelign_popnest_fas)

popnest_tab_fas = lapply(popnest_all_fas, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames)

# intervalScr 
alph = 0.1
popnest_tab_fas = popnest_tab_fas %>%  
  mutate(mean_yObs = mean(as.numeric(nhdata_full$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)),
         sample = "Fasting")


# mrp plot ----------------------------------------------------------------
# combine all four together
join1_dr2 = left_join(loo_tab_dr2, wtd_loo_tab_dr2, by = "model")
popn_all_tab_dr2 = left_join(join1_dr2, popnest_tab_dr2, by="model") %>% 
  mutate(MRP_intScr_scaled = (MRP_intervalScr - min(MRP_intervalScr)) / (max(MRP_intervalScr) - min(MRP_intervalScr)))

join1_voc = left_join(loo_tab_voc, wtd_loo_tab_voc, by = "model")
popn_all_tab_voc = left_join(join1_voc, popnest_tab_voc, by="model") %>% 
  mutate(MRP_intScr_scaled = (MRP_intervalScr - min(MRP_intervalScr)) / (max(MRP_intervalScr) - min(MRP_intervalScr)))

join1_env = left_join(loo_tab_env, wtd_loo_tab_env, by = "model")
popn_all_tab_env = left_join(join1_env, popnest_tab_env, by="model") %>% 
  mutate(MRP_intScr_scaled = (MRP_intervalScr - min(MRP_intervalScr)) / (max(MRP_intervalScr) - min(MRP_intervalScr)))

join1_fas = left_join(loo_tab_fas, wtd_loo_tab_fas, by = "model")
popn_all_tab_fas = left_join(join1_fas, popnest_tab_fas, by="model") %>% 
  mutate(MRP_intScr_scaled = (MRP_intervalScr - min(MRP_intervalScr)) / (max(MRP_intervalScr) - min(MRP_intervalScr)))

shape_base  = c(21, 22, 24, 23, 25, 4)
colour_palette_var_base  =  c(`All variables` =  "#4477AA",
                              `Bias-precision` = "#88CCEE", 
                              `Bias-only` = "#CC6677",
                              `Precision-only` = "#228833", 
                              `Inconsequential` = "#BBBBBB", 
                              `Ignorable` = '#DDDDDD')
bd_col = c(`All variables` =  "black",
           `Bias-precision` = "black", 
           `Bias-only` = "black",
           `Precision-only` = "black", 
           `Inconsequential` = "black",
           `Ignorable` = "black")

scales::show_col(ggthemes::colorblind_pal()(8))
colour_palette_var_base2 = c('#CC79A7', '#009E73', '#F0E442', '#D55E00')


( g1 = rbind(popn_all_tab_dr2, popn_all_tab_env, popn_all_tab_voc, popn_all_tab_fas) %>% 
    mutate(model = case_when(model == 'allvar' ~ "All variables",
                             model == 'biasprec' ~ "Bias-precision",
                             model == 'bias' ~ "Bias-only",
                             model == 'prec' ~ "Precision-only",
                             model == 'inconsequential' ~ "Inconsequential",
                             model == 'ignorable' ~ "Ignorable"),
           model = fct_relevel(model, c("All variables", "Bias-precision", "Bias-only", "Precision-only",
                                        "Inconsequential", "Ignorable"))) %>% 
    mutate(elpd_loo_std = (elpd_loo - min(elpd_loo)) / (max(elpd_loo) - min(elpd_loo)),
           wtdElpd_loo_std = (wtdElpd_loo - min(wtdElpd_loo)) / (max(wtdElpd_loo) - min(wtdElpd_loo))) %>% 
    pivot_longer(cols = c("elpd_loo_std","wtdElpd_loo_std"), names_to = "type", values_to = "model_score") %>%
    mutate(type = factor(type),
           type = fct_recode(type, `PSIS-LOO` = "elpd_loo_std", `WTD-PSIS-LOO` = "wtdElpd_loo_std")) %>% 
    ggplot(., aes(x = model_score, y = MRP_intScr_scaled, group=sample, shape = model, fill = sample, colour = sample)) +
    facet_grid(~type, scales = "free")+
    geom_line(alpha=0.6) + 
    geom_point(size=4, alpha = .7) + 
    theme_bw(base_size = 15)  +
    scale_shape_manual(values = shape_base) +
    scale_fill_manual(values = colour_palette_var_base2) +
    scale_colour_manual(values = colour_palette_var_base2) +
    ylab("Scaled interval score for MRP estimates") +
    xlab("Model score") + 
    guides(fill = guide_legend("Sample", override.aes=list(color=colour_palette_var_base2)),
           colour = "none",
           shape = guide_legend("Model")) +
    ggtitle('MRP interval score') +
    theme(legend.title = element_text(size=15, face="bold"),
          legend.text = element_text(size=15)))

ggsave(g1, width=14, height=8, file=here("nhanes/figures/elpd_MRP_all.png"))



# individual level --------------------------------------------------------
# VOC ---------------------------------------------------------------------
pred_allvar_sampInd_voc = posterior_predict(nhmodel_allvar_voc, newdata = nhsub_voc)  # getting outcome estimate for each sample indv

modelallvar_pred_sampInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelallvar_pred_sampInd_voc[i] = mean(pred_allvar_sampInd_voc[,i] == nhsub_voc$high_bp[i])
}
modelallvar_pred_sampInd_voc = modelallvar_pred_sampInd_voc %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# all var - popn
pred_allvar_popnInd_voc = posterior_predict(nhmodel_allvar_voc, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelallvar_pred_popnInd_voc[i] = mean(pred_allvar_popnInd_voc[,i] == nhdata_full$high_bp[i])
}
modelallvar_pred_popnInd_voc = modelallvar_pred_popnInd_voc %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# biasprec - sample
pred_biasprec_sampInd_voc = posterior_predict(nhmodel_biasprec_voc, newdata = nhsub_voc)  # getting outcome estimate for each sample indv
modelbiasprec_pred_sampInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelbiasprec_pred_sampInd_voc[i] = mean(pred_biasprec_sampInd_voc[,i] == nhsub_voc$high_bp[i])
}
modelbiasprec_pred_sampInd_voc = modelbiasprec_pred_sampInd_voc %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)
# biasprec - popn
pred_biasprec_popnInd_voc = posterior_predict(nhmodel_biasprec_voc, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelbiasprec_pred_popnInd_voc[i] = mean(pred_biasprec_popnInd_voc[,i] == nhdata_full$high_bp[i])
}
modelbiasprec_pred_popnInd_voc = modelbiasprec_pred_popnInd_voc %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# bias - sample
pred_bias_sampInd_voc = posterior_predict(nhmodel_bias_voc, newdata = nhsub_voc)  # getting outcome estimate for each sample indv
modelbias_pred_sampInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelbias_pred_sampInd_voc[i] = mean(pred_bias_sampInd_voc[,i] == nhsub_voc$high_bp[i])
}
modelbias_pred_sampInd_voc = modelbias_pred_sampInd_voc %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)
# bias - popn
pred_bias_popnInd_voc = posterior_predict(nhmodel_bias_voc, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelbias_pred_popnInd_voc[i] = mean(pred_bias_popnInd_voc[,i] == nhdata_full$high_bp[i])
}
modelbias_pred_popnInd_voc = modelbias_pred_popnInd_voc %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# precision - sample
pred_prec_sampInd_voc = posterior_predict(nhmodel_prec_voc, newdata = nhsub_voc)  # getting outcome estimate for each sample indv
modelprec_pred_sampInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelprec_pred_sampInd_voc[i] = mean(pred_prec_sampInd_voc[,i] == nhsub_voc$high_bp[i])
}
modelprec_pred_sampInd_voc = modelprec_pred_sampInd_voc %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)
# precision - popn
pred_prec_popnInd_voc = posterior_predict(nhmodel_prec_voc, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelprec_pred_popnInd_voc[i] = mean(pred_prec_popnInd_voc[,i] == nhdata_full$high_bp[i])
}
modelprec_pred_popnInd_voc = modelprec_pred_popnInd_voc %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# inconsequential - sample
pred_inc_sampInd_voc = posterior_predict(nhmodel_inc_voc, newdata = nhsub_voc)  # getting outcome estimate for each sample indv
modelinc_pred_sampInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelinc_pred_sampInd_voc[i] = mean(pred_inc_sampInd_voc[,i] == nhsub_voc$high_bp[i])
}
modelinc_pred_sampInd_voc = modelinc_pred_sampInd_voc %>% 
  as_tibble %>% 
  mutate(model="inconsequential") %>% 
  rename(prob_pred_out = value)
# inc - popn
pred_inc_popnInd_voc = posterior_predict(nhmodel_inc_voc, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelinc_pred_popnInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelinc_pred_popnInd_voc[i] = mean(pred_inc_popnInd_voc[,i] == nhdata_full$high_bp[i])
}
modelinc_pred_popnInd_voc = modelinc_pred_popnInd_voc %>% 
  as_tibble %>% 
  mutate(model="inconsequential") %>% 
  rename(prob_pred_out = value)

# ignorable - sample
pred_ign_sampInd_voc = posterior_predict(nhmodel_ign_voc, newdata = nhsub_voc)  # getting outcome estimate for each sample indv
modelign_pred_sampInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelign_pred_sampInd_voc[i] = mean(pred_ign_sampInd_voc[,i] == nhsub_voc$high_bp[i])
}
modelign_pred_sampInd_voc = modelign_pred_sampInd_voc %>% 
  as_tibble %>% 
  mutate(model="ignorable") %>% 
  rename(prob_pred_out = value)
# ign - popn
pred_ign_popnInd_voc = posterior_predict(nhmodel_ign_voc, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelign_pred_popnInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelign_pred_popnInd_voc[i] = mean(pred_ign_popnInd_voc[,i] == nhdata_full$high_bp[i])
}
modelign_pred_popnInd_voc = modelign_pred_popnInd_voc %>% 
  as_tibble %>% 
  mutate(model="ignorable") %>% 
  rename(prob_pred_out = value)


pred_sampInd_voc = list(modelallvar_pred_sampInd_voc, modelbiasprec_pred_sampInd_voc,
                        modelbias_pred_sampInd_voc, modelprec_pred_sampInd_voc, 
                        modelinc_pred_sampInd_voc, modelign_pred_sampInd_voc) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 0,
         sample = "VOC") 

pred_popnInd_voc = list(modelallvar_pred_popnInd_voc, modelbiasprec_pred_popnInd_voc, 
                        modelbias_pred_popnInd_voc, modelprec_pred_popnInd_voc, 
                        modelinc_pred_popnInd_voc, modelign_pred_popnInd_voc) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 1,
         sample = "VOC") 

# dr2 ---------------------------------------------------------------------
pred_allvar_sampInd_dr2 = posterior_predict(nhmodel_allvar_dr2, newdata = nhsub_dr2)  # getting outcome estimate for each sample indv

modelallvar_pred_sampInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelallvar_pred_sampInd_dr2[i] = mean(pred_allvar_sampInd_dr2[,i] == nhsub_dr2$high_bp[i])
}
modelallvar_pred_sampInd_dr2 = modelallvar_pred_sampInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# all var - popn
pred_allvar_popnInd_dr2 = posterior_predict(nhmodel_allvar_dr2, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelallvar_pred_popnInd_dr2[i] = mean(pred_allvar_popnInd_dr2[,i] == nhdata_full$high_bp[i])
}
modelallvar_pred_popnInd_dr2 = modelallvar_pred_popnInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# biasprec - sample
pred_biasprec_sampInd_dr2 = posterior_predict(nhmodel_biasprec_dr2, newdata = nhsub_dr2)  # getting outcome estimate for each sample indv
modelbiasprec_pred_sampInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelbiasprec_pred_sampInd_dr2[i] = mean(pred_biasprec_sampInd_dr2[,i] == nhsub_dr2$high_bp[i])
}
modelbiasprec_pred_sampInd_dr2 = modelbiasprec_pred_sampInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# biasprec - popn
pred_biasprec_popnInd_dr2 = posterior_predict(nhmodel_biasprec_dr2, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelbiasprec_pred_popnInd_dr2[i] = mean(pred_biasprec_popnInd_dr2[,i] == nhdata_full$high_bp[i])
}
modelbiasprec_pred_popnInd_dr2 = modelbiasprec_pred_popnInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# bias - sample
pred_bias_sampInd_dr2 = posterior_predict(nhmodel_bias_dr2, newdata = nhsub_dr2)  # getting outcome estimate for each sample indv
modelbias_pred_sampInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelbias_pred_sampInd_dr2[i] = mean(pred_bias_sampInd_dr2[,i] == nhsub_dr2$high_bp[i])
}
modelbias_pred_sampInd_dr2 = modelbias_pred_sampInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# bias - popn
pred_bias_popnInd_dr2 = posterior_predict(nhmodel_bias_dr2, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelbias_pred_popnInd_dr2[i] = mean(pred_bias_popnInd_dr2[,i] == nhdata_full$high_bp[i])
}
modelbias_pred_popnInd_dr2 = modelbias_pred_popnInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# precision - sample
pred_prec_sampInd_dr2 = posterior_predict(nhmodel_prec_dr2, newdata = nhsub_dr2)  # getting outcome estimate for each sample indv
modelprec_pred_sampInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelprec_pred_sampInd_dr2[i] = mean(pred_prec_sampInd_dr2[,i] == nhsub_dr2$high_bp[i])
}
modelprec_pred_sampInd_dr2 = modelprec_pred_sampInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)
# precision - popn
pred_prec_popnInd_dr2 = posterior_predict(nhmodel_prec_dr2, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelprec_pred_popnInd_dr2[i] = mean(pred_prec_popnInd_dr2[,i] == nhdata_full$high_bp[i])
}
modelprec_pred_popnInd_dr2 = modelprec_pred_popnInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# inc - sample
pred_inc_sampInd_dr2 = posterior_predict(nhmodel_inc_dr2, newdata = nhsub_dr2)  # getting outcome estimate for each sample indv
modelinc_pred_sampInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelinc_pred_sampInd_dr2[i] = mean(pred_inc_sampInd_dr2[,i] == nhsub_dr2$high_bp[i])
}
modelinc_pred_sampInd_dr2 = modelinc_pred_sampInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="inconsequential") %>% 
  rename(prob_pred_out = value)
# inc - popn
pred_inc_popnInd_dr2 = posterior_predict(nhmodel_inc_dr2, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelinc_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelinc_pred_popnInd_dr2[i] = mean(pred_inc_popnInd_dr2[,i] == nhdata_full$high_bp[i])
}
modelinc_pred_popnInd_dr2 = modelinc_pred_popnInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="inconsequential") %>% 
  rename(prob_pred_out = value)

# ignorable - sample
pred_ign_sampInd_dr2 = posterior_predict(nhmodel_ign_dr2, newdata = nhsub_dr2)  # getting outcome estimate for each sample indv
modelign_pred_sampInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelign_pred_sampInd_dr2[i] = mean(pred_ign_sampInd_dr2[,i] == nhsub_dr2$high_bp[i])
}
modelign_pred_sampInd_dr2 = modelign_pred_sampInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="ignorable") %>% 
  rename(prob_pred_out = value)
# ignorable - popn
pred_ign_popnInd_dr2 = posterior_predict(nhmodel_ign_dr2, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelign_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelign_pred_popnInd_dr2[i] = mean(pred_ign_popnInd_dr2[,i] == nhdata_full$high_bp[i])
}
modelign_pred_popnInd_dr2 = modelign_pred_popnInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="ignorable") %>% 
  rename(prob_pred_out = value)

pred_sampInd_dr2 = list(modelallvar_pred_sampInd_dr2, modelbiasprec_pred_sampInd_dr2,
                    modelbias_pred_sampInd_dr2, modelprec_pred_sampInd_dr2, 
                    modelinc_pred_sampInd_dr2, modelign_pred_sampInd_dr2) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 0,
         sample = "Dietary") 

pred_popnInd_dr2 = list(modelallvar_pred_popnInd_dr2, modelbiasprec_pred_popnInd_dr2, 
                    modelbias_pred_popnInd_dr2, modelprec_pred_popnInd_dr2, 
                    modelinc_pred_popnInd_dr2, modelign_pred_popnInd_dr2) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 1,
         sample = "Dietary") 

# env ---------------------------------------------------------------------
pred_allvar_sampInd_env = posterior_predict(nhmodel_allvar_env, newdata = nhsub_env)  # getting outcome estimate for each sample indv

modelallvar_pred_sampInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelallvar_pred_sampInd_env[i] = mean(pred_allvar_sampInd_env[,i] == nhsub_env$high_bp[i])
}
modelallvar_pred_sampInd_env = modelallvar_pred_sampInd_env %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# all var - popn
pred_allvar_popnInd_env = posterior_predict(nhmodel_allvar_env, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelallvar_pred_popnInd_env[i] = mean(pred_allvar_popnInd_env[,i] == nhdata_full$high_bp[i])
}
modelallvar_pred_popnInd_env = modelallvar_pred_popnInd_env %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# biasprec - sample
pred_biasprec_sampInd_env = posterior_predict(nhmodel_biasprec_env, newdata = nhsub_env)  # getting outcome estimate for each sample indv
modelbiasprec_pred_sampInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelbiasprec_pred_sampInd_env[i] = mean(pred_biasprec_sampInd_env[,i] == nhsub_env$high_bp[i])
}
modelbiasprec_pred_sampInd_env = modelbiasprec_pred_sampInd_env %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# biasprec - popn
pred_biasprec_popnInd_env = posterior_predict(nhmodel_biasprec_env, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelbiasprec_pred_popnInd_env[i] = mean(pred_biasprec_popnInd_env[,i] == nhdata_full$high_bp[i])
}
modelbiasprec_pred_popnInd_env = modelbiasprec_pred_popnInd_env %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# bias - sample
pred_bias_sampInd_env = posterior_predict(nhmodel_bias_env, newdata = nhsub_env)  # getting outcome estimate for each sample indv
modelbias_pred_sampInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelbias_pred_sampInd_env[i] = mean(pred_bias_sampInd_env[,i] == nhsub_env$high_bp[i])
}
modelbias_pred_sampInd_env = modelbias_pred_sampInd_env %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# bias - popn
pred_bias_popnInd_env = posterior_predict(nhmodel_bias_env, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelbias_pred_popnInd_env[i] = mean(pred_bias_popnInd_env[,i] == nhdata_full$high_bp[i])
}
modelbias_pred_popnInd_env = modelbias_pred_popnInd_env %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# precision - sample
pred_prec_sampInd_env = posterior_predict(nhmodel_prec_env, newdata = nhsub_env)  # getting outcome estimate for each sample indv
modelprec_pred_sampInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelprec_pred_sampInd_env[i] = mean(pred_prec_sampInd_env[,i] == nhsub_env$high_bp[i])
}
modelprec_pred_sampInd_env = modelprec_pred_sampInd_env %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)
# precision - popn
pred_prec_popnInd_env = posterior_predict(nhmodel_prec_env, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelprec_pred_popnInd_env[i] = mean(pred_prec_popnInd_env[,i] == nhdata_full$high_bp[i])
}
modelprec_pred_popnInd_env = modelprec_pred_popnInd_env %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# inconsequential - sample
pred_inc_sampInd_env = posterior_predict(nhmodel_inc_env, newdata = nhsub_env)  # getting outcome estimate for each sample indv
modelinc_pred_sampInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelinc_pred_sampInd_env[i] = mean(pred_inc_sampInd_env[,i] == nhsub_env$high_bp[i])
}
modelinc_pred_sampInd_env = modelinc_pred_sampInd_env %>% 
  as_tibble %>% 
  mutate(model="inconsequential") %>% 
  rename(prob_pred_out = value)
# inconsequential - popn
pred_inc_popnInd_env = posterior_predict(nhmodel_inc_env, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelinc_pred_popnInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelinc_pred_popnInd_env[i] = mean(pred_inc_popnInd_env[,i] == nhdata_full$high_bp[i])
}
modelinc_pred_popnInd_env = modelinc_pred_popnInd_env %>% 
  as_tibble %>% 
  mutate(model="inconsequential") %>% 
  rename(prob_pred_out = value)


# ignorable - sample
pred_ign_sampInd_env = posterior_predict(nhmodel_ign_env, newdata = nhsub_env)  # getting outcome estimate for each sample indv
modelign_pred_sampInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelign_pred_sampInd_env[i] = mean(pred_ign_sampInd_env[,i] == nhsub_env$high_bp[i])
}
modelign_pred_sampInd_env = modelign_pred_sampInd_env %>% 
  as_tibble %>% 
  mutate(model="ignorable") %>% 
  rename(prob_pred_out = value)
# ignorable - popn
pred_ign_popnInd_env = posterior_predict(nhmodel_ign_env, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelign_pred_popnInd_env = matrix(NA)
for (i in 1:nrow(nhsub_env)){
  modelign_pred_popnInd_env[i] = mean(pred_ign_popnInd_env[,i] == nhdata_full$high_bp[i])
}
modelign_pred_popnInd_env = modelign_pred_popnInd_env %>% 
  as_tibble %>% 
  mutate(model="ignorable") %>% 
  rename(prob_pred_out = value)

pred_sampInd_env = list(modelallvar_pred_sampInd_env, modelbiasprec_pred_sampInd_env,
                        modelbias_pred_sampInd_env, modelprec_pred_sampInd_env,
                        modelinc_pred_sampInd_env, modelign_pred_sampInd_env) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 0,
         sample = "Environ. A") 

pred_popnInd_env = list(modelallvar_pred_popnInd_env, modelbiasprec_pred_popnInd_env, 
                        modelbias_pred_popnInd_env, modelprec_pred_popnInd_env, 
                        modelinc_pred_popnInd_env, modelign_pred_popnInd_env) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 1,
         sample = "Environ. A") 

# fas ---------------------------------------------------------------------
pred_allvar_sampInd_fas = posterior_predict(nhmodel_allvar_fas, newdata = nhsub_fas)  # getting outcome estimate for each sample indv

modelallvar_pred_sampInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelallvar_pred_sampInd_fas[i] = mean(pred_allvar_sampInd_fas[,i] == nhsub_fas$high_bp[i])
}
modelallvar_pred_sampInd_fas = modelallvar_pred_sampInd_fas %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# all var - popn
pred_allvar_popnInd_fas = posterior_predict(nhmodel_allvar_fas, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelallvar_pred_popnInd_fas[i] = mean(pred_allvar_popnInd_fas[,i] == nhdata_full$high_bp[i])
}
modelallvar_pred_popnInd_fas = modelallvar_pred_popnInd_fas %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# biasprec - sample
pred_biasprec_sampInd_fas = posterior_predict(nhmodel_biasprec_fas, newdata = nhsub_fas)  # getting outcome estimate for each sample indv
modelbiasprec_pred_sampInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelbiasprec_pred_sampInd_fas[i] = mean(pred_biasprec_sampInd_fas[,i] == nhsub_fas$high_bp[i])
}
modelbiasprec_pred_sampInd_fas = modelbiasprec_pred_sampInd_fas %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# biasprec - popn
pred_biasprec_popnInd_fas = posterior_predict(nhmodel_biasprec_fas, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelbiasprec_pred_popnInd_fas[i] = mean(pred_biasprec_popnInd_fas[,i] == nhdata_full$high_bp[i])
}
modelbiasprec_pred_popnInd_fas = modelbiasprec_pred_popnInd_fas %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# bias - sample
pred_bias_sampInd_fas = posterior_predict(nhmodel_bias_fas, newdata = nhsub_fas)  # getting outcome estimate for each sample indv
modelbias_pred_sampInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelbias_pred_sampInd_fas[i] = mean(pred_bias_sampInd_fas[,i] == nhsub_fas$high_bp[i])
}
modelbias_pred_sampInd_fas = modelbias_pred_sampInd_fas %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# bias - popn
pred_bias_popnInd_fas = posterior_predict(nhmodel_bias_fas, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelbias_pred_popnInd_fas[i] = mean(pred_bias_popnInd_fas[,i] == nhdata_full$high_bp[i])
}
modelbias_pred_popnInd_fas = modelbias_pred_popnInd_fas %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# precision - sample
pred_prec_sampInd_fas = posterior_predict(nhmodel_prec_fas, newdata = nhsub_fas)  # getting outcome estimate for each sample indv
modelprec_pred_sampInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelprec_pred_sampInd_fas[i] = mean(pred_prec_sampInd_fas[,i] == nhsub_fas$high_bp[i])
}
modelprec_pred_sampInd_fas = modelprec_pred_sampInd_fas %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# precision - popn
pred_prec_popnInd_fas = posterior_predict(nhmodel_prec_fas, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelprec_pred_popnInd_fas[i] = mean(pred_prec_popnInd_fas[,i] == nhdata_full$high_bp[i])
}
modelprec_pred_popnInd_fas = modelprec_pred_popnInd_fas %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# inconsequential - sample
pred_inc_sampInd_fas = posterior_predict(nhmodel_inc_fas, newdata = nhsub_fas)  # getting outcome estimate for each sample indv
modelinc_pred_sampInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelinc_pred_sampInd_fas[i] = mean(pred_inc_sampInd_fas[,i] == nhsub_fas$high_bp[i])
}
modelinc_pred_sampInd_fas = modelinc_pred_sampInd_fas %>% 
  as_tibble %>% 
  mutate(model="inconsequential") %>% 
  rename(prob_pred_out = value)
# inconsequential - popn
pred_inc_popnInd_fas = posterior_predict(nhmodel_inc_fas, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelinc_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelinc_pred_popnInd_fas[i] = mean(pred_inc_popnInd_fas[,i] == nhdata_full$high_bp[i])
}
modelinc_pred_popnInd_fas = modelinc_pred_popnInd_fas %>% 
  as_tibble %>% 
  mutate(model="inconsequential") %>% 
  rename(prob_pred_out = value)

# ignorable - sample
pred_ign_sampInd_fas = posterior_predict(nhmodel_ign_fas, newdata = nhsub_fas)  # getting outcome estimate for each sample indv
modelign_pred_sampInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelign_pred_sampInd_fas[i] = mean(pred_ign_sampInd_fas[,i] == nhsub_fas$high_bp[i])
}
modelign_pred_sampInd_fas = modelign_pred_sampInd_fas %>% 
  as_tibble %>% 
  mutate(model="ignorable") %>% 
  rename(prob_pred_out = value)
# ignorable - popn
pred_ign_popnInd_fas = posterior_predict(nhmodel_ign_fas, newdata = nhdata_full)  # getting outcome estimate for each sample indv
modelign_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelign_pred_popnInd_fas[i] = mean(pred_ign_popnInd_fas[,i] == nhdata_full$high_bp[i])
}
modelign_pred_popnInd_fas = modelign_pred_popnInd_fas %>% 
  as_tibble %>% 
  mutate(model="ignorable") %>% 
  rename(prob_pred_out = value)

pred_sampInd_fas = list(modelallvar_pred_sampInd_fas, modelbiasprec_pred_sampInd_fas,
                        modelbias_pred_sampInd_fas, modelprec_pred_sampInd_fas, 
                        modelinc_pred_sampInd_fas, modelign_pred_sampInd_fas) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 0,
         sample = "Fasting") 

pred_popnInd_fas = list(modelallvar_pred_popnInd_fas, modelbiasprec_pred_popnInd_fas, 
                        modelbias_pred_popnInd_fas, modelprec_pred_popnInd_fas, 
                        modelinc_pred_popnInd_fas, modelign_pred_popnInd_fas) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 1,
         sample = "Fasting") 

j1_voc = left_join(pred_sampInd_voc, loo_tab_voc, by="model")
t1_voc = left_join(j1_voc, wtd_loo_tab_voc, by="model")

j2_voc = left_join(pred_popnInd_voc, loo_tab_voc, by="model")
t2_voc = left_join(j2_voc, wtd_loo_tab_voc, by="model")

j1_dr2 = left_join(pred_sampInd_dr2, loo_tab_dr2, by="model")
t1_dr2 =left_join(j1_dr2, wtd_loo_tab_dr2, by="model")

j2_dr2 = left_join(pred_popnInd_dr2, loo_tab_dr2, by="model")
t2_dr2 = left_join(j2_dr2, wtd_loo_tab_dr2, by="model")

j1_env = left_join(pred_sampInd_env, loo_tab_env, by="model")
t1_env = left_join(j1_env, wtd_loo_tab_env, by="model")

j2_env = left_join(pred_popnInd_env, loo_tab_env, by="model")
t2_env = left_join(j2_env, wtd_loo_tab_env, by="model")

j1_fas = left_join(pred_sampInd_fas, loo_tab_fas, by="model")
t1_fas =left_join(j1_fas, wtd_loo_tab_fas, by="model")

j2_fas = left_join(pred_popnInd_fas, loo_tab_fas, by="model")
t2_fas =left_join(j2_fas, wtd_loo_tab_fas, by="model") 

# individuals plot --------------------------------------------------------------------
(g2 = rbind(t1_voc, t2_voc, t1_dr2, t2_dr2, 
            t1_env, t2_env, t1_fas, t2_fas) %>%
    mutate(model = case_when(model == 'allvar' ~ "All variables",
                             model == 'biasprec' ~ "Bias-precision",
                             model == 'bias' ~ "Bias-only",
                             model == 'prec' ~ "Precision-only",
                             model == 'inconsequential' ~ "Inconsequential",
                             model == 'ignorable' ~ "Ignorable"),
           model = fct_relevel(model, c("All variables", "Bias-precision", "Bias-only", "Precision-only",
                                        "Inconsequential", "Ignorable"))) %>% 
    mutate(popnInd = factor(popnInd),
           popnInd = fct_recode(popnInd, `Sample` = "0", `Population` = "1"),
           elpd_loo_std = (elpd_loo - min(elpd_loo)) / (max(elpd_loo) - min(elpd_loo)),
           wtdElpd_loo_std = (wtdElpd_loo - min(wtdElpd_loo)) / (max(wtdElpd_loo) - min(wtdElpd_loo))) %>% 
    pivot_longer(cols = c("elpd_loo","wtdElpd_loo"), names_to = "type", values_to = "model_score") %>%
    mutate(type = factor(type),
           type = fct_recode(type, `PSIS-LOO` = "elpd_loo", `WTD-PSIS-LOO` = "wtdElpd_loo"))  %>% 
    ggplot(., aes(x = model_score, y = 1-mean_prob_pred_out, colour = sample, shape = factor(model), fill = sample, group=sample)) +
    geom_line(alpha=0.6) +
    geom_point(size=3, alpha = .7) + 
    scale_shape_manual(values = shape_base) +
    scale_fill_manual(values = colour_palette_var_base2) +
    scale_colour_manual(values = colour_palette_var_base2) +
    facet_grid(popnInd~type, scales = "free") +
    theme_bw(base_size = 15) +
    ggtitle('Individual mean prediction of outcome') +
    ylab('1 - mean of prediction of outcome') +
    xlab("Model score") +
    guides(fill = guide_legend("Sample", override.aes=list(color=colour_palette_var_base2)),
           colour = "none",
           shape = guide_legend("Model")) +
    theme(legend.title = element_text(size=15, face="bold"),
          legend.text = element_text(size=15)) )

ggsave(g2, width=14, height=8, file=here("nhanes/figures/elpd_indv_pred_outcome.png"))

