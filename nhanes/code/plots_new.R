## plotting the diff subsample
library(here)
library(loo)
library(tidyverse)
library(brms)
library(survey)
source(here('02-super popn approach/functions.R'))

# reading in data
nhnew = readRDS(file="nhanes/data/nhnew.rds")

nhsub_fas = nhnew %>% 
  filter(incl_fas == 1) %>% 
  select(-c(wts_urn, wts_voc, wts_dr1,
            incl_urn, incl_voc, incl_dr1))

nhsub_urn = nhnew %>% 
  filter(incl_urn == 1) %>% 
  select(-c(wts_fas, wts_voc, wts_dr1,
            incl_fas, incl_voc, incl_dr1))

nhsub_dr1 = nhnew %>% 
  filter(incl_dr1 == 1) %>% 
  select(-c(wts_urn, wts_voc, wts_fas,
            incl_urn, incl_voc, incl_fas))

nhsub_voc = nhnew %>% 
  filter(incl_voc == 1) %>% 
  select(-c(wts_urn, wts_fas, wts_dr1,
            incl_urn, incl_fas, incl_dr1))


# VOC ---------------------------------------------------------------------
nhmodel_allvar_voc = readRDS(file=here('nhanes/data/voc_allvar.rds'))
nhmodel_biasprec_voc = readRDS(file=here('nhanes/data/voc_biasprec.rds'))
nhmodel_bias_voc = readRDS(file=here('nhanes/data/voc_bias.rds'))
nhmodel_prec_voc = readRDS(file=here('nhanes/data/voc_prec.rds'))
nhmodel_none_voc = readRDS(file=here('nhanes/data/voc_none.rds'))

loo_allvar_voc = loo(nhmodel_allvar_voc)
loo_biasprec_voc = loo(nhmodel_biasprec_voc)
loo_bias_voc = loo(nhmodel_bias_voc)
loo_prec_voc = loo(nhmodel_prec_voc)
loo_none_voc = loo(nhmodel_none_voc)

loo_all_voc = list(loo_allvar_voc, loo_biasprec_voc, loo_bias_voc, loo_prec_voc, loo_none_voc)
modelnames =  c('allvar', 'biasprec', 'bias', 'prec', 'none')
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
popn_ps = nhnew %>% 
  group_by(age, ethnicity, gender, educ, marital_status, phys_act,   
           overweight, dep_severity, BMI_range, alc_exc, diabetes, trb_sleep,
           pov_level, smk2_exp, smk_tobcig, sodium_lvl, potassium_lvl, elst_status,
           pest_use, hepA_ind, hepB_ind, hiv_ind, asth_ind) %>% 
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

modelnone_predict_voc = posterior_linpred(nhmodel_none_voc, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelnone_popnest_voc = apply(modelnone_predict_voc, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


popnest_all_voc = list(modelallvar_popnest_voc, modelbiasprec_popnest_voc, modelbias_popnest_voc, modelprec_popnest_voc, modelnone_popnest_voc)

popnest_tab_voc = lapply(popnest_all_voc, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames)

# intervalScr 
alph = 0.1
popnest_tab_voc = popnest_tab_voc %>%  
  mutate(mean_yObs = mean(as.numeric(nhnew$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)),
         sample = "VOC")


# dr1 ---------------------------------------------------------------------
nhmodel_allvar_dr1 = readRDS(file=here('nhanes/data/dr1_allvar.rds'))
nhmodel_biasprec_dr1 = readRDS(file=here('nhanes/data/dr1_biasprec.rds'))
nhmodel_bias_dr1 = readRDS(file=here('nhanes/data/dr1_bias.rds'))
nhmodel_prec_dr1 = readRDS(file=here('nhanes/data/dr1_prec.rds'))
nhmodel_none_dr1 = readRDS(file=here('nhanes/data/dr1_none.rds'))

loo_allvar_dr1 = loo(nhmodel_allvar_dr1)
loo_biasprec_dr1 = loo(nhmodel_biasprec_dr1)
loo_bias_dr1 = loo(nhmodel_bias_dr1)
loo_prec_dr1 = loo(nhmodel_prec_dr1)
loo_none_dr1 = loo(nhmodel_none_dr1)

loo_all_dr1 = list(loo_allvar_dr1, loo_biasprec_dr1, loo_bias_dr1, loo_prec_dr1, loo_none_dr1)
loo_tab_dr1 = loo_all_dr1 %>%  # extracting elpd_loo estimates
  lapply(., function(x)x[[1]][1,]) %>% 
  do.call(rbind, .) %>% 
  as.data.frame(.) %>% 
  mutate(model = modelnames) %>% 
  rename(elpd_loo = Estimate, 
         elpd_SE = SE)

# creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                     weights=~wts_dr1, # including raked weights in the survey design
                     data=nhsub_dr1)

# calculating loo_wtd
wtd_loo_tab_dr1 = lapply(loo_all_dr1, function(x)loo_wtd(x,svy_rake)) %>%
  do.call(rbind,.) %>%
  data.frame(.) %>%
  mutate(model = modelnames) %>%
  rename(wtdElpd_loo = wtd_elpd_loo)


# getting prediction for MRP - for poststrat table
modelallvar_predict_dr1 = posterior_linpred(nhmodel_allvar_dr1, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelallvar_popnest_dr1 = apply(modelallvar_predict_dr1, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbiasprec_predict_dr1 = posterior_linpred(nhmodel_biasprec_dr1, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbiasprec_popnest_dr1 = apply(modelbiasprec_predict_dr1, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbias_predict_dr1 = posterior_linpred(nhmodel_bias_dr1, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbias_popnest_dr1 = apply(modelbias_predict_dr1, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelprec_predict_dr1 = posterior_linpred(nhmodel_prec_dr1, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelprec_popnest_dr1 = apply(modelprec_predict_dr1, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelnone_predict_dr1 = posterior_linpred(nhmodel_none_dr1, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelnone_popnest_dr1 = apply(modelnone_predict_dr1, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


popnest_all_dr1 = list(modelallvar_popnest_dr1, modelbiasprec_popnest_dr1, modelbias_popnest_dr1, modelprec_popnest_dr1, modelnone_popnest_dr1)

popnest_tab_dr1 = lapply(popnest_all_dr1, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames)

# intervalScr 
alph = 0.1
popnest_tab_dr1 = popnest_tab_dr1 %>%  
  mutate(mean_yObs = mean(as.numeric(nhnew$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)),
         sample = "Dietary")

# urn ---------------------------------------------------------------------
nhmodel_allvar_urn = readRDS(file=here('nhanes/data/urn_allvar.rds'))
nhmodel_biasprec_urn = readRDS(file=here('nhanes/data/urn_biasprec.rds'))
nhmodel_bias_urn = readRDS(file=here('nhanes/data/urn_bias.rds'))
nhmodel_prec_urn = readRDS(file=here('nhanes/data/urn_prec.rds'))
nhmodel_none_urn = readRDS(file=here('nhanes/data/urn_none.rds'))

loo_allvar_urn = loo(nhmodel_allvar_urn)
loo_biasprec_urn = loo(nhmodel_biasprec_urn)
loo_bias_urn = loo(nhmodel_bias_urn)
loo_prec_urn = loo(nhmodel_prec_urn)
loo_none_urn = loo(nhmodel_none_urn)

loo_all_urn = list(loo_allvar_urn, loo_biasprec_urn, loo_bias_urn, loo_prec_urn, loo_none_urn)
loo_tab_urn = loo_all_urn %>%  # extracting elpd_loo estimates
  lapply(., function(x)x[[1]][1,]) %>% 
  do.call(rbind, .) %>% 
  as.data.frame(.) %>% 
  mutate(model = modelnames) %>% 
  rename(elpd_loo = Estimate, 
         elpd_SE = SE)

# creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                     weights=~wts_urn, # including raked weights in the survey design
                     data=nhsub_urn)

# calculating loo_wtd
wtd_loo_tab_urn = lapply(loo_all_urn, function(x)loo_wtd(x,svy_rake)) %>%
  do.call(rbind,.) %>%
  data.frame(.) %>%
  mutate(model = modelnames) %>%
  rename(wtdElpd_loo = wtd_elpd_loo)


# getting prediction for MRP - for poststrat table
modelallvar_predict_urn = posterior_linpred(nhmodel_allvar_urn, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelallvar_popnest_urn = apply(modelallvar_predict_urn, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbiasprec_predict_urn = posterior_linpred(nhmodel_biasprec_urn, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbiasprec_popnest_urn = apply(modelbiasprec_predict_urn, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbias_predict_urn = posterior_linpred(nhmodel_bias_urn, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbias_popnest_urn = apply(modelbias_predict_urn, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelprec_predict_urn = posterior_linpred(nhmodel_prec_urn, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelprec_popnest_urn = apply(modelprec_predict_urn, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelnone_predict_urn = posterior_linpred(nhmodel_none_urn, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelnone_popnest_urn = apply(modelnone_predict_urn, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


popnest_all_urn = list(modelallvar_popnest_urn, modelbiasprec_popnest_urn, modelbias_popnest_urn, modelprec_popnest_urn, modelnone_popnest_urn)

popnest_tab_urn = lapply(popnest_all_urn, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames)

# intervalScr 
alph = 0.1
popnest_tab_urn = popnest_tab_urn %>%  
  mutate(mean_yObs = mean(as.numeric(nhnew$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)),
         sample = "Environ. A")

# Fasting -----------------------------------------------------------------
nhmodel_allvar_fas = readRDS(file=here('nhanes/data/fas_allvar.rds'))
nhmodel_biasprec_fas = readRDS(file=here('nhanes/data/fas_biasprec.rds'))
nhmodel_bias_fas = readRDS(file=here('nhanes/data/fas_bias.rds'))
nhmodel_prec_fas = readRDS(file=here('nhanes/data/fas_prec.rds'))
nhmodel_none_fas = readRDS(file=here('nhanes/data/fas_none.rds'))

loo_allvar_fas = loo(nhmodel_allvar_fas)
loo_biasprec_fas = loo(nhmodel_biasprec_fas)
loo_bias_fas = loo(nhmodel_bias_fas)
loo_prec_fas = loo(nhmodel_prec_fas)
loo_none_fas = loo(nhmodel_none_fas)

loo_all_fas = list(loo_allvar_fas, loo_biasprec_fas, loo_bias_fas, loo_prec_fas, loo_none_fas)
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

modelnone_predict_fas = posterior_linpred(nhmodel_none_fas, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelnone_popnest_fas = apply(modelnone_predict_fas, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


popnest_all_fas = list(modelallvar_popnest_fas, modelbiasprec_popnest_fas, modelbias_popnest_fas, modelprec_popnest_fas, modelnone_popnest_fas)

popnest_tab_fas = lapply(popnest_all_fas, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames)

# intervalScr 
alph = 0.1
popnest_tab_fas = popnest_tab_fas %>%  
  mutate(mean_yObs = mean(as.numeric(nhnew$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)),
         sample = "Fasting")


# mrp plot ----------------------------------------------------------------
# combine all four together
join1_dr1 = left_join(loo_tab_dr1, wtd_loo_tab_dr1, by = "model")
popn_all_tab_dr1 = left_join(join1_dr1, popnest_tab_dr1, by="model") %>% 
  mutate(MRP_intScr_scaled = (MRP_intervalScr - min(MRP_intervalScr)) / (max(MRP_intervalScr) - min(MRP_intervalScr)))

join1_voc = left_join(loo_tab_voc, wtd_loo_tab_voc, by = "model")
popn_all_tab_voc = left_join(join1_voc, popnest_tab_voc, by="model") %>% 
  mutate(MRP_intScr_scaled = (MRP_intervalScr - min(MRP_intervalScr)) / (max(MRP_intervalScr) - min(MRP_intervalScr)))

join1_urn = left_join(loo_tab_urn, wtd_loo_tab_urn, by = "model")
popn_all_tab_urn = left_join(join1_urn, popnest_tab_urn, by="model") %>% 
  mutate(MRP_intScr_scaled = (MRP_intervalScr - min(MRP_intervalScr)) / (max(MRP_intervalScr) - min(MRP_intervalScr)))

join1_fas = left_join(loo_tab_fas, wtd_loo_tab_fas, by = "model")
popn_all_tab_fas = left_join(join1_fas, popnest_tab_fas, by="model") %>% 
  mutate(MRP_intScr_scaled = (MRP_intervalScr - min(MRP_intervalScr)) / (max(MRP_intervalScr) - min(MRP_intervalScr)))

shape_base  = c(21, 22, 24, 23, 25)
colour_palette_var_base  =  c(`All variables` =  "#4477AA",
                              `Bias-precision` = "#88CCEE", 
                              `Bias-only` = "#CC6677",
                              `Precision-only` = "#228833", 
                              `Irrelevant` = "#BBBBBB")
bd_col = c(`All variables` =  "black",
           `Bias-precision` = "black", 
           `Bias-only` = "black",
           `Precision-only` = "black", 
           `Irrelevant` = "black")

show_col(colorblind_pal()(8))
colour_palette_var_base2 = c('#E69F00', '#009E73', '#F0E442', '#D55E00')


( g1 = rbind(popn_all_tab_dr1, popn_all_tab_urn, popn_all_tab_voc, popn_all_tab_fas) %>% 
    mutate(model = case_when(model == 'allvar' ~ "All variables",
                             model == 'biasprec' ~ "Bias-precision",
                             model == 'bias' ~ "Bias-only",
                             model == 'prec' ~ "Precision-only",
                             model == 'none' ~ "Irrelevant")) %>% 
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

ggsave(g1, width=14, height=8, file=here("nhanes/figures/elpd_MRP_all_new.png"))



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
pred_allvar_popnInd_voc = posterior_predict(nhmodel_allvar_voc, newdata = nhnew)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelallvar_pred_popnInd_voc[i] = mean(pred_allvar_popnInd_voc[,i] == nhnew$high_bp[i])
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
pred_biasprec_popnInd_voc = posterior_predict(nhmodel_biasprec_voc, newdata = nhnew)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelbiasprec_pred_popnInd_voc[i] = mean(pred_biasprec_popnInd_voc[,i] == nhnew$high_bp[i])
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
pred_bias_popnInd_voc = posterior_predict(nhmodel_bias_voc, newdata = nhnew)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelbias_pred_popnInd_voc[i] = mean(pred_bias_popnInd_voc[,i] == nhnew$high_bp[i])
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
pred_prec_popnInd_voc = posterior_predict(nhmodel_prec_voc, newdata = nhnew)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelprec_pred_popnInd_voc[i] = mean(pred_prec_popnInd_voc[,i] == nhnew$high_bp[i])
}
modelprec_pred_popnInd_voc = modelprec_pred_popnInd_voc %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# none - sample
pred_none_sampInd_voc = posterior_predict(nhmodel_none_voc, newdata = nhsub_voc)  # getting outcome estimate for each sample indv
modelnone_pred_sampInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelnone_pred_sampInd_voc[i] = mean(pred_none_sampInd_voc[,i] == nhsub_voc$high_bp[i])
}
modelnone_pred_sampInd_voc = modelnone_pred_sampInd_voc %>% 
  as_tibble %>% 
  mutate(model="none") %>% 
  rename(prob_pred_out = value)
# none - popn
pred_none_popnInd_voc = posterior_predict(nhmodel_none_voc, newdata = nhnew)  # getting outcome estimate for each sample indv
modelnone_pred_popnInd_voc = matrix(NA)
for (i in 1:nrow(nhsub_voc)){
  modelnone_pred_popnInd_voc[i] = mean(pred_none_popnInd_voc[,i] == nhnew$high_bp[i])
}
modelnone_pred_popnInd_voc = modelnone_pred_popnInd_voc %>% 
  as_tibble %>% 
  mutate(model="none") %>% 
  rename(prob_pred_out = value)

pred_sampInd_voc = list(modelallvar_pred_sampInd_voc, modelbiasprec_pred_sampInd_voc,
                        modelbias_pred_sampInd_voc, modelprec_pred_sampInd_voc, modelnone_pred_sampInd_voc) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 0,
         sample = "VOC") 

pred_popnInd_voc = list(modelallvar_pred_popnInd_voc, modelbiasprec_pred_popnInd_voc, 
                        modelbias_pred_popnInd_voc, modelprec_pred_popnInd_voc, modelnone_pred_popnInd_voc) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 1,
         sample = "VOC") 

# dr1 ---------------------------------------------------------------------
pred_allvar_sampInd_dr1 = posterior_predict(nhmodel_allvar_dr1, newdata = nhsub_dr1)  # getting outcome estimate for each sample indv

modelallvar_pred_sampInd_dr1 = matrix(NA)
for (i in 1:nrow(nhsub_dr1)){
  modelallvar_pred_sampInd_dr1[i] = mean(pred_allvar_sampInd_dr1[,i] == nhsub_dr1$high_bp[i])
}
modelallvar_pred_sampInd_dr1 = modelallvar_pred_sampInd_dr1 %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# all var - popn
pred_allvar_popnInd_dr1 = posterior_predict(nhmodel_allvar_dr1, newdata = nhnew)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd_dr1 = matrix(NA)
for (i in 1:nrow(nhsub_dr1)){
  modelallvar_pred_popnInd_dr1[i] = mean(pred_allvar_popnInd_dr1[,i] == nhnew$high_bp[i])
}
modelallvar_pred_popnInd_dr1 = modelallvar_pred_popnInd_dr1 %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# biasprec - sample
pred_biasprec_sampInd_dr1 = posterior_predict(nhmodel_biasprec_dr1, newdata = nhsub_dr1)  # getting outcome estimate for each sample indv
modelbiasprec_pred_sampInd_dr1 = matrix(NA)
for (i in 1:nrow(nhsub_dr1)){
  modelbiasprec_pred_sampInd_dr1[i] = mean(pred_biasprec_sampInd_dr1[,i] == nhsub_dr1$high_bp[i])
}
modelbiasprec_pred_sampInd_dr1 = modelbiasprec_pred_sampInd_dr1 %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# biasprec - popn
pred_biasprec_popnInd_dr1 = posterior_predict(nhmodel_biasprec_dr1, newdata = nhnew)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd_dr1 = matrix(NA)
for (i in 1:nrow(nhsub_dr1)){
  modelbiasprec_pred_popnInd_dr1[i] = mean(pred_biasprec_popnInd_dr1[,i] == nhnew$high_bp[i])
}
modelbiasprec_pred_popnInd_dr1 = modelbiasprec_pred_popnInd_dr1 %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# bias - sample
pred_bias_sampInd_dr1 = posterior_predict(nhmodel_bias_dr1, newdata = nhsub_dr1)  # getting outcome estimate for each sample indv
modelbias_pred_sampInd_dr1 = matrix(NA)
for (i in 1:nrow(nhsub_dr1)){
  modelbias_pred_sampInd_dr1[i] = mean(pred_bias_sampInd_dr1[,i] == nhsub_dr1$high_bp[i])
}
modelbias_pred_sampInd_dr1 = modelbias_pred_sampInd_dr1 %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# bias - popn
pred_bias_popnInd_dr1 = posterior_predict(nhmodel_bias_dr1, newdata = nhnew)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd_dr1 = matrix(NA)
for (i in 1:nrow(nhsub_dr1)){
  modelbias_pred_popnInd_dr1[i] = mean(pred_bias_popnInd_dr1[,i] == nhnew$high_bp[i])
}
modelbias_pred_popnInd_dr1 = modelbias_pred_popnInd_dr1 %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# precision - sample
pred_prec_sampInd_dr1 = posterior_predict(nhmodel_prec_dr1, newdata = nhsub_dr1)  # getting outcome estimate for each sample indv
modelprec_pred_sampInd_dr1 = matrix(NA)
for (i in 1:nrow(nhsub_dr1)){
  modelprec_pred_sampInd_dr1[i] = mean(pred_prec_sampInd_dr1[,i] == nhsub_dr1$high_bp[i])
}
modelprec_pred_sampInd_dr1 = modelprec_pred_sampInd_dr1 %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)
# precision - popn
pred_prec_popnInd_dr1 = posterior_predict(nhmodel_prec_dr1, newdata = nhnew)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd_dr1 = matrix(NA)
for (i in 1:nrow(nhsub_dr1)){
  modelprec_pred_popnInd_dr1[i] = mean(pred_prec_popnInd_dr1[,i] == nhnew$high_bp[i])
}
modelprec_pred_popnInd_dr1 = modelprec_pred_popnInd_dr1 %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# none - sample
pred_none_sampInd_dr1 = posterior_predict(nhmodel_none_dr1, newdata = nhsub_dr1)  # getting outcome estimate for each sample indv
modelnone_pred_sampInd_dr1 = matrix(NA)
for (i in 1:nrow(nhsub_dr1)){
  modelnone_pred_sampInd_dr1[i] = mean(pred_none_sampInd_dr1[,i] == nhsub_dr1$high_bp[i])
}
modelnone_pred_sampInd_dr1 = modelnone_pred_sampInd_dr1 %>% 
  as_tibble %>% 
  mutate(model="none") %>% 
  rename(prob_pred_out = value)
# none - popn
pred_none_popnInd_dr1 = posterior_predict(nhmodel_none_dr1, newdata = nhnew)  # getting outcome estimate for each sample indv
modelnone_pred_popnInd_dr1 = matrix(NA)
for (i in 1:nrow(nhsub_dr1)){
  modelnone_pred_popnInd_dr1[i] = mean(pred_none_popnInd_dr1[,i] == nhnew$high_bp[i])
}
modelnone_pred_popnInd_dr1 = modelnone_pred_popnInd_dr1 %>% 
  as_tibble %>% 
  mutate(model="none") %>% 
  rename(prob_pred_out = value)

pred_sampInd_dr1 = list(modelallvar_pred_sampInd_dr1, modelbiasprec_pred_sampInd_dr1,
                        modelbias_pred_sampInd_dr1, modelprec_pred_sampInd_dr1, modelnone_pred_sampInd_dr1) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 0,
         sample = "Dietary") 

pred_popnInd_dr1 = list(modelallvar_pred_popnInd_dr1, modelbiasprec_pred_popnInd_dr1, 
                        modelbias_pred_popnInd_dr1, modelprec_pred_popnInd_dr1, modelnone_pred_popnInd_dr1) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 1,
         sample = "Dietary") 

# urn ---------------------------------------------------------------------
pred_allvar_sampInd_urn = posterior_predict(nhmodel_allvar_urn, newdata = nhsub_urn)  # getting outcome estimate for each sample indv

modelallvar_pred_sampInd_urn = matrix(NA)
for (i in 1:nrow(nhsub_urn)){
  modelallvar_pred_sampInd_urn[i] = mean(pred_allvar_sampInd_urn[,i] == nhsub_urn$high_bp[i])
}
modelallvar_pred_sampInd_urn = modelallvar_pred_sampInd_urn %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# all var - popn
pred_allvar_popnInd_urn = posterior_predict(nhmodel_allvar_urn, newdata = nhnew)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd_urn = matrix(NA)
for (i in 1:nrow(nhsub_urn)){
  modelallvar_pred_popnInd_urn[i] = mean(pred_allvar_popnInd_urn[,i] == nhnew$high_bp[i])
}
modelallvar_pred_popnInd_urn = modelallvar_pred_popnInd_urn %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# biasprec - sample
pred_biasprec_sampInd_urn = posterior_predict(nhmodel_biasprec_urn, newdata = nhsub_urn)  # getting outcome estimate for each sample indv
modelbiasprec_pred_sampInd_urn = matrix(NA)
for (i in 1:nrow(nhsub_urn)){
  modelbiasprec_pred_sampInd_urn[i] = mean(pred_biasprec_sampInd_urn[,i] == nhsub_urn$high_bp[i])
}
modelbiasprec_pred_sampInd_urn = modelbiasprec_pred_sampInd_urn %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# biasprec - popn
pred_biasprec_popnInd_urn = posterior_predict(nhmodel_biasprec_urn, newdata = nhnew)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd_urn = matrix(NA)
for (i in 1:nrow(nhsub_urn)){
  modelbiasprec_pred_popnInd_urn[i] = mean(pred_biasprec_popnInd_urn[,i] == nhnew$high_bp[i])
}
modelbiasprec_pred_popnInd_urn = modelbiasprec_pred_popnInd_urn %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# bias - sample
pred_bias_sampInd_urn = posterior_predict(nhmodel_bias_urn, newdata = nhsub_urn)  # getting outcome estimate for each sample indv
modelbias_pred_sampInd_urn = matrix(NA)
for (i in 1:nrow(nhsub_urn)){
  modelbias_pred_sampInd_urn[i] = mean(pred_bias_sampInd_urn[,i] == nhsub_urn$high_bp[i])
}
modelbias_pred_sampInd_urn = modelbias_pred_sampInd_urn %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# bias - popn
pred_bias_popnInd_urn = posterior_predict(nhmodel_bias_urn, newdata = nhnew)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd_urn = matrix(NA)
for (i in 1:nrow(nhsub_urn)){
  modelbias_pred_popnInd_urn[i] = mean(pred_bias_popnInd_urn[,i] == nhnew$high_bp[i])
}
modelbias_pred_popnInd_urn = modelbias_pred_popnInd_urn %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# precision - sample
pred_prec_sampInd_urn = posterior_predict(nhmodel_prec_urn, newdata = nhsub_urn)  # getting outcome estimate for each sample indv
modelprec_pred_sampInd_urn = matrix(NA)
for (i in 1:nrow(nhsub_urn)){
  modelprec_pred_sampInd_urn[i] = mean(pred_prec_sampInd_urn[,i] == nhsub_urn$high_bp[i])
}
modelprec_pred_sampInd_urn = modelprec_pred_sampInd_urn %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)
# precision - popn
pred_prec_popnInd_urn = posterior_predict(nhmodel_prec_urn, newdata = nhnew)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd_urn = matrix(NA)
for (i in 1:nrow(nhsub_urn)){
  modelprec_pred_popnInd_urn[i] = mean(pred_prec_popnInd_urn[,i] == nhnew$high_bp[i])
}
modelprec_pred_popnInd_urn = modelprec_pred_popnInd_urn %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# none - sample
pred_none_sampInd_urn = posterior_predict(nhmodel_none_urn, newdata = nhsub_urn)  # getting outcome estimate for each sample indv
modelnone_pred_sampInd_urn = matrix(NA)
for (i in 1:nrow(nhsub_urn)){
  modelnone_pred_sampInd_urn[i] = mean(pred_none_sampInd_urn[,i] == nhsub_urn$high_bp[i])
}
modelnone_pred_sampInd_urn = modelnone_pred_sampInd_urn %>% 
  as_tibble %>% 
  mutate(model="none") %>% 
  rename(prob_pred_out = value)
# none - popn
pred_none_popnInd_urn = posterior_predict(nhmodel_none_urn, newdata = nhnew)  # getting outcome estimate for each sample indv
modelnone_pred_popnInd_urn = matrix(NA)
for (i in 1:nrow(nhsub_urn)){
  modelnone_pred_popnInd_urn[i] = mean(pred_none_popnInd_urn[,i] == nhnew$high_bp[i])
}
modelnone_pred_popnInd_urn = modelnone_pred_popnInd_urn %>% 
  as_tibble %>% 
  mutate(model="none") %>% 
  rename(prob_pred_out = value)

pred_sampInd_urn = list(modelallvar_pred_sampInd_urn, modelbiasprec_pred_sampInd_urn,
                        modelbias_pred_sampInd_urn, modelprec_pred_sampInd_urn, modelnone_pred_sampInd_urn) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 0,
         sample = "Environ. A") 

pred_popnInd_urn = list(modelallvar_pred_popnInd_urn, modelbiasprec_pred_popnInd_urn, 
                        modelbias_pred_popnInd_urn, modelprec_pred_popnInd_urn, modelnone_pred_popnInd_urn) %>% 
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
pred_allvar_popnInd_fas = posterior_predict(nhmodel_allvar_fas, newdata = nhnew)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelallvar_pred_popnInd_fas[i] = mean(pred_allvar_popnInd_fas[,i] == nhnew$high_bp[i])
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
pred_biasprec_popnInd_fas = posterior_predict(nhmodel_biasprec_fas, newdata = nhnew)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelbiasprec_pred_popnInd_fas[i] = mean(pred_biasprec_popnInd_fas[,i] == nhnew$high_bp[i])
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
pred_bias_popnInd_fas = posterior_predict(nhmodel_bias_fas, newdata = nhnew)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelbias_pred_popnInd_fas[i] = mean(pred_bias_popnInd_fas[,i] == nhnew$high_bp[i])
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
pred_prec_popnInd_fas = posterior_predict(nhmodel_prec_fas, newdata = nhnew)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelprec_pred_popnInd_fas[i] = mean(pred_prec_popnInd_fas[,i] == nhnew$high_bp[i])
}
modelprec_pred_popnInd_fas = modelprec_pred_popnInd_fas %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# none - sample
pred_none_sampInd_fas = posterior_predict(nhmodel_none_fas, newdata = nhsub_fas)  # getting outcome estimate for each sample indv
modelnone_pred_sampInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelnone_pred_sampInd_fas[i] = mean(pred_none_sampInd_fas[,i] == nhsub_fas$high_bp[i])
}
modelnone_pred_sampInd_fas = modelnone_pred_sampInd_fas %>% 
  as_tibble %>% 
  mutate(model="none") %>% 
  rename(prob_pred_out = value)
# none - popn
pred_none_popnInd_fas = posterior_predict(nhmodel_none_fas, newdata = nhnew)  # getting outcome estimate for each sample indv
modelnone_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelnone_pred_popnInd_fas[i] = mean(pred_none_popnInd_fas[,i] == nhnew$high_bp[i])
}
modelnone_pred_popnInd_fas = modelnone_pred_popnInd_fas %>% 
  as_tibble %>% 
  mutate(model="none") %>% 
  rename(prob_pred_out = value)

pred_sampInd_fas = list(modelallvar_pred_sampInd_fas, modelbiasprec_pred_sampInd_fas,
                        modelbias_pred_sampInd_fas, modelprec_pred_sampInd_fas, modelnone_pred_sampInd_fas) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 0,
         sample = "Fasting") 

pred_popnInd_fas = list(modelallvar_pred_popnInd_fas, modelbiasprec_pred_popnInd_fas, 
                        modelbias_pred_popnInd_fas, modelprec_pred_popnInd_fas, modelnone_pred_popnInd_fas) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 1,
         sample = "Fasting") 

j1_voc = left_join(pred_sampInd_voc, loo_tab_voc, by="model")
t1_voc = left_join(j1_voc, wtd_loo_tab_voc, by="model")

j2_voc = left_join(pred_popnInd_voc, loo_tab_voc, by="model")
t2_voc = left_join(j2_voc, wtd_loo_tab_voc, by="model")

j1_dr1 = left_join(pred_sampInd_dr1, loo_tab_dr1, by="model")
t1_dr1 =left_join(j1_dr1, wtd_loo_tab_dr1, by="model")

j2_dr1 = left_join(pred_popnInd_dr1, loo_tab_dr1, by="model")
t2_dr1 = left_join(j2_dr1, wtd_loo_tab_dr1, by="model")

j1_urn = left_join(pred_sampInd_urn, loo_tab_urn, by="model")
t1_urn = left_join(j1_urn, wtd_loo_tab_urn, by="model")

j2_urn = left_join(pred_popnInd_urn, loo_tab_urn, by="model")
t2_urn = left_join(j2_urn, wtd_loo_tab_urn, by="model")

j1_fas = left_join(pred_sampInd_fas, loo_tab_fas, by="model")
t1_fas =left_join(j1_fas, wtd_loo_tab_fas, by="model")

j2_fas = left_join(pred_popnInd_fas, loo_tab_fas, by="model")
t2_fas =left_join(j2_fas, wtd_loo_tab_fas, by="model") 

# individuals plot --------------------------------------------------------------------
(g2 = rbind(t1_voc, t2_voc, t1_dr1, t2_dr1, 
            t1_urn, t2_urn, t1_fas, t2_fas) %>%
   mutate(model = case_when(model == 'allvar' ~ "All variables",
                            model == 'biasprec' ~ "Bias-precision",
                            model == 'bias' ~ "Bias-only",
                            model == 'prec' ~ "Precision-only",
                            model == 'none' ~ "Irrelevant")) %>% 
   mutate(popnInd = factor(popnInd),
          popnInd = fct_recode(popnInd, `Sample` = "0", `Population` = "1"),
          elpd_loo_std = (elpd_loo - min(elpd_loo)) / (max(elpd_loo) - min(elpd_loo)),
          wtdElpd_loo_std = (wtdElpd_loo - min(wtdElpd_loo)) / (max(wtdElpd_loo) - min(wtdElpd_loo))) %>% 
   pivot_longer(cols = c("elpd_loo","wtdElpd_loo"), names_to = "type", values_to = "model_score") %>%
   mutate(type = factor(type),
          type = fct_recode(type, `PSIS-LOO` = "elpd_loo", `WTD-PSIS-LOO` = "wtdElpd_loo"))  %>% 
   ggplot(., aes(x = model_score, y = 1-mean_prob_pred_out, colour = sample, shape = factor(model), fill = sample, group=sample)) +
   geom_line(alpha=0.6) +
   geom_point(size=4, alpha = .7) + 
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

ggsave(g2, width=14, height=8, file=here("nhanes/figures/elpd_indv_pred_outcome_new.png"))

