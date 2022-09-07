## comparing LOO, wtd LOO and intervalScr
## calculating loo
library(magrittr) # set_rownames()
library(here)
library(loo)
library(tidyverse)
library(survey)
library(brms)
library(ggplot2)

source(here('02-super popn approach/functions.R'))
nhdata = readRDS(file=here("nhanes/data/clean_data.rds"))
nhsub = readRDS(file=here("nhanes/data/nhsub.rds"))

nhmodel_allvar = readRDS(file=here('nhanes/data/nhmodel_allvar.rds'))
nhmodel_biasprec = readRDS(file=here('nhanes/data/nhmodel_biasprec.rds'))
nhmodel_bias = readRDS(file=here('nhanes/data/nhmodel_bias.rds'))
nhmodel_prec = readRDS(file=here('nhanes/data/nhmodel_prec.rds'))

loo_allvar = loo(nhmodel_allvar)
loo_biasprec = loo(nhmodel_biasprec)
loo_bias = loo(nhmodel_bias)
loo_prec = loo(nhmodel_prec)

loo_all = list(loo_allvar, loo_biasprec, loo_bias, loo_prec)
modelnames =  c('allvar', 'biasprec', 'bias', 'prec')
loo_tab = loo_all %>%  # extracting elpd_loo estimates
  lapply(., function(x)x[[1]][1,]) %>% 
  do.call(rbind, .) %>% 
  as.data.frame(.) %>% 
  mutate(model = modelnames) %>% 
  rename(elpd_loo = Estimate, 
         elpd_SE = SE)

# creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                     weights=~wts, # including raked weights in the survey design
                     data=nhsub) 

# calculating loo_wtd
wtd_loo_tab = lapply(loo_all, function(x)loo_wtd(x,svy_rake)) %>% 
  do.call(rbind,.) %>%
  data.frame(.) %>% 
  mutate(model = modelnames) %>% 
  rename(wtdElpd_loo = wtd_elpd_loo)

nhsub = nhsub %>% 
  mutate(elpd_allvar = loo_allvar$pointwise[,1],
         elpd_biasprec = loo_biasprec$pointwise[,1],
         elpd_bias = loo_bias$pointwise[,1],
         elpd_prec = loo_prec$pointwise[,1])

# make MRP estimates
popn_ps = nhdata %>% 
  group_by(age, ethnicity, gender, educ, marital_status, phys_act,   
           overweight, dep_severity, BMI_range, alc_exc, diabetes, trb_sleep,
           pov_level, smk2_exp, smk_tobcig, sodium_lvl, potassium_lvl) %>% 
  summarise(Nj = n()) %>% 
  ungroup() 

summary(popn_ps$Nj)

# getting prediction for MRP - for poststrat table
modelallvar_predict = posterior_linpred(nhmodel_allvar, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelallvar_popnest = apply(modelallvar_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbiasprec_predict = posterior_linpred(nhmodel_biasprec, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbiasprec_popnest = apply(modelbiasprec_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbias_predict = posterior_linpred(nhmodel_bias, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbias_popnest = apply(modelbias_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelprec_predict = posterior_linpred(nhmodel_prec, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelprec_popnest = apply(modelprec_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

popnest_all = list(modelallvar_popnest, modelbiasprec_popnest, modelbias_popnest, modelprec_popnest)

popnest_tab = lapply(popnest_all, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames)

# intervalScr 
alph = 0.1
popnest_tab = popnest_tab %>%  
  mutate(mean_yObs = mean(as.numeric(nhdata$high_bp)-1),
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)))

# combine all three together
join1 = left_join(loo_tab, wtd_loo_tab, by = "model")
popn_all_tab = left_join(join1, popnest_tab, by="model")

( g1 = popn_all_tab %>% 
  pivot_longer(cols = c("elpd_loo","wtdElpd_loo"), names_to = "type", values_to = "model_score") %>%
  ggplot(., aes(x = model_score, y = MRP_intervalScr)) +
  geom_point(size=3, alpha = .5, aes(colour = model)) + 
  facet_grid(~type, scales = "free") + 
  theme_bw(base_size = 15) +
  ggtitle('p-value approach') )

ggsave(g1, file=here("nhanes/figures/elpd_pval.png"))


## individual level
# all var - sample
modelallvar_pred_sampInd = posterior_linpred(nhmodel_allvar, newdata = nhsub, transform = T) %>% # getting model estimate for each sample indv
  apply(., 2, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  set_rownames(c('sampIndX5', 'sampIndX50', 'sampIndX95')) %>% 
  t() %>% as_tibble %>% 
  mutate(model="allvar")
# all var - popn
modelallvar_pred_popnInd = posterior_linpred(nhmodel_allvar, newdata = nhdata, transform = T) %>% # getting model estimate for each popn indv
  apply(., 2, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  set_rownames(c('popnIndX5', 'popnIndX50', 'popnIndX95')) %>% 
  t() %>% as_tibble %>% 
  mutate(model="allvar")

# biasprec - sample
modelbiasprec_pred_sampInd = posterior_linpred(nhmodel_biasprec, newdata = nhsub, transform = T) %>% 
  apply(., 2, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  set_rownames(c('sampIndX5',  'sampIndX50', 'sampIndX95')) %>% 
  t() %>% as_tibble %>% 
  mutate(model="biasprec")
# biasprec - popn
modelbiasprec_pred_popnInd = posterior_linpred(nhmodel_biasprec, newdata = nhdata, transform = T) %>% 
  apply(., 2, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  set_rownames(c('popnIndX5', 'popnIndX50', 'popnIndX95')) %>% 
  t() %>% as_tibble %>% 
  mutate(model="biasprec")

# bias - sample
modelbias_pred_sampInd = posterior_linpred(nhmodel_bias, newdata = nhsub, transform = T)  %>% 
  apply(., 2, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  set_rownames(c('sampIndX5',  'sampIndX50', 'sampIndX95')) %>% 
  t() %>% as_tibble %>% 
  mutate(model="bias")
# bias - popn
modelbias_pred_popnInd = posterior_linpred(nhmodel_bias, newdata = nhdata, transform = T) %>% 
  apply(., 2, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  set_rownames(c('popnIndX5', 'popnIndX50', 'popnIndX95')) %>% 
  t() %>% as_tibble %>% 
  mutate(model="bias")

# precision - sample
modelprec_pred_sampInd = posterior_linpred(nhmodel_prec, newdata = nhsub, transform = T)  %>% 
  apply(., 2, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  set_rownames(c('sampIndX5',  'sampIndX50','sampIndX95')) %>% 
  t() %>% as_tibble %>% 
  mutate(model="prec")
# precision - popn
modelprec_pred_popnInd = posterior_linpred(nhmodel_prec, newdata = nhdata, transform = T)  %>% 
  apply(., 2, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  set_rownames(c('popnIndX5', 'popnIndX50', 'popnIndX95')) %>% 
  t() %>% as_tibble %>% 
  mutate(model="prec")

pred_sampInd = list(modelallvar_pred_sampInd, modelbiasprec_pred_sampInd, modelbias_pred_sampInd, modelprec_pred_sampInd) %>% 
  do.call(rbind, .) %>% 
  mutate(mean_yObs = mean(as.numeric(nhsub$high_bp)-1),
         sampInd_intervalScr = (sampIndX95 - sampIndX5) + 
           ((2 / alph * (sampIndX5 - mean_yObs)) * ifelse(mean_yObs < sampIndX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - sampIndX95)) * ifelse(mean_yObs > sampIndX95, 1, 0)),
         sampInd_bias = sampIndX50 - mean_yObs, 
         sampInd_ci_width = sampIndX95 - sampIndX5) %>% 
  group_by(model) %>% 
  summarise(intervalScr = mean(sampInd_intervalScr),
            mean_bias = mean(sampInd_bias),
            mean_ci_width = mean(sampInd_ci_width)) %>% 
  mutate(popnInd = 0) 

pred_popnInd = list(modelallvar_pred_popnInd, modelbiasprec_pred_popnInd, modelbias_pred_popnInd, modelprec_pred_popnInd) %>% 
  do.call(rbind, .) %>% 
  mutate(mean_yObs = mean(as.numeric(nhdata$high_bp)-1),
         popnInd_intervalScr = (popnIndX95 - popnIndX5) + 
           ((2 / alph * (popnIndX5 - mean_yObs)) * ifelse(mean_yObs < popnIndX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnIndX95)) * ifelse(mean_yObs > popnIndX95, 1, 0)),
         popnInd_bias = popnIndX50 - mean_yObs, 
         popnInd_ci_width = popnIndX95 - popnIndX5) %>% 
  group_by(model) %>% 
  summarise(intervalScr = mean(popnInd_intervalScr),
            mean_bias = mean(popnInd_bias),
            mean_ci_width = mean(popnInd_ci_width)) %>% 
  mutate(popnInd = 1) 

t1 = left_join(pred_sampInd, loo_tab, by="model")
t2 = left_join(t1, wtd_loo_tab, by="model")

t3 = left_join(pred_popnInd, loo_tab, by="model")
t4 = left_join(t3, wtd_loo_tab, by="model")


(g2 = union(t2, t4) %>%
    mutate(popnInd = factor(popnInd),
           popnInd = fct_recode(popnInd, `Sample` = "0", `Population` = "1")) %>% 
  pivot_longer(cols = c("elpd_loo","wtdElpd_loo"), names_to = "type", values_to = "model_score") %>%
  ggplot(., aes(x = model_score, y = mean_bias)) +
  geom_point(size=3, alpha = .5, aes(colour = model)) + 
  facet_grid(popnInd~type, scales = "free") + 
  theme_bw(base_size = 15) +
  ggtitle('individual mean bias - "p-value approach"') )

ggsave(g2, file=here("nhanes/figures/elpd_indv_bias_pval.png"))





