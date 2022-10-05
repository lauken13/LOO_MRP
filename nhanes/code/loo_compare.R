## comparing LOO, wtd LOO and intervalScr
## calculating loo
library(magrittr) # set_rownames()
library(here)
library(loo)
library(tidyverse)
library(survey)
library(brms)
library(ggplot2)

nhdata = readRDS(file=here("nhanes/data/clean_data_dr1.rds"))
nhsub = readRDS(file=here("nhanes/data/nhsub_dr1.rds"))

source(here('02-super popn approach/functions.R'))

nhmodel_allvar = readRDS(file=here('nhanes/data/nhmodel_allvar_dr1.rds'))
nhmodel_biasprec = readRDS(file=here('nhanes/data/nhmodel_biasprec_dr1.rds'))
nhmodel_bias = readRDS(file=here('nhanes/data/nhmodel_bias_dr1.rds'))
nhmodel_prec = readRDS(file=here('nhanes/data/nhmodel_prec_dr1.rds'))
nhmodel_none = readRDS(file=here('nhanes/data/nhmodel_none_dr1.rds'))

loo_allvar = loo(nhmodel_allvar)
loo_biasprec = loo(nhmodel_biasprec)
loo_bias = loo(nhmodel_bias)
loo_prec = loo(nhmodel_prec)
loo_none = loo(nhmodel_none)


loo_all = list(loo_allvar, loo_biasprec, loo_bias, loo_prec, loo_none)
modelnames =  c('allvar', 'biasprec', 'bias', 'prec', 'none')
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
         elpd_prec = loo_prec$pointwise[,1],
         elpd_none = loo_none$pointwise[,1])

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

modelnone_predict = posterior_linpred(nhmodel_none, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelnone_popnest = apply(modelnone_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


popnest_all = list(modelallvar_popnest, modelbiasprec_popnest, modelbias_popnest, modelprec_popnest, modelnone_popnest)

popnest_tab = lapply(popnest_all, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames)

# intervalScr 
alph = 0.1
popnest_tab = popnest_tab %>%  
  mutate(mean_yObs = mean(as.numeric(nhdata$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)))

# combine all three together
join1 = left_join(loo_tab, wtd_loo_tab, by = "model")
popn_all_tab = left_join(loo_tab_fas, popnest_tab_fas, by="model")

( g1 = popn_all_tab %>% 
    # mutate(elpd_loo_std = (elpd_loo - min(elpd_loo)) / (max(elpd_loo) - min(elpd_loo)),
           # wtdElpd_loo_std = (wtdElpd_loo - min(wtdElpd_loo)) / (max(wtdElpd_loo) - min(wtdElpd_loo))) %>% 
    # pivot_longer(cols = c("elpd_loo_std","wtdElpd_loo_std"), names_to = "type", values_to = "model_score") %>%
    # mutate(type = factor(type),
           # type = fct_recode(type, `PSIS-LOO` = "elpd_loo_std", `WTD-PSIS-LOO` = "wtdElpd_loo_std"))  %>% 
  ggplot(., aes(x = elpd_loo, y = MRP_intervalScr)) +
  # facet_grid(~type, scales = "free")+
  geom_point(size=3, alpha = .5, aes(colour = model)) + 
  theme_bw(base_size = 15)  +
    ylab("Interval score for MRP estimates") +
    xlab("Model score") + 
    ggtitle('MRP interval score'))

ggsave(g1, file=here("nhanes/figures/elpd_MRP_dr1.png"))


## individual level
# all var - sample
pred_allvar_sampInd = posterior_predict(nhmodel_allvar, newdata = nhsub)  # getting outcome estimate for each sample indv
  
modelallvar_pred_sampInd = matrix(NA)
for (i in 1:nrow(nhsub)){
  modelallvar_pred_sampInd[i] = mean(pred_allvar_sampInd[,i] == nhsub$high_bp[i])
}
modelallvar_pred_sampInd = modelallvar_pred_sampInd %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# all var - popn
pred_allvar_popnInd = posterior_predict(nhmodel_allvar, newdata = nhdata)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd = matrix(NA)
for (i in 1:nrow(nhsub)){
  modelallvar_pred_popnInd[i] = mean(pred_allvar_popnInd[,i] == nhdata$high_bp[i])
}
modelallvar_pred_popnInd = modelallvar_pred_popnInd %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# biasprec - sample
pred_biasprec_sampInd = posterior_predict(nhmodel_biasprec, newdata = nhsub)  # getting outcome estimate for each sample indv
modelbiasprec_pred_sampInd = matrix(NA)
for (i in 1:nrow(nhsub)){
  modelbiasprec_pred_sampInd[i] = mean(pred_biasprec_sampInd[,i] == nhsub$high_bp[i])
}
modelbiasprec_pred_sampInd = modelbiasprec_pred_sampInd %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)
# biasprec - popn
pred_biasprec_popnInd = posterior_predict(nhmodel_biasprec, newdata = nhdata)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd = matrix(NA)
for (i in 1:nrow(nhsub)){
  modelbiasprec_pred_popnInd[i] = mean(pred_biasprec_popnInd[,i] == nhdata$high_bp[i])
}
modelbiasprec_pred_popnInd = modelbiasprec_pred_popnInd %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# bias - sample
pred_bias_sampInd = posterior_predict(nhmodel_bias, newdata = nhsub)  # getting outcome estimate for each sample indv
modelbias_pred_sampInd = matrix(NA)
for (i in 1:nrow(nhsub)){
  modelbias_pred_sampInd[i] = mean(pred_bias_sampInd[,i] == nhsub$high_bp[i])
}
modelbias_pred_sampInd = modelbias_pred_sampInd %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)
# bias - popn
pred_bias_popnInd = posterior_predict(nhmodel_bias, newdata = nhdata)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd = matrix(NA)
for (i in 1:nrow(nhsub)){
  modelbias_pred_popnInd[i] = mean(pred_bias_popnInd[,i] == nhdata$high_bp[i])
}
modelbias_pred_popnInd = modelbias_pred_popnInd %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# precision - sample
pred_prec_sampInd = posterior_predict(nhmodel_prec, newdata = nhsub)  # getting outcome estimate for each sample indv
modelprec_pred_sampInd = matrix(NA)
for (i in 1:nrow(nhsub)){
  modelprec_pred_sampInd[i] = mean(pred_prec_sampInd[,i] == nhsub$high_bp[i])
}
modelprec_pred_sampInd = modelprec_pred_sampInd %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)
# precision - popn
pred_prec_popnInd = posterior_predict(nhmodel_prec, newdata = nhdata)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd = matrix(NA)
for (i in 1:nrow(nhsub)){
  modelprec_pred_popnInd[i] = mean(pred_prec_popnInd[,i] == nhdata$high_bp[i])
}
modelprec_pred_popnInd = modelprec_pred_popnInd %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# none - sample
pred_none_sampInd = posterior_predict(nhmodel_none, newdata = nhsub)  # getting outcome estimate for each sample indv
modelnone_pred_sampInd = matrix(NA)
for (i in 1:nrow(nhsub)){
  modelnone_pred_sampInd[i] = mean(pred_none_sampInd[,i] == nhsub$high_bp[i])
}
modelnone_pred_sampInd = modelnone_pred_sampInd %>% 
  as_tibble %>% 
  mutate(model="none") %>% 
  rename(prob_pred_out = value)
# none - popn
pred_none_popnInd = posterior_predict(nhmodel_none, newdata = nhdata)  # getting outcome estimate for each sample indv
modelnone_pred_popnInd = matrix(NA)
for (i in 1:nrow(nhsub)){
  modelnone_pred_popnInd[i] = mean(pred_none_popnInd[,i] == nhdata$high_bp[i])
}
modelnone_pred_popnInd = modelnone_pred_popnInd %>% 
  as_tibble %>% 
  mutate(model="none") %>% 
  rename(prob_pred_out = value)


pred_sampInd = list(modelallvar_pred_sampInd, modelbiasprec_pred_sampInd,
                    modelbias_pred_sampInd, modelprec_pred_sampInd, modelnone_pred_sampInd) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 0) 

pred_popnInd = list(modelallvar_pred_popnInd, modelbiasprec_pred_popnInd, 
                    modelbias_pred_popnInd, modelprec_pred_popnInd, modelnone_pred_popnInd) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 1) 

t1 = left_join(pred_sampInd, loo_tab, by="model")
t2 = left_join(t1, wtd_loo_tab, by="model")

t3 = left_join(pred_popnInd, loo_tab, by="model")
t4 = left_join(t3, wtd_loo_tab, by="model")

(g2 = union(t2, t4) %>%
    mutate(elpd_loo_std = (elpd_loo - min(elpd_loo)) / (max(elpd_loo) - min(elpd_loo)),
           wtdElpd_loo_std = (wtdElpd_loo - min(wtdElpd_loo)) / (max(wtdElpd_loo) - min(wtdElpd_loo))) %>% 
    pivot_longer(cols = c("elpd_loo_std","wtdElpd_loo_std"), names_to = "type", values_to = "model_score") %>%
    mutate(type = factor(type),
           type = fct_recode(type, `PSIS-LOO` = "elpd_loo_std", `WTD-PSIS-LOO` = "wtdElpd_loo_std"))  %>% 
    mutate(popnInd = factor(popnInd),
           popnInd = fct_recode(popnInd, `Sample` = "0", `Population` = "1")) %>% 
  ggplot(., aes(x = model_score, y = 1-mean_prob_pred_out)) +
  geom_point(size=3, alpha = .5, aes(colour = model)) + 
  facet_grid(popnInd~type, scales = "free") +
  theme_bw(base_size = 15) +
  ggtitle('Individual mean prediction of outcome') +
    ylab('1 - mean of prediction of outcome') +
    xlab("Model score"))

ggsave(g2, file=here("nhanes/figures/elpd_indv_pred_outcome_dr1.png"))





