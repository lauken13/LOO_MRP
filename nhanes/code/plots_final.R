## plotting the diff subsample
library(here)
library(loo)
library(tidyverse)
library(brms)
library(survey)
library(cowplot)
source(here('02-super popn approach/functions.R'))
nhfinal = readRDS(file="nhanes/data/nhanes_final_gen.rds")

nhsub_dr2 = nhfinal %>% 
  filter(incl_dr2 == 1) 

nhsub_fas = nhfinal %>% 
  filter(incl_fas == 1) 

nhsub_gen = nhfinal %>% 
  filter(incl_gen == 1) 

# make MRP estimates
popn_ps = nhfinal %>% 
  group_by(age, ethnicity, gender, educ, marital_status, 
           phys_act, overweight, diabetes, trb_sleep, pov_level,
           smk_tobcig, sodium_lvl, elst_status, hiv_test, urn_vol) %>% 
  summarise(Nj = n()) %>% 
  ungroup() 


# dr2 ---------------------------------------------------------------------
nhmodel_allvar_dr2 = readRDS(file=here('nhanes/data/dr2_allvar.rds'))
nhmodel_biasprec_dr2 = readRDS(file=here('nhanes/data/dr2_biasprec.rds'))
nhmodel_bias_dr2 = readRDS(file=here('nhanes/data/dr2_bias.rds'))
nhmodel_prec_dr2 = readRDS(file=here('nhanes/data/dr2_prec.rds'))
nhmodel_inc_dr2 = readRDS(file=here('nhanes/data/dr2_inc.rds'))
nhmodel_ign_dr2 = readRDS(file=here('nhanes/data/dr2_ign.rds'))

loo_allvar_dr2 = loo(nhmodel_allvar_dr2)
loo_biasprec_dr2 = loo(nhmodel_biasprec_dr2)
loo_bias_dr2 = loo(nhmodel_bias_dr2)
loo_prec_dr2 = loo(nhmodel_prec_dr2)
loo_inc_dr2 = loo(nhmodel_inc_dr2)
loo_ign_dr2 = loo(nhmodel_ign_dr2)

loo_all_dr2 = list(loo_allvar_dr2, loo_biasprec_dr2, loo_bias_dr2, loo_prec_dr2, loo_inc_dr2, loo_ign_dr2)
modelnames =  c('allvar', 'biasprec', 'bias', 'prec', 'inc', 'ign')

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


popnest_all_dr2 = list(modelallvar_popnest_dr2, modelbiasprec_popnest_dr2, modelbias_popnest_dr2, 
                       modelprec_popnest_dr2, modelinc_popnest_dr2, modelign_popnest_dr2)

alph = 0.1

popnest_tab_dr2 = lapply(popnest_all_dr2, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames,
         mean_yObs = mean(as.numeric(nhfinal$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) +  # intervalScr 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)),
         sample = "Dietary")

# Fasting -----------------------------------------------------------------
nhmodel_allvar_fas = readRDS(file=here('nhanes/data/fas_allvar.rds'))
nhmodel_biasprec_fas = readRDS(file=here('nhanes/data/fas_biasprec.rds'))
nhmodel_bias_fas = readRDS(file=here('nhanes/data/fas_bias.rds'))
nhmodel_prec_fas = readRDS(file=here('nhanes/data/fas_prec.rds'))
nhmodel_inc_fas = readRDS(file=here('nhanes/data/fas_inc.rds'))
nhmodel_ign_fas = readRDS(file=here('nhanes/data/fas_ign.rds'))

loo_allvar_fas = loo(nhmodel_allvar_fas)
loo_biasprec_fas = loo(nhmodel_biasprec_fas)
loo_bias_fas = loo(nhmodel_bias_fas)
loo_prec_fas = loo(nhmodel_prec_fas)
loo_inc_fas = loo(nhmodel_inc_fas)
loo_ign_fas = loo(nhmodel_ign_fas)

loo_all_fas = list(loo_allvar_fas, loo_biasprec_fas, loo_bias_fas, loo_prec_fas,  loo_inc_fas, loo_ign_fas)
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


popnest_all_fas = list(modelallvar_popnest_fas, modelbiasprec_popnest_fas, modelbias_popnest_fas,
                       modelprec_popnest_fas, modelinc_popnest_fas, modelign_popnest_fas)

popnest_tab_fas = lapply(popnest_all_fas, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames,
         mean_yObs = mean(as.numeric(nhfinal$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)),
         sample = "Fasting")


# Generated -----------------------------------------------------------------
nhmodel_allvar_gen = readRDS(file=here('nhanes/data/gen_allvar.rds'))
nhmodel_biasprec_gen = readRDS(file=here('nhanes/data/gen_biasprec.rds'))
nhmodel_bias_gen = readRDS(file=here('nhanes/data/gen_bias.rds'))
nhmodel_prec_gen = readRDS(file=here('nhanes/data/gen_prec.rds'))
nhmodel_inc_gen = readRDS(file=here('nhanes/data/gen_inc.rds'))
nhmodel_ign_gen = readRDS(file=here('nhanes/data/gen_ign.rds'))

loo_allvar_gen = loo(nhmodel_allvar_gen)
loo_biasprec_gen = loo(nhmodel_biasprec_gen)
loo_bias_gen = loo(nhmodel_bias_gen)
loo_prec_gen = loo(nhmodel_prec_gen)
loo_inc_gen = loo(nhmodel_inc_gen)
loo_ign_gen = loo(nhmodel_ign_gen)

loo_all_gen = list(loo_allvar_gen, loo_biasprec_gen, loo_bias_gen, loo_prec_gen,  loo_inc_gen, loo_ign_gen)
loo_tab_gen = loo_all_gen %>%  # extracting elpd_loo estimates
  lapply(., function(x)x[[1]][1,]) %>% 
  do.call(rbind, .) %>% 
  as.data.frame(.) %>% 
  mutate(model = modelnames) %>% 
  rename(elpd_loo = Estimate, 
         elpd_SE = SE)

# creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                     weights=~wts_gen, # including raked weights in the survey design
                     data=nhsub_gen)

# calculating loo_wtd
wtd_loo_tab_gen = lapply(loo_all_gen, function(x)loo_wtd(x,svy_rake)) %>%
  do.call(rbind,.) %>%
  data.frame(.) %>%
  mutate(model = modelnames) %>%
  rename(wtdElpd_loo = wtd_elpd_loo)


# getting prediction for MRP - for poststrat table
modelallvar_predict_gen = posterior_linpred(nhmodel_allvar_gen, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelallvar_popnest_gen = apply(modelallvar_predict_gen, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbiasprec_predict_gen = posterior_linpred(nhmodel_biasprec_gen, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbiasprec_popnest_gen = apply(modelbiasprec_predict_gen, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelbias_predict_gen = posterior_linpred(nhmodel_bias_gen, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelbias_popnest_gen = apply(modelbias_predict_gen, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelprec_predict_gen = posterior_linpred(nhmodel_prec_gen, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelprec_popnest_gen = apply(modelprec_predict_gen, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelinc_predict_gen = posterior_linpred(nhmodel_inc_gen, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelinc_popnest_gen = apply(modelinc_predict_gen, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

modelign_predict_gen = posterior_linpred(nhmodel_ign_gen, newdata = popn_ps, transform = T) # getting model estimate for each cell
modelign_popnest_gen = apply(modelign_predict_gen, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


popnest_all_gen = list(modelallvar_popnest_gen, modelbiasprec_popnest_gen, modelbias_popnest_gen,
                       modelprec_popnest_gen, modelinc_popnest_gen, modelign_popnest_gen)

alph = 0.1

popnest_tab_gen = lapply(popnest_all_gen, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind, .) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
  mutate(model = modelnames,
         mean_yObs = mean(as.numeric(nhfinal$high_bp)-1), # to revert to level 0, 1
         popn_ci_width = as.numeric(popnestX95 - popnestX5),
         MRP_intervalScr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
           ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0)),
         sample = "Generated")


# *PLOT mrp ----------------------------------------------------------------
# combine all four together
join1_dr2 = left_join(loo_tab_dr2, wtd_loo_tab_dr2, by = "model")
popn_all_tab_dr2 = left_join(join1_dr2, popnest_tab_dr2, by = "model")

join1_fas = left_join(loo_tab_fas, wtd_loo_tab_fas, by = "model")
popn_all_tab_fas = left_join(join1_fas, popnest_tab_fas, by = "model")

join1_gen = left_join(loo_tab_gen, wtd_loo_tab_gen, by = "model")
popn_all_tab_gen = left_join(join1_gen, popnest_tab_gen, by = "model")
 
popn_all_tab = rbind(popn_all_tab_dr2, popn_all_tab_fas, popn_all_tab_gen) %>% 
  group_by(sample) %>% 
  mutate(elpd_loo_std = (elpd_loo - min(elpd_loo)) / (max(elpd_loo) - min(elpd_loo)),
         wtdElpd_loo_std = (wtdElpd_loo - min(wtdElpd_loo)) / (max(wtdElpd_loo) - min(wtdElpd_loo)),
         MRP_intScr_scaled = (MRP_intervalScr - min(MRP_intervalScr)) / (max(MRP_intervalScr) - min(MRP_intervalScr)))

# colour for samples, shapes for models
shape_base  = c(21, 22, 24, 23, 25, 4)

bd_col = c(`All variables` =  "black",
           `Bias-precision` = "black", 
           `Bias-only` = "black",
           `Precision-only` = "black", 
           `Inconsequential` = "black",
           `Ignorable` = "black")

scales::show_col(ggthemes::colorblind_pal()(8))
colour_palette_var_base2 = c('#E69F00', '#009E73', '#CC79A7') # '#009E73')

plot_tab = popn_all_tab %>% 
  mutate(model = case_when(model == 'allvar' ~ "All variables",
                           model == 'biasprec' ~ "Bias-precision",
                           model == 'bias' ~ "Bias-only",
                           model == 'prec' ~ "Precision-only",
                           model == 'inc' ~ "Inconsequential",
                           model == 'ign' ~ "Ignorable"),
         model = fct_relevel(model, c("All variables", "Bias-precision", "Bias-only", "Precision-only",
                                      "Inconsequential", "Ignorable"))) %>% 
  pivot_longer(cols = c("elpd_loo_std","wtdElpd_loo_std"), names_to = "type", values_to = "model_score") %>%
  mutate(type = factor(type),
         type = fct_recode(type, `PSIS-LOO` = "elpd_loo_std", `WTD-PSIS-LOO` = "wtdElpd_loo_std"))

( g1 =  plot_tab %>%
    ggplot(., aes(x = model_score, y = MRP_intScr_scaled, group=sample, shape = model, fill = sample, colour = sample)) +
    facet_grid(~type, scales = "free")+
    geom_line(alpha=0.8) + 
    geom_point(size=4, alpha = 0.7) + 
    theme_bw(base_size = 15)  +
    scale_shape_manual(values = shape_base) +
    scale_fill_manual(values = colour_palette_var_base2) +
    scale_colour_manual(values = colour_palette_var_base2) +
    ylab("Scaled interval score for MRP estimates") +
    xlab("Scaled PSIS-LOO-based model score") + 
    ggtitle('MRP interval score') +
    theme( plot.tag.position = c(0.52, -0.05),
           plot.tag = element_text(size=15, face="bold"),
           plot.margin = margin(10, 20, 80, 10),
           legend.text.align = 0,
          legend.title = element_text(size=15, face="bold"),
          legend.text = element_text(size=13)) + 
    guides(fill = guide_legend("Sample", override.aes=list(color=colour_palette_var_base2)),
           colour = "none" ,
           shape = "none") +
    labs(tag = paste0(str_pad("Bias-precision", 36, "right"),
                      str_pad("Bias-only", 15, "right"), 
                      str_pad("Precision-only", 21, "right"), 
                      str_pad("Irrelevant", 22, "right"))) +
    cowplot::panel_border())

mod = c("All variables", "Bias-precision")
leg_lab = c(bquote(bold("Bias")~"+"~bold("Precision")),
            bquote("Ign. + "~bold("Precision")~"+ Inc. +"~bold("Bias")), 
            bquote(bold("Bias        ")),
            bquote(bold("Precision ")), 
            "Inconsequential (Inc.)",
            "Ignorable (Ign.)")

( g2a = plot_tab %>% 
    filter(model %in% mod ) %>%
    ggplot(., aes(x = model_score,, y = MRP_intScr_scaled, group=sample, shape = model, fill = sample, colour = sample)) +
    facet_grid(~type, scales = "free")+
    geom_line(alpha=0.8) + 
    geom_point(size=4, alpha = 0.7) + 
    theme_bw(base_size = 15)  +
    scale_shape_manual(values = shape_base[c(2,1)], label=leg_lab[1:2]) +
    scale_fill_manual(values = colour_palette_var_base2) +
    scale_colour_manual(values = colour_palette_var_base2) +
    ylab("Scaled interval score for MRP estimates") +
    xlab("Scaled PSIS-LOO-based model score") + 
    ggtitle('MRP interval score') +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.margin =  margin(10, 20, 10, 10),
          legend.text = element_text(size=13),
          legend.justification = c(0.2, 1),
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          plot.tag.position = c(0.527, 0.12),
          plot.tag = element_text(size=18, face="bold")) + 
    guides(fill = "none",
           colour = "none" ,
           shape = guide_legend(override.aes = list(size=3), nrow=2)) )


( g2b = plot_tab %>% 
    filter(model==c("Bias-only", "Precision-only")) %>%
    ggplot(., aes(x = model_score, y = MRP_intScr_scaled, group=sample, shape = model, fill = sample, colour = sample)) +
    facet_grid(~type, scales = "free")+
    geom_line(alpha=0.8) + 
    geom_point(size=4, alpha = 0.7) + 
    theme_bw(base_size = 15)  +
    scale_shape_manual(values = shape_base[3:4], labels = leg_lab[3:4]) +
    scale_fill_manual(values = colour_palette_var_base2) +
    scale_colour_manual(values = colour_palette_var_base2) +
    ylab("Scaled interval score for MRP estimates") +
    xlab("Scaled PSIS-LOO-based model score") + 
    ggtitle('MRP interval score') +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=13),
          legend.justification = c(0.55, 1), 
          legend.margin =  margin(10, 20, 10, 10),
          legend.background = element_rect(fill='transparent'), #transparent legend bg       
          plot.margin = margin(10, 70, 10, 10),
          plot.tag.position = c(0.527, 0.12),
          plot.tag = element_text(size=18, face="bold"),) + 
    guides(fill = "none",
           colour = "none" ,
           shape = guide_legend(override.aes = list(size=3), nrow=1)) )

( g2c = plot_tab %>% 
    filter(model==c("Inconsequential", "Ignorable")) %>%
    ggplot(., aes(x = model_score, y = MRP_intScr_scaled, group=sample, shape = model, fill = sample, colour = sample)) +
    facet_grid(~type, scales = "free")+
    geom_line(alpha=0.8) + 
    geom_point(size=4, alpha = 0.7) + 
    theme_bw(base_size = 15)  +
    scale_shape_manual(values = shape_base[5:6], labels=leg_lab[5:6]) +
    scale_fill_manual(values = colour_palette_var_base2) +
    scale_colour_manual(values = colour_palette_var_base2) +
    ylab("Scaled interval score for MRP estimates") +
    xlab("Scaled PSIS-LOO-based model score") + 
    ggtitle('MRP interval score') +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=13),
          legend.justification = c(0.9, 1), 
          legend.margin =  margin(10, 20, 10, 10),
          legend.background = element_rect(fill='transparent'), #transparent legend bg       
          legend.text.align = 0,
          plot.tag.position = c(0.55, 0.12),
          plot.tag = element_text(size=18, face="bold"),) + 
    guides(fill = "none",
           colour = "none" ,
           shape = guide_legend(override.aes = list(size=3), nrow=2)) )

g3 = ggdraw() +
  draw_plot(g1, 0, .1, 1, .9) +
  draw_plot(get_legend(g2a), 0, 0, 0.92, .2) + 
  draw_plot(get_legend(g2b), 0, 0, 0.97, .2) + 
  draw_plot(get_legend(g2c), 0, 0, 0.98, .2) 

ggsave(g3, width=11, height=8, file=here("nhanes/figures/elpd_MRP_all.png"))


# dr2 ####
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
pred_allvar_popnInd_dr2 = posterior_predict(nhmodel_allvar_dr2, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelallvar_pred_popnInd_dr2[i] = mean(pred_allvar_popnInd_dr2[,i] == nhfinal$high_bp[i])
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
pred_biasprec_popnInd_dr2 = posterior_predict(nhmodel_biasprec_dr2, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelbiasprec_pred_popnInd_dr2[i] = mean(pred_biasprec_popnInd_dr2[,i] == nhfinal$high_bp[i])
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
pred_bias_popnInd_dr2 = posterior_predict(nhmodel_bias_dr2, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelbias_pred_popnInd_dr2[i] = mean(pred_bias_popnInd_dr2[,i] == nhfinal$high_bp[i])
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
pred_prec_popnInd_dr2 = posterior_predict(nhmodel_prec_dr2, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelprec_pred_popnInd_dr2[i] = mean(pred_prec_popnInd_dr2[,i] == nhfinal$high_bp[i])
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
  mutate(model="inc") %>% 
  rename(prob_pred_out = value)
# inc - popn
pred_inc_popnInd_dr2 = posterior_predict(nhmodel_inc_dr2, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelinc_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelinc_pred_popnInd_dr2[i] = mean(pred_inc_popnInd_dr2[,i] == nhfinal$high_bp[i])
}
modelinc_pred_popnInd_dr2 = modelinc_pred_popnInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="inc") %>% 
  rename(prob_pred_out = value)

# ignorable - sample
pred_ign_sampInd_dr2 = posterior_predict(nhmodel_ign_dr2, newdata = nhsub_dr2)  # getting outcome estimate for each sample indv
modelign_pred_sampInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelign_pred_sampInd_dr2[i] = mean(pred_ign_sampInd_dr2[,i] == nhsub_dr2$high_bp[i])
}
modelign_pred_sampInd_dr2 = modelign_pred_sampInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="ign") %>% 
  rename(prob_pred_out = value)
# ignorable - popn
pred_ign_popnInd_dr2 = posterior_predict(nhmodel_ign_dr2, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelign_pred_popnInd_dr2 = matrix(NA)
for (i in 1:nrow(nhsub_dr2)){
  modelign_pred_popnInd_dr2[i] = mean(pred_ign_popnInd_dr2[,i] == nhfinal$high_bp[i])
}
modelign_pred_popnInd_dr2 = modelign_pred_popnInd_dr2 %>% 
  as_tibble %>% 
  mutate(model="ign") %>% 
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
pred_allvar_popnInd_fas = posterior_predict(nhmodel_allvar_fas, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelallvar_pred_popnInd_fas[i] = mean(pred_allvar_popnInd_fas[,i] == nhfinal$high_bp[i])
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
pred_biasprec_popnInd_fas = posterior_predict(nhmodel_biasprec_fas, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelbiasprec_pred_popnInd_fas[i] = mean(pred_biasprec_popnInd_fas[,i] == nhfinal$high_bp[i])
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
pred_bias_popnInd_fas = posterior_predict(nhmodel_bias_fas, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelbias_pred_popnInd_fas[i] = mean(pred_bias_popnInd_fas[,i] == nhfinal$high_bp[i])
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
pred_prec_popnInd_fas = posterior_predict(nhmodel_prec_fas, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelprec_pred_popnInd_fas[i] = mean(pred_prec_popnInd_fas[,i] == nhfinal$high_bp[i])
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
  mutate(model="inc") %>% 
  rename(prob_pred_out = value)
# inconsequential - popn
pred_inc_popnInd_fas = posterior_predict(nhmodel_inc_fas, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelinc_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelinc_pred_popnInd_fas[i] = mean(pred_inc_popnInd_fas[,i] == nhfinal$high_bp[i])
}
modelinc_pred_popnInd_fas = modelinc_pred_popnInd_fas %>% 
  as_tibble %>% 
  mutate(model="inc") %>% 
  rename(prob_pred_out = value)

# ignorable - sample
pred_ign_sampInd_fas = posterior_predict(nhmodel_ign_fas, newdata = nhsub_fas)  # getting outcome estimate for each sample indv
modelign_pred_sampInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelign_pred_sampInd_fas[i] = mean(pred_ign_sampInd_fas[,i] == nhsub_fas$high_bp[i])
}
modelign_pred_sampInd_fas = modelign_pred_sampInd_fas %>% 
  as_tibble %>% 
  mutate(model="ign") %>% 
  rename(prob_pred_out = value)
# ignorable - popn
pred_ign_popnInd_fas = posterior_predict(nhmodel_ign_fas, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelign_pred_popnInd_fas = matrix(NA)
for (i in 1:nrow(nhsub_fas)){
  modelign_pred_popnInd_fas[i] = mean(pred_ign_popnInd_fas[,i] == nhfinal$high_bp[i])
}
modelign_pred_popnInd_fas = modelign_pred_popnInd_fas %>% 
  as_tibble %>% 
  mutate(model="ign") %>% 
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


# Generated ---------------------------------------------------------------
pred_allvar_sampInd_gen = posterior_predict(nhmodel_allvar_gen, newdata = nhsub_gen)  # getting outcome estimate for each sample indv

modelallvar_pred_sampInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelallvar_pred_sampInd_gen[i] = mean(pred_allvar_sampInd_gen[,i] == nhsub_gen$high_bp[i])
}
modelallvar_pred_sampInd_gen = modelallvar_pred_sampInd_gen %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# all var - popn
pred_allvar_popnInd_gen = posterior_predict(nhmodel_allvar_gen, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelallvar_pred_popnInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelallvar_pred_popnInd_gen[i] = mean(pred_allvar_popnInd_gen[,i] == nhfinal$high_bp[i])
}
modelallvar_pred_popnInd_gen = modelallvar_pred_popnInd_gen %>% 
  as_tibble %>% 
  mutate(model="allvar") %>% 
  rename(prob_pred_out = value)

# biasprec - sample
pred_biasprec_sampInd_gen = posterior_predict(nhmodel_biasprec_gen, newdata = nhsub_gen)  # getting outcome estimate for each sample indv
modelbiasprec_pred_sampInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelbiasprec_pred_sampInd_gen[i] = mean(pred_biasprec_sampInd_gen[,i] == nhsub_gen$high_bp[i])
}
modelbiasprec_pred_sampInd_gen = modelbiasprec_pred_sampInd_gen %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# biasprec - popn
pred_biasprec_popnInd_gen = posterior_predict(nhmodel_biasprec_gen, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelbiasprec_pred_popnInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelbiasprec_pred_popnInd_gen[i] = mean(pred_biasprec_popnInd_gen[,i] == nhfinal$high_bp[i])
}
modelbiasprec_pred_popnInd_gen = modelbiasprec_pred_popnInd_gen %>% 
  as_tibble %>% 
  mutate(model="biasprec") %>% 
  rename(prob_pred_out = value)

# bias - sample
pred_bias_sampInd_gen = posterior_predict(nhmodel_bias_gen, newdata = nhsub_gen)  # getting outcome estimate for each sample indv
modelbias_pred_sampInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelbias_pred_sampInd_gen[i] = mean(pred_bias_sampInd_gen[,i] == nhsub_gen$high_bp[i])
}
modelbias_pred_sampInd_gen = modelbias_pred_sampInd_gen %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# bias - popn
pred_bias_popnInd_gen = posterior_predict(nhmodel_bias_gen, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelbias_pred_popnInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelbias_pred_popnInd_gen[i] = mean(pred_bias_popnInd_gen[,i] == nhfinal$high_bp[i])
}
modelbias_pred_popnInd_gen = modelbias_pred_popnInd_gen %>% 
  as_tibble %>% 
  mutate(model="bias") %>% 
  rename(prob_pred_out = value)

# precision - sample
pred_prec_sampInd_gen = posterior_predict(nhmodel_prec_gen, newdata = nhsub_gen)  # getting outcome estimate for each sample indv
modelprec_pred_sampInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelprec_pred_sampInd_gen[i] = mean(pred_prec_sampInd_gen[,i] == nhsub_gen$high_bp[i])
}
modelprec_pred_sampInd_gen = modelprec_pred_sampInd_gen %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# precision - popn
pred_prec_popnInd_gen = posterior_predict(nhmodel_prec_gen, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelprec_pred_popnInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelprec_pred_popnInd_gen[i] = mean(pred_prec_popnInd_gen[,i] == nhfinal$high_bp[i])
}
modelprec_pred_popnInd_gen = modelprec_pred_popnInd_gen %>% 
  as_tibble %>% 
  mutate(model="prec") %>% 
  rename(prob_pred_out = value)

# inconsequential - sample
pred_inc_sampInd_gen = posterior_predict(nhmodel_inc_gen, newdata = nhsub_gen)  # getting outcome estimate for each sample indv
modelinc_pred_sampInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelinc_pred_sampInd_gen[i] = mean(pred_inc_sampInd_gen[,i] == nhsub_gen$high_bp[i])
}
modelinc_pred_sampInd_gen = modelinc_pred_sampInd_gen %>% 
  as_tibble %>% 
  mutate(model="inc") %>% 
  rename(prob_pred_out = value)
# inconsequential - popn
pred_inc_popnInd_gen = posterior_predict(nhmodel_inc_gen, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelinc_pred_popnInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelinc_pred_popnInd_gen[i] = mean(pred_inc_popnInd_gen[,i] == nhfinal$high_bp[i])
}
modelinc_pred_popnInd_gen = modelinc_pred_popnInd_gen %>% 
  as_tibble %>% 
  mutate(model="inc") %>% 
  rename(prob_pred_out = value)

# ignorable - sample
pred_ign_sampInd_gen = posterior_predict(nhmodel_ign_gen, newdata = nhsub_gen)  # getting outcome estimate for each sample indv
modelign_pred_sampInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelign_pred_sampInd_gen[i] = mean(pred_ign_sampInd_gen[,i] == nhsub_gen$high_bp[i])
}
modelign_pred_sampInd_gen = modelign_pred_sampInd_gen %>% 
  as_tibble %>% 
  mutate(model="ign") %>% 
  rename(prob_pred_out = value)
# ignorable - popn
pred_ign_popnInd_gen = posterior_predict(nhmodel_ign_gen, newdata = nhfinal)  # getting outcome estimate for each sample indv
modelign_pred_popnInd_gen = matrix(NA)
for (i in 1:nrow(nhsub_gen)){
  modelign_pred_popnInd_gen[i] = mean(pred_ign_popnInd_gen[,i] == nhfinal$high_bp[i])
}
modelign_pred_popnInd_gen = modelign_pred_popnInd_gen %>% 
  as_tibble %>% 
  mutate(model="ign") %>% 
  rename(prob_pred_out = value)

pred_sampInd_gen = list(modelallvar_pred_sampInd_gen, modelbiasprec_pred_sampInd_gen,
                        modelbias_pred_sampInd_gen, modelprec_pred_sampInd_gen, 
                        modelinc_pred_sampInd_gen, modelign_pred_sampInd_gen) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 0,
         sample = "Generated") 

pred_popnInd_gen = list(modelallvar_pred_popnInd_gen, modelbiasprec_pred_popnInd_gen, 
                        modelbias_pred_popnInd_gen, modelprec_pred_popnInd_gen, 
                        modelinc_pred_popnInd_gen, modelign_pred_popnInd_gen) %>% 
  do.call(rbind, .) %>% 
  group_by(model) %>% 
  summarise(mean_prob_pred_out = mean(prob_pred_out)) %>% 
  mutate(popnInd = 1,
         sample = "Generated") 

j1_dr2 = left_join(pred_sampInd_dr2, loo_tab_dr2, by="model")
t1_dr2 = left_join(j1_dr2, wtd_loo_tab_dr2, by="model") 

j2_dr2 = left_join(pred_popnInd_dr2, loo_tab_dr2, by="model")
t2_dr2 = left_join(j2_dr2, wtd_loo_tab_dr2, by="model")

j1_fas = left_join(pred_sampInd_fas, loo_tab_fas, by="model")
t1_fas =left_join(j1_fas, wtd_loo_tab_fas, by="model")

j2_fas = left_join(pred_popnInd_fas, loo_tab_fas, by="model")
t2_fas =left_join(j2_fas, wtd_loo_tab_fas, by="model") 

j1_gen = left_join(pred_sampInd_gen, loo_tab_gen, by="model")
t1_gen =left_join(j1_gen, wtd_loo_tab_gen, by="model")

j2_gen = left_join(pred_popnInd_gen, loo_tab_gen, by="model")
t2_gen =left_join(j2_gen, wtd_loo_tab_gen, by="model") 

t1_all = rbind(t1_dr2, t2_dr2, 
               t1_fas, t2_fas,
               t1_gen, t2_gen) %>% 
  group_by(popnInd, sample) %>% 
  mutate(elpd_loo_std = (elpd_loo - min(elpd_loo)) / (max(elpd_loo) - min(elpd_loo)),
         wtdElpd_loo_std = (wtdElpd_loo - min(wtdElpd_loo)) / (max(wtdElpd_loo) - min(wtdElpd_loo))) %>% 
  mutate(model = case_when(model == 'allvar' ~ "All variables",
                           model == 'biasprec' ~ "Bias-precision",
                           model == 'bias' ~ "Bias-only",
                           model == 'prec' ~ "Precision-only",
                           model == 'inc' ~ "Inconsequential",
                           model == 'ign' ~ "Ignorable"),
         model = fct_relevel(model, c("All variables", "Bias-precision", "Bias-only", "Precision-only",
                                      "Inconsequential", "Ignorable"))) %>% 
  mutate(popnInd = factor(popnInd),
         popnInd = fct_recode(popnInd, `Sample` = "0", `Population` = "1")) %>% 
  pivot_longer(cols = c("elpd_loo_std","wtdElpd_loo_std"), names_to = "type", values_to = "model_score") %>%
  mutate(type = factor(type),
         type = fct_recode(type, `PSIS-LOO` = "elpd_loo_std", `WTD-PSIS-LOO` = "wtdElpd_loo_std")) 

# *PLOT individuals --------------------------------------------------------------------

# no bottom legend
( g4 =  t1_all  %>%
   ggplot(., aes(x = model_score, y = 1-mean_prob_pred_out, colour = sample, shape = factor(model), fill = sample, group=sample)) +
   geom_line(alpha=0.8) +
   geom_point(size=3, alpha = .8) + 
   scale_shape_manual(values = shape_base) +
   scale_fill_manual(values = colour_palette_var_base2) +
   scale_colour_manual(values = colour_palette_var_base2) +
   facet_grid(popnInd~type, scales = "free") +
   theme_bw(base_size = 15) +
   ggtitle('Mean of correctly predicting individual outcomes') +
   ylab('1 - mean of correctly predicting the outcome') +
   xlab("Scaled PSIS-LOO-based model score") +
   guides(fill = guide_legend("Sample", override.aes=list(color=colour_palette_var_base2)),
          colour = "none",
          shape = "none") +
   theme(legend.title = element_text(size=15, face="bold"),
         legend.text = element_text(size=15),
         plot.tag.position = c(0.52, -0.05),
         plot.tag = element_text(size=15, face="bold"),
         plot.margin = margin(10, 20, 80, 10)) +
    labs(tag = paste0(str_pad("Bias-precision", 36, "right"),
                      str_pad("Bias-only", 15, "right"), 
                      str_pad("Precision-only", 21, "right"), 
                      str_pad("Irrelevant", 22, "right"))) +
    panel_border())

# first legend group
( g4a =  t1_all  %>%
    filter(model %in% mod ) %>%
    ggplot(., aes(x = model_score, y = 1-mean_prob_pred_out, colour = sample, shape = factor(model), fill = sample, group=sample)) +
    geom_line(alpha=0.8) +
    geom_point(size=3, alpha = .8) + 
    scale_shape_manual(values = shape_base[c(2,1)], label = leg_lab[1:2]) +
    scale_fill_manual(values = colour_palette_var_base2) +
    scale_colour_manual(values = colour_palette_var_base2) +
    facet_grid(popnInd~type, scales = "free") +
    theme_bw(base_size = 15) +
    ggtitle('Mean of correctly predicting individual outcomes') +
    ylab('1 - mean of correctly predicting the outcome') +
    xlab("Scaled PSIS-LOO-based model score") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.margin =  margin(10, 20, 10, 10),
          legend.text = element_text(size=13),
          legend.justification = c(0.2, 1),
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          plot.tag.position = c(0.527, 0.12),
          plot.tag = element_text(size=18, face="bold")) + 
    guides(fill = "none",
           colour = "none" ,
           shape = guide_legend(override.aes = list(size=3), nrow=2)) )

# second legend group
( g4b =  t1_all  %>%
    filter(model==c("Bias-only", "Precision-only")) %>%
    ggplot(., aes(x = model_score, y = 1-mean_prob_pred_out, colour = sample, shape = factor(model), fill = sample, group=sample)) +
    geom_line(alpha=0.8) +
    geom_point(size=3, alpha = .8) + 
    scale_shape_manual(values = shape_base[3:4], label = leg_lab[3:4]) +
    scale_fill_manual(values = colour_palette_var_base2) +
    scale_colour_manual(values = colour_palette_var_base2) +
    facet_grid(popnInd~type, scales = "free") +
    theme_bw(base_size = 15) +
    ggtitle('Mean of correctly predicting individual outcomes') +
    ylab('1 - mean of correctly predicting the outcome') +
    xlab("Scaled PSIS-LOO-based model score") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=13),
          legend.justification = c(0.55, 1), 
          legend.margin =  margin(10, 20, 10, 10),
          legend.background = element_rect(fill='transparent'), #transparent legend bg       
          plot.margin = margin(10, 70, 10, 10),
          plot.tag.position = c(0.527, 0.12),
          plot.tag = element_text(size=18, face="bold"),) + 
    guides(fill = "none",
           colour = "none" ,
           shape = guide_legend(override.aes = list(size=3), nrow=1)) )

# third legend group
( g4c =  t1_all  %>%
    filter(model==c("Bias-only", "Precision-only")) %>%
    ggplot(., aes(x = model_score, y = 1-mean_prob_pred_out, colour = sample, shape = factor(model), fill = sample, group=sample)) +
    geom_line(alpha=0.8) +
    geom_point(size=3, alpha = .8) + 
    scale_shape_manual(values = shape_base[5:6], label = leg_lab[5:6]) +
    scale_fill_manual(values = colour_palette_var_base2) +
    scale_colour_manual(values = colour_palette_var_base2) +
    facet_grid(popnInd~type, scales = "free") +
    theme_bw(base_size = 15) +
    ggtitle('Mean of correctly predicting individual outcomes') +
    ylab('1 - mean of correctly predicting the outcome') +
    xlab("Scaled PSIS-LOO-based model score") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=13),
          legend.justification = c(0.9, 1), 
          legend.margin =  margin(10, 20, 10, 10),
          legend.background = element_rect(fill='transparent'), #transparent legend bg       
          legend.text.align = 0,
          plot.tag.position = c(0.55, 0.12),
          plot.tag = element_text(size=18, face="bold"),) + 
    guides(fill = "none",
           colour = "none" ,
           shape = guide_legend(override.aes = list(size=3), nrow=2)) )

g5 = ggdraw() +
  draw_plot(g4, 0, .1, 1, .9) +
  draw_plot(get_legend(g4a), 0, 0, 0.92, .2) + 
  draw_plot(get_legend(g4b), 0, 0, 0.97, .2) + 
  draw_plot(get_legend(g4c), 0, 0, 0.98, .2) 

ggsave(g5, width=11, height=8, file=here("nhanes/figures/elpd_indv_pred_outcome.png"))

