## table for individual data set 
library(tidyverse)

# ## data file
# load(here::here('02-super popn approach/experiment4_SAE/03-data/temp/loo_sae_1000.RData'))

# individual bias ---------------------------------------------------------
alph=0.1
sampest_list = list()
for(i in iter){
  y_prob = samp_data_list[[i]]$y_prob
  sampest_list[[i]] = mutate(sampest_tab_all[[i]],
                             ci_ind_width = sampestX95 - sampestX5,
                             coverage_ind = ifelse(y_prob >= sampestX5 & y_prob <= sampestX95, 1, 0),
                             bias_X50_ind =  sampestX50 - y_prob,
                             bias_X5_ind =  sampestX5 - y_prob,
                             bias_X95_ind =  sampestX95 - y_prob, 
                             bias_X50_abs_ind =  abs(sampestX50 - y_prob),
                             bias_X5_abs_ind =  abs(sampestX5 - y_prob),
                             bias_X95_abs_ind =  abs(sampestX95 - y_prob), 
                             bias_width_ind = bias_X95_ind - bias_X5_ind, 
                             intervalScr_ind = (sampestX95 - sampestX5) + 
                               ((2 / alph * (sampestX5 - y_prob)) * ifelse(y_prob < sampestX5, 1, 0)) + 
                               ((2 / alph * (y_prob - sampestX95)) * ifelse(y_prob > sampestX95, 1, 0)),
                             iteration = i) 
}

indv_all_tab = do.call(rbind, sampest_list[iter]) %>%
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',
                                        'model13' = 'X1 + X3 + X4',
                                        'model13a' = '*X1 + X3 + X4',
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4',
                                        'model12' = 'X1 + X2 + X4',
                                        'model14' = 'X2 + X3 + X4')))

indv_summ_tab = indv_all_tab %>% 
  group_by(iteration, model) %>% 
  summarise(
    # ind_ci_width_mean = mean(ci_ind_width),
            ind_bias_mean = mean(bias_X50_ind),
            ind_bias_sd = sd(bias_X50_ind), 
            ind_bias_mean_abs = mean(bias_X50_abs_ind),
            ind_bias_sd_abs = sd(bias_X50_abs_ind),
            ind_coverage_mean = mean(coverage_ind),
            ind_biasWidth_mean = mean(bias_width_ind),
            ind_biasWidth_sd = sd(bias_width_ind),
            ind_intervalScr_mean = mean(intervalScr_ind),
            ind_intervalScr_sd = sd(intervalScr_ind))


# loo ------------------------------------------------------------
source(here::here("02-super popn approach/experiment4_SAE/01-code/samp_loo.R"), echo=T)

indv_summ_tab = left_join(indv_summ_tab, elpd_all_tab, by=c('model', 'iteration')) %>%
  left_join(., wtd_elpd_se_tab, by=c('model', 'iteration')) %>%
  as_tibble() %>%
  mutate(model = factor(model))

indv_summ_tab$model = forcats::fct_relevel(indv_summ_tab$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                                                'X1 + X3 + X4', '*X1 + X3 + X4',
                                                                'X1 + X2 + X3', 'X1 + X2 + X4',
                                                                'X2 + X3 + X4', 'X1 + X3'))

