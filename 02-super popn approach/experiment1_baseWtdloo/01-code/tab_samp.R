## table for individual data set 
library(tidyverse)

# individual bias ---------------------------------------------------------
alph=0.1
sampest_list = list()
for(i in iter){
  
  sampest_tab_list[[i]]$y_prob = rep(as.numeric(samp_data_list[[i]]$y_prob), 15) # repeat for 15 models
  sampest_tab_list[[i]]$wts = rep(as.numeric(samp_data_list[[i]]$wts), 15)
  
  sampest_list[[i]] = mutate(sampest_tab_list[[i]],
                             ind_ci_width = sampestX95 - sampestX5,
                             ind_coverage = ifelse(y_prob >= sampestX5 & y_prob <= sampestX95, 1, 0),
                             ind_bias_X50 =  sampestX50 - y_prob,
                             ind_bias_X5 =  sampestX5 - y_prob,
                             ind_bias_X95 =  sampestX95 - y_prob, 
                             ind_bias_X50_abs =  abs(sampestX50 - y_prob),
                             ind_bias_X5_abs =  abs(sampestX5 - y_prob),
                             ind_bias_X95_abs =  abs(sampestX95 - y_prob), 
                             ind_bias_width = ind_bias_X95 - ind_bias_X5, 
                             ind_intervalScr = (sampestX95 - sampestX5) + 
                               ((2 / alph * (sampestX5 - y_prob)) * ifelse(y_prob < sampestX5, 1, 0)) + 
                               ((2 / alph * (y_prob - sampestX95)) * ifelse(y_prob > sampestX95, 1, 0)),
                             iteration = i) 
}

indv_all_tab = do.call(rbind, sampest_list[iter]) %>%
  mutate(popnInd_intervalScr = ind_intervalScr * wts,
         model = plyr::revalue(model, c("model01" = 'X1',
                                        "model02" = 'X2', 
                                        "model03" = 'X3',
                                        "model04" = 'X4', 
                                        "model05" = 'X1 + X2', 
                                        'model06' = 'X1 + X3',
                                        "model07" = 'X1 + X4',
                                        "model08" = 'X2 + X3', 
                                        "model09" = 'X2 + X4', 
                                        "model10" = 'X3 + X4', 
                                        'model11' = 'X1 + X2 + X3',
                                        'model12' = 'X1 + X2 + X4',
                                        'model13' = 'X1 + X3 + X4', 
                                        'model14' = 'X2 + X3 + X4',
                                        "model15" = 'X1 + X2 + X3 + X4')))

indv_summ_tab = indv_all_tab %>% 
  group_by(model, iteration) %>% 
  summarise(ind_wtd_intervalScr_sum = sum(popnInd_intervalScr),
            ind_ci_width_mean = mean(ind_ci_width),
            ind_bias_mean = mean(ind_bias_X50),
            ind_bias_sd = sd(ind_bias_X50), 
            ind_bias_mean_abs = mean(ind_bias_X50_abs),
            ind_bias_sd_abs = sd(ind_bias_X50_abs),
            ind_coverage_mean = mean(ind_coverage),
            ind_biasWidth_mean = mean(ind_bias_width),
            ind_biasWidth_sd = sd(ind_bias_width),
            ind_intervalScr_mean = mean(ind_intervalScr),
            ind_intervalScr_sd = sd(ind_intervalScr))

indv_summ_tab$model = fct_relevel(indv_summ_tab$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                                         'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                                         'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                                         'X1', 'X3', 'X1 + X3'))

indv_all_tab$model = fct_relevel(indv_all_tab$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                                         'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                                         'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                                         'X1', 'X3', 'X1 + X3'))

## calculating weighted popn interval Scr
indv_all_tab %>% 
  mutate(popnInd_intervalScr = ind_intervalScr * wts) %>% 
 group_by(model, iteration) %>% 
  summarise()
