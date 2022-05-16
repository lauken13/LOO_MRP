## table for individual data set 
library(tidyverse)

# individual bias ---------------------------------------------------------
alph=0.1
sampest_list = list()
for(i in iter){
  
  sampest_tab_list[[i]]$y_obs = rep(as.numeric(samp_data_list[[i]]$y_obs), 15)
  
  sampest_list[[i]] = mutate(sampest_tab_list[[i]],
                             ci_ind_width = range_int,
                             coverage_ind = coverage,
                             bias_X50_ind =  sampestX50 - y_obs,
                             bias_X5_ind =  sampestX5 - y_obs,
                             bias_X95_ind =  sampestX95 - y_obs, 
                             bias_X50_abs_ind =  abs(sampestX50 - y_obs),
                             bias_X5_abs_ind =  abs(sampestX5 - y_obs),
                             bias_X95_abs_ind =  abs(sampestX95 - y_obs), 
                             bias_width_ind = bias_X95_ind - bias_X5_ind, 
                             intervalScr_ind = (sampestX95 - sampestX5) + 
                               ((2 / alph * (sampestX5 - y_obs)) * ifelse(y_obs < sampestX5, 1, 0)) + 
                               ((2 / alph * (y_obs - sampestX95)) * ifelse(y_obs > sampestX95, 1, 0)),
                             iteration = i) 
}

indv_all_tab = do.call(rbind, sampest_list[iter]) %>%
  mutate(model = plyr::revalue(model, c("model01" = 'X1',
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
  group_by(iteration, model) %>% 
  summarise(ind_ci_width_mean = mean(ci_ind_width),
            ind_bias_mean = mean(bias_X50_ind),
            ind_bias_sd = sd(bias_X50_ind), 
            ind_bias_mean_abs = mean(bias_X50_abs_ind),
            ind_bias_sd_abs = sd(bias_X50_abs_ind),
            ind_coverage_mean = mean(coverage_ind),
            ind_biasWidth_mean = mean(bias_width_ind),
            ind_biasWidth_sd = sd(bias_width_ind),
            ind_intervalScr_mean = mean(intervalScr_ind),
            ind_intervalScr_sd = sd(intervalScr_ind))


# # loo ------------------------------------------------------------
# indv_summ_tab = left_join(indv_summ_tab, elpd_all_tab, by=c('model', 'iteration')) %>%
#   left_join(., wtdElpd_all_tab, by=c('model', 'iteration')) %>%
#   as_tibble() %>%
#   mutate(model = factor(model))

