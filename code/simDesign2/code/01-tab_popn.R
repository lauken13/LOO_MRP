## combining popn estimates for plotting 
## data file
load(here::here('code/simDesign2/data/temp/loo_sae_500_wtd.RData')) # samp_size = 500
load(here::here('code/simDesign2/data/temp/loo_sae_1000.RData')) # samp_size = 1000

library(tidyverse)

# getting bias at popn level ----------------------------------------------
# bias, sum elpd, coverage (0 or 1), truth and lower/upper CI quantile and interval score
# calculating popnest and prob. of outcome for each iteration
popnest_list = list()
alph=0.1

for(i in iter){
  popnest_list[[i]] = popnest_summary_list[[i]] %>% 
    mutate(mean_yObs = pt_popn_list[[i]],
           popn_bias_X50 = as.numeric(popnestX50 - mean_yObs),
           popn_bias_X5 = as.numeric(popnestX5 - mean_yObs),
           popn_bias_X95 = as.numeric(popnestX95 - mean_yObs),
           popn_ci_width = as.numeric(popnestX95 - popnestX5),
           popn_coverage = ifelse(mean_yObs >= popnestX5 & mean_yObs <= popnestX95, 1, 0),
           popn_intervalScr = (popnestX95 - popnestX5) + 
             ((2 / alph * (popnestX5 - mean_yObs)) * ifelse(mean_yObs < popnestX5, 1, 0)) + 
             ((2 / alph * (mean_yObs - popnestX95)) * ifelse(mean_yObs > popnestX95, 1, 0))) 
}

popnest_all_tab = do.call(rbind, popnest_list) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4',
                                        'model12' = 'X1 + X2 + X4',
                                        'model14' = 'X2 + X3 + X4'))) %>% 
  rename(iteration = iter)

## elpd values
source(here::here("code/simDesign2/code/tab_popn_elpd.R"), echo=TRUE)
elpd_all_tab


popn_all_tab = left_join(popnest_all_tab, elpd_all_tab, by=c('model', 'iteration')) %>% 
  left_join(., wtdElpd_all_tab, by=c('model', 'iteration')) 
# View(popn_all_tab)

## loading individual tab
source(here::here("code/simDesign2/code/tab_samp.R"), echo=TRUE)

popn_indv_tab = left_join(popn_all_tab, indv_summ_tab, by=c('model', 'iteration')) 

## loading SAE tab 
source(here::here("code/simDesign2/code/tab_SAE.R"), echo=TRUE)

model_sae_X1_tab
model_sae_X2_tab
model_sae_X3_tab
model_sae_X4_tab

## counts for table
source(here::here("code/simDesign2/code/tab_counts.R"), echo=TRUE)

res_list_sae = list(indv_all_tab = indv_all_tab,
                    popn_indv_tab =  popn_indv_tab,
                    indv_summ_tab = indv_summ_tab,
                    model_sae_X1_tab = model_sae_X1_tab,
                    model_sae_X2_tab = model_sae_X2_tab,
                    model_sae_X3_tab = model_sae_X3_tab,
                    model_sae_X4_tab = model_sae_X4_tab, 
                    popn_counts = popn_counts,
                    samp_counts = samp_counts)
saveRDS(res_list_sae, file=here::here("code/simDesign2/data/res_list_sae_1000_wtd.rds"), compress=T)                 

# saveRDS(res_list_sae, file=here::here("code/simDesign2/data/res_list_sae_500_wtd.rds"), compress=T)


