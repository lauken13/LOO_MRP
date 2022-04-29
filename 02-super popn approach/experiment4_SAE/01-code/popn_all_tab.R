## combining popn estimates for plotting (Lauren's email)

## data file
load(here::here('02-super popn approach/experiment4_SAE/03-data/loo_sae_fx3_image.Rbin'))
iter = c(1:16,19:34,36:54,56:71,73:91,93:100)


# getting bias at popn level ----------------------------------------------
# bias, sum elpd, coverage (0 or 1), truth and lower/upper CI quantile and interval score
# calculating popnest and prob. of outcome for each iteration
popnest_list = list()

for(i in iter){
  popnest_list[[i]] = popnest_summary_list[[i]] %>% 
    mutate(prob_truth = pt_list[[i]]) %>% 
    mutate(bias_X50 = as.numeric(popnestX50 - prob_truth)) %>% # recursive diff for pt_list
    mutate(bias_X5 = as.numeric(popnestX5 - prob_truth)) %>% 
    mutate(bias_X95 = as.numeric(popnestX95 - prob_truth)) %>% 
    mutate(bias_range = as.numeric(popnestX95 - popnestX5)) %>% 
    mutate(coverage = ifelse(prob_truth >= popnestX5 & prob_truth <= popnestX95, 1, 0))
}

popnest_all_tab = do.call(rbind, popnest_list) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4')))

## elpd values
# unweighted loo ----------------------------------------------------------
elpd_06_tab = elpd_06_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_11_tab = elpd_11_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_13_tab = elpd_13_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_13a_tab = elpd_13a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_15_tab = elpd_15_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_15a_tab = elpd_15a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_all_tab = rbind(elpd_06_tab, elpd_11_tab, elpd_13_tab,
                     elpd_13a_tab, elpd_15_tab, elpd_15a_tab) %>% 
  mutate(elpd_X5 = .$elpd_loo - (.$elpd_SE*1.64), # calculating upper and lower bound of the elpd values
         elpd_X95 = .$elpd_loo + (.$elpd_SE*1.64),
         elpd_range = elpd_X95 - elpd_X5,
         iter = rep(1:length(iter), 6))

alph = 0.1
popn_all_tab = left_join(popnest_all_tab, elpd_all_tab, by=c('model', 'iter')) %>% 
  filter(model == 'X1 + X3' | 
           model == 'X1 + X2 + X3' | 
           model == 'X1 + X3 + X4' | 
           model == 'X1 + X2 + X3 + X4') %>% 
  ## calculating Gneiting interval score 
  mutate(interval_scr = (popnestX95 - popnestX5) + 
           ((2 / alph * (popnestX5 - prob_truth)) * ifelse(prob_truth < popnestX5, 1, 0)) + 
           ((2 / alph * (prob_truth - popnestX95)) * ifelse(prob_truth > popnestX95, 1, 0)))




