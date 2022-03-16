## calculating coverage (~frequentist) / calibration (~Bayesian)

cov_calc = list()
cov_list = list()
num_ite = length(popnest_list)

for(iter in 1:num_ite){
  prob_truth = pt_list[[iter]]
  cov_calc[[iter]] = popnest_list[[iter]] %>% 
    rename(low_int = popnestX5,
           upp_int = popnestX95) %>% 
    mutate( range_int = upp_int - low_int,
            coverage = ifelse(prob_truth >= low_int & prob_truth <= upp_int, 1, 0))
}

## calculating coverage
cov_calc_tab = do.call(rbind, cov_calc)
(cov_prop_tab = cov_calc_tab %>% 
  group_by(model) %>%
  summarise(coverage_sum = sum(coverage),
            coverage_prop = coverage_sum/num_ite))

## plotting coverage proportion
