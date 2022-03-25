## calculating coverage (~frequentist) / calibration (~Bayesian)
cov_calc = list()
cov_list = list()
num_ite = length(popnest_list)

for(ite in iter){
  # calculating coverage for each iteration
  cov_calc[[ite]] = popnest_list[[ite]] %>% 
    rename(low_int = popnestX5,
           upp_int = popnestX95) %>% 
    mutate( range_int = upp_int - low_int,
            prob_truth = pt_tab[seq_along(ite),],  # truth
            coverage = ifelse(prob_truth >= low_int & prob_truth <= upp_int, 1, 0))
}

## calculating coverage
cov_calc_tab = do.call(rbind, cov_calc)
(cov_prop_tab = cov_calc_tab %>% 
    group_by(model) %>%
    summarise(coverage_sum = sum(coverage),
              coverage_prop = coverage_sum/length(iter))) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4')))

## plotting prob of truth first
sim_y_obs = sapply(samp_data_list[iter], function(x)mean(x$y_obs))
prob_truth = pt_list[iter]

plot(1:length(iter), prob_truth, ylim=c(0.3,0.55),pch=1)
points(1:length(iter), sim_y_obs, pch=8, col="blue")

legend(42, 0.52, pch=c(8,1), legend = c('smp_y_obs', 'popn'),
       col=c("blue","black"))
