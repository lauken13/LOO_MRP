## calculating coverage (~frequentist) / calibration (~Bayesian)


## calculating coverage
cov_calc = list()
cov_list = list()
num_ite = length(popnest_list)

for(ite in iter){
  # calculating coverage for each iteration
  cov_calc[[ite]] = popnest_list[[ite]] %>% 
    rename(low_int = popnestX5,
           upp_int = popnestX95) %>% 
    mutate( range_int = upp_int - low_int,
            prob_truth = pt_list[[ite]],  # truth
            coverage = ifelse(prob_truth >= low_int & prob_truth <= upp_int, 1, 0))
  
}

## calculating coverage
cov_calc_tab = do.call(rbind, cov_calc)
(cov_prop_tab = cov_calc_tab %>% 
  group_by(model) %>%
  summarise(coverage_sum = sum(coverage),
            coverage_prop = coverage_sum/length(iter)))

## plotting prob of truth first
sim_y_obs = sapply(samp_data_list[iter], function(x)mean(x$y_obs))
prob_truth = pt_list[iter]

# calculating popnest and prob. of outcome for each iteration
popnest_tab = do.call(rbind,popnest_list[iter])


plot(1:length(iter), prob_truth, ylim=c(0.3,0.45), pch=1)
points(1:length(iter), sim_y_obs, pch=8, col="blue")
points(1:nrow(popnest_tab), popnest_tab$popnestX50, col="orange", pch=23)

legend(60, 0.45, pch=c(23,8,1), legend = c('mrp_est', 'smp_y_obs', 'popn'),
       col=c("orange","blue","black"))

plot(sim_out, prob_truth, xlim=c(0.47,0.68), ylim=c(0.47,0.68), pch=19)
abline(a=0, b=1)
