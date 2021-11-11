## plotting the elpd values against 'best' model 
library(ggplot2)
library(reshape2)

load("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment1/simulated33temp_1.RData")
sim_list2 = sim_list[1:33]

samp_data = samp_data_list[1:33]

for (i in 1:length(samp_data)){
  samp_data[[i]]$diff01_15 = samp_data[[i]]$elpd_1 - samp_data[[i]]$elpd_15
  samp_data[[i]]$diff02_15 = samp_data[[i]]$elpd_2 - samp_data[[i]]$elpd_15
  samp_data[[i]]$diff03_15 = samp_data[[i]]$elpd_3 - samp_data[[i]]$elpd_15
  samp_data[[i]]$diff04_15 = samp_data[[i]]$elpd_4 - samp_data[[i]]$elpd_15
}

## selecting only differences
diff_list = lapply(samp_data, function(x)select(x, diff01_15, diff02_15, diff03_15, diff04_15)) %>% 
  lapply(., melt) 

for (i in 1:length(diff_list)){
  diff_list[[i]]$ite = i
}
 
diff_comb = do.call(rbind, diff_list)

g = ggplot(diff_comb, aes(variable, value, fill=factor(ite))) +
  geom_boxplot(position='dodge') +
  coord_flip() +
  labs(fill="iteration", title="Difference in elpd values") +  
  scale_x_discrete(limits = rev) 

ggsave("plot_loo_diff_0104.pdf", g, width=6, height=7.5, units="in", device="pdf")




  
