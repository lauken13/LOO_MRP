## plotting the elpd values against 'best' model 
library(ggplot2)
library(reshape2)

load("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment1/simulated40temp_3.RData")

sim_list2 = sim_trip_list[1:40]

samp_data = samp_data_list[1:40]

for (i in 1:length(samp_data)){
  samp_data[[i]]$diff11_15 = samp_data[[i]]$elpd_11 - samp_data[[i]]$elpd_15
  samp_data[[i]]$diff12_15 = samp_data[[i]]$elpd_12 - samp_data[[i]]$elpd_15
  samp_data[[i]]$diff13_15 = samp_data[[i]]$elpd_13 - samp_data[[i]]$elpd_15
  samp_data[[i]]$diff14_15 = samp_data[[i]]$elpd_14 - samp_data[[i]]$elpd_15
}

## selecting only differences
diff_list = lapply(samp_data, function(x)select(x, diff11_15, diff12_15, diff13_15, diff14_15)) %>% 
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

ggsave("plot_loo_diff_1115.pdf", g, width=6, height=7.5, units="in", device="pdf")
