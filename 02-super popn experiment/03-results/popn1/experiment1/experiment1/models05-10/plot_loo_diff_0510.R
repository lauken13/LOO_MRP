## plotting the elpd values against 'best' model 
library(ggplot2)
library(reshape2)

setwd("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment1")
load("simulated30temp_2.RData")

N = nrow(samp_data_list[[1]])

ind = c(1:5, 26:29, 51:61, 76:85)
sim_list2 = sim_doub_list[ind]
samp_data = samp_data_list[ind]


for (i in 1:length(samp_data)){
  samp_data[[i]]$diff05_15 = samp_data[[i]]$elpd_5 - samp_data[[i]]$elpd_15
  samp_data[[i]]$diff06_15 = samp_data[[i]]$elpd_6 - samp_data[[i]]$elpd_15
  samp_data[[i]]$diff07_15 = samp_data[[i]]$elpd_7 - samp_data[[i]]$elpd_15
  samp_data[[i]]$diff08_15 = samp_data[[i]]$elpd_8 - samp_data[[i]]$elpd_15
  samp_data[[i]]$diff09_15 = samp_data[[i]]$elpd_9 - samp_data[[i]]$elpd_15
  samp_data[[i]]$diff10_15 = samp_data[[i]]$elpd_10 - samp_data[[i]]$elpd_15
}

## selecting only differences
diff_list = lapply(samp_data, function(x)select(x, diff05_15, diff06_15, diff07_15, diff08_15, diff09_15, diff10_15)) %>% 
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
ggsave("plot_loo_diff_0510.pdf", g, width=6, height=7.5, units="in", device="pdf")
