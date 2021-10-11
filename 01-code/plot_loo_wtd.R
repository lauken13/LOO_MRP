## plotting the results from simulations LOO_wtd
library(dplyr)
library(magrittr)
library(ggplot2)
load("~/MRP project/tests/simulated100temp_1.RData")
sim_list[1:60]
samp_data_list[1:60]

i=1
sim_list[[i]][sim_list[[i]][,'loo_rank'],]

sim_list2 = sim_list[1:60]

loo_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'loo_rank'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model1", "model2", "model3", "model4")) %>% 
  melt(.) %>%  
  group_by(Var2) %>%
  count(value) %>% 
  mutate(value, value = factor(value, levels= c("1", "2", "3", "4"))) %>% 
  mutate(n, freq = as.numeric(n))  %>% 
  rename(Model = Var2)

loo_wtd_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'loo_wtd_rank'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model1", "model2", "model3", "model4")) %>% 
  melt(.) %>%  
  group_by(Var2) %>%
  count(value) %>% 
  mutate(value, value = factor(value, levels= c("1", "2", "3", "4"))) %>% 
  mutate(n, freq = as.numeric(n)) %>% 
  rename(Model = Var2)

png(filename="loo_unwtd_2.png", width=550, height=500)
ggplot(loo_tab, aes(value, freq, fill=Model)) +
  geom_bar(stat="identity") + 
  facet_wrap(.~Model, ncol=2) +  scale_fill_brewer(palette = "Dark2") +
  labs(title='Unweighted LOO', x="Ranking")
dev.off()


png(filename="loo_wtd_2.png", width=550, height=500)
ggplot(loo_wtd_tab, aes(value, freq, fill=Model)) +
       geom_bar(stat="identity") + 
  facet_wrap(.~Model, ncol=2) +  scale_fill_brewer(palette = "RdYlGn")+
  labs(title='Weighted LOO', x="Ranking")
dev.off()

# second set of population ------------------------------------------------
load("~/MRP project/tests/simulated100temp_2.RData")
sim_list2 = sim_list[1:85]


## plotting the elpd values and std error
