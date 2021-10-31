## plotting the results from simulations LOO_wtd
setwd("~/MRP project/02-simulations/models05-15")
library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)

load("simulated30temp_2_1.RData")
sim_trip_list[1:30]

N = nrow(samp_data_list[[1]])

sim_list2 = sim_trip_list[1:30]

loo_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'loo_trip_rank'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model11", "model12", "model13", "model14", "model15")) %>% 
  melt(.) %>%  
  group_by(Var2) %>%
  count(value) %>% 
  mutate(value, value = factor(value, levels= c("1", "2", "3", "4", "5"))) %>% 
  mutate(n, freq = as.numeric(n))  %>% 
  rename(Model = Var2) 

loo_wtd_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'loo_wtd_trip_rank'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model11", "model12", "model13", "model14", "model15")) %>% 
  melt(.) %>%  
  group_by(Var2) %>%
  count(value) %>% 
  mutate(value, value = factor(value, levels= c("1", "2", "3", "4", "5"))) %>% 
  mutate(n, freq = as.numeric(n))  %>% 
  rename(Model = Var2) 

png(filename="loo_trip_unwtd.png", width=550, height=500)
ggplot(loo_tab, aes(value, freq, fill=Model)) +
  geom_bar(stat="identity") + 
  facet_wrap(.~Model, ncol=2) +  scale_fill_brewer(palette = "Dark2") +
  labs(title='Unweighted LOO', x="Ranking")
dev.off()


png(filename="loo_trip_wtd.png", width=550, height=500)
ggplot(loo_wtd_tab, aes(value, freq, fill=Model)) +
  geom_bar(stat="identity") + 
  facet_wrap(.~Model, ncol=2) +  scale_fill_brewer(palette = "RdYlGn")+
  labs(title='Weighted LOO', x="Ranking")
dev.off()
