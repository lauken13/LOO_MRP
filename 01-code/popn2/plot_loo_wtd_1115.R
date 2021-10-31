## plotting the results from simulations LOO_wtd
setwd("~/Documents/GitHub/LOO_MRP/01-code/popn2/models05-15")
library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)

load("simulated14temp_2.RData")

N = nrow(samp_data_list[[1]])

sim_list2 = sim_trip_list[1:19]

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



# plotting raw elpd values ------------------------------------------------
# unweighted
loo_elpd_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model11", "model12", "model13", "model14", "model15")) %>% 
  melt(.) %>% 
  rename(Model = Var2) 

gg1 = ggplot(loo_elpd_tab, aes(Model, value, fill=Model)) +
  geom_violin() + labs(title="Unweighted elpd") +
  scale_fill_brewer(palette = "Dark2") 

# weighted
loo_wtd_elpd_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'wtd_elpd_loo'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model11", "model12", "model13", "model14", "model15")) %>% 
  melt(.) %>% 
  rename(Model = Var2)

gg2 = ggplot(loo_wtd_elpd_tab, aes(Model, value, fill=Model)) +
  geom_violin() + labs(title="Weighted elpd")  + 
  scale_fill_brewer(palette = "RdYlGn") 

png(file="loo_elpd_1115.png", width=600, height=790)
gridExtra::grid.arrange(gg1,gg2)
dev.off()

# plotting raw se elpd values ------------------------------------------------
# unweighted
loo_se_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'SE'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model11", "model12", "model13", "model14", "model15")) %>% 
  melt(.) %>% 
  rename(Model = Var2) 

gg1 = ggplot(loo_se_tab, aes(Model, value, fill=Model)) +
  geom_violin() + labs(title="Unweighted std. error") +
  scale_fill_brewer(palette = "Dark2") 

# weighted
loo_wtd_se_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'wtd_SE'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model11", "model12", "model13", "model14", "model15")) %>% 
  melt(.) %>% 
  rename(Model = Var2) 

gg2 = ggplot(loo_wtd_se_tab, aes(Model, value, fill=Model)) +
  geom_violin() + labs(title="Weighted std. error") +
  scale_fill_brewer(palette = "RdYlGn") 

png(file="loo_elpd_se_1115.png", width=600, height=790)
gridExtra::grid.arrange(gg1,gg2)
dev.off()

