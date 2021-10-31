## plotting the results from simulations LOO_wtd
setwd("~/MRP project/02-simulations/models05-15")
library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)

load("simulated30temp_2_1.RData")
sim_doub_list[1:30]

N = nrow(samp_data_list[[1]])

sim_list2 = sim_doub_list[1:30]

loo_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'loo_doub_rank'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model10", "model15", "model05", "model06", "model07", "model08", "model09")) %>% 
  melt(.) %>%  
  group_by(Var2) %>%
  count(value) %>% 
  mutate(value, value = factor(value, levels= c("1", "2", "3", "4", "5", "6", "7"))) %>% 
  mutate(n, freq = as.numeric(n))  %>% 
  rename(Model = Var2) %>% 
  mutate(Model = factor(Model, levels=c("model05", "model06", "model07", "model08", "model09", "model10", "model15")))

loo_wtd_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'loo_wtd_doub_rank'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model10", "model15", "model05", "model06", "model07", "model08", "model09")) %>% 
  melt(.) %>%  
  group_by(Var2) %>%
  count(value) %>% 
  mutate(value, value = factor(value, levels= c("1", "2", "3", "4", "5", "6", "7"))) %>% 
  mutate(n, freq = as.numeric(n)) %>% 
  rename(Model = Var2) %>% 
  mutate(Model = factor(Model, levels=c("model05", "model06", "model07", "model08", "model09", "model10", "model15")))

png(filename="loo_doub_unwtd.png", width=550, height=500)
ggplot(loo_tab, aes(value, freq, fill=Model)) +
  geom_bar(stat="identity") + 
  facet_wrap(.~Model, ncol=2) +  scale_fill_brewer(palette = "Dark2") +
  labs(title='Unweighted LOO', x="Ranking")
dev.off()


png(filename="loo_doub_wtd.png", width=550, height=500)
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
  set_colnames(c("model10", "model15", "model05", "model06", "model07", "model08", "model09")) %>% 
  melt(.) %>% 
  rename(Model = Var2) %>% 
  mutate(Model = factor(Model, levels=c("model05", "model06", "model07", "model08", "model09", "model10", "model15")))

gg1 = ggplot(loo_elpd_tab, aes(Model, value, fill=Model)) +
  geom_violin() + labs(title="Unweighted elpd") +
  scale_fill_brewer(palette = "Dark2") 

# weighted
loo_wtd_elpd_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'wtd_elpd_loo'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model10", "model15", "model05", "model06", "model07", "model08", "model09")) %>% 
  melt(.) %>% 
  rename(Model = Var2) %>% 
  mutate(Model = factor(Model, levels=c("model05", "model06", "model07", "model08", "model09", "model10", "model15")))


gg2 = ggplot(loo_wtd_elpd_tab, aes(Model, value, fill=Model)) +
  geom_violin() + labs(title="Weighted elpd")  + 
  scale_fill_brewer(palette = "RdYlGn") 

png(file="loo_elpd_0510.png", width=600, height=790)
gridExtra::grid.arrange(gg1,gg2)
dev.off()

# plotting raw se elpd values ------------------------------------------------
# unweighted
loo_se_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'SE'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model10", "model15", "model05", "model06", "model07", "model08", "model09")) %>% 
  melt(.) %>% 
  rename(Model = Var2) %>% 
  mutate(Model = factor(Model, levels=c("model05", "model06", "model07", "model08", "model09", "model10", "model15")))

gg1 = ggplot(loo_se_tab, aes(Model, value, fill=Model)) +
  geom_violin() + labs(title="Unweighted std. error") +
  scale_fill_brewer(palette = "Dark2") 

# weighted
loo_wtd_se_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'wtd_SE'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model10", "model15", "model05", "model06", "model07", "model08", "model09")) %>% 
  melt(.) %>% 
  rename(Model = Var2) %>% 
  mutate(Model = factor(Model, levels=c("model05", "model06", "model07", "model08", "model09", "model10", "model15")))


gg2 = ggplot(loo_wtd_se_tab, aes(Model, value, fill=Model)) +
  geom_violin() + labs(title="Weighted std. error") +
  scale_fill_brewer(palette = "RdYlGn") 

png(file="loo_elpd_se_0510.png", width=600, height=790)
gridExtra::grid.arrange(gg1,gg2)
dev.off()


# plotting pairwise comparisons -------------------------------------------
elpdcol = grep('^elpd',colnames(samp_data_list[[1]]))

pairdiff <- function(a, b, colname1, colname2){
  dif = sum(a-b)
  stderror = sqrt(N) * sd(a - b)
  tab = rbind(dif, stderror)
  rownames(tab) = c(colname1, colname2)
  tab
}

pairdiff(test$elpd_2, test$elpd_4)

samp_data_list2 = samp_data_list[1:30]

## getting all the pairwise elpd_diff and se_diff
diff56 = samp_data_list2 %>% 
  lapply(., function(x)(x[, grep('^elpd',colnames(x))])) %>% 
  lapply(., function(x)pairdiff(x$elpd_5, x$elpd_6, 'elpd_diff_56', 'se_diff_56')) %>% 
  do.call(cbind,.) %>% 
  t() 

