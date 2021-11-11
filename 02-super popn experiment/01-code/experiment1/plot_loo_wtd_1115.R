## plotting the results from simulations LOO_wtd
setwd("~/GitHub/LOO_MRP/02-super popn experiment/03-results")
library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)

load("simulated40temp_3.RData")

N = nrow(samp_data_list[[1]])

sim_list2 = sim_trip_list[1:40]

loo_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'loo_trip_rank'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model11", "model12", "model13", "model14", "model15")) %>% 
  melt(.) %>%  
  group_by(Var2) %>%
  count(value) %>% 
  mutate(value, value = factor(value, levels= c("1", "2", "3", "4", "5"))) %>% 
  mutate(n, n = as.numeric(n))  %>% 
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
  mutate(n, n = as.numeric(n))  %>% 
  rename(Model = Var2) 

png(filename="loo_trip_unwtd.png", width=550, height=500)
ggplot(loo_tab, aes(value, n, fill=Model)) +
  geom_bar(stat="identity") + 
  facet_wrap(.~Model, ncol=2) +  scale_fill_brewer(palette = "Dark2") +
  labs(title='Unweighted LOO', x="Ranking")
dev.off()

#png(filename="loo_trip_wtd.png", width=550, height=500)
g2 <- ggplot(loo_wtd_tab, aes(value, n, fill=Model)) +
  geom_bar(stat="identity") + 
  facet_wrap(.~Model, ncol=2) +  scale_fill_brewer(palette = "RdYlGn")+
  labs(title='Weighted LOO', x="Ranking")
#dev.off()

gridExtra::grid.arrange(g1,g2)

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

#png(file="loo_elpd_1115.png", width=600, height=790)
gridExtra::grid.arrange(gg1,gg2)
#dev.off()

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

#png(file="loo_elpd_se_1115.png", width=600, height=790)
gridExtra::grid.arrange(gg1,gg2)
dev.off()

# MRP_LOO -----------------------------------------------------------------
loo_mrp_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'elpd_popnest_rank'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model11", "model12", "model13", "model14", "model15")) %>% 
  melt(.) %>%  
  group_by(Var2) %>%
  count(value) %>% 
  mutate(value, value = factor(value, levels= c("1", "2", "3", "4", "5"))) %>% 
  mutate(n, n = as.numeric(n))  %>% 
  rename(Model = Var2) 

png(filename="loo_trip_mrp_wtd.png", width=550, height=500)
ggplot(loo_mrp_tab, aes(value, n, fill=Model)) +
  geom_bar(stat="identity") + 
  facet_wrap(.~Model, ncol=2) + scale_fill_brewer(palette = "Set2") + 
  labs(title='MRP LOO', x="Ranking")
dev.off()

loo_mrp_tab$type = "MRP"
loo_wtd_tab$type = "raked"
loo_tab$type = "unweighted"

loo_comb_tab1 = bind_rows(loo_tab, loo_wtd_tab, loo_mrp_tab) %>% 
  mutate(type = as_factor(type))

## side by side plot
png('plot_sidebysiderank.png', width=835,height=575)
ggplot(loo_comb_tab1, aes(value, n, fill=type)) +
  facet_wrap(.~Model, ncol=3) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette = "Set1")
dev.off()


# plotting MRP elpd values ------------------------------------------------
loo_mrp_elpd_tab = sim_list2 %>% 
  lapply(., function(x)x[sort(rownames(x)),]) %>% 
  lapply(., function(x)(x[,'elpd.popnestX50'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model11", "model12", "model13", "model14", "model15")) %>% 
  melt(.) %>% 
  rename(Model = Var2) 

ggplot(loo_mrp_elpd_tab, aes(Model, value, fill=Model)) +
  geom_violin() + labs(title="MRP weighted elpd")  + 
  scale_fill_brewer(palette = "Set2") 

loo_mrp_elpd_tab$type = "MRP"
loo_wtd_elpd_tab$type = "raked"

loo_comb_tab2 = bind_rows(loo_mrp_elpd_tab, loo_wtd_elpd_tab)

png('plot_rakedVmrp.png', width=890, height=650)
ggplot(loo_comb_tab, aes(Model, value, fill=type)) +
  geom_violin() + 
  labs(title="Weighted LOO") + ylim(-7000,-3000) 
dev.off()

## plotting against the 'benchmark' ----------------------------------------
# calculate rank for our predicted outcome
for(i in 1:length(sim_list2)){
  sim_list2[[i]]$popnest_rank = rank(abs(prob_truth[i] - sim_list2[[i]]$popnestX50))
}

## calculating how many times ranked according to popnest
f1 = function(x){
  ind = which(x$popnest_rank==1);
  c(ifelse(which(x$loo_trip_rank==1)==ind,1,0),
    ifelse(which(x$loo_wtd_trip_rank==1)==ind,1,0),
    ifelse(which(x$elpd_popnest_rank==1)==ind,1,0))
}

rank_first_tab = lapply(sim_list2, f1) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("LOO", "raked", "LOOP")) %>% 
  melt(.) %>% 
  rename(type = Var2, freq = value) %>% 
  mutate(rank = "first")

f2 = function(x){
  c(identical(x$loo_trip_rank, x$popnest_rank), 
    identical(x$loo_wtd_trip_rank, x$popnest_rank),
    identical(x$elpd_popnest_rank, x$popnest_rank))
}

rank_ident_tab  = lapply(sim_list2, f2) %>% 
  lapply(., as.numeric) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("LOO", "raked", "LOOP")) %>% 
  melt(.) %>% 
  rename(type = Var2, freq = value) %>% 
  mutate(rank = "full")

rank_tab = bind_rows(rank_first_tab, rank_ident_tab)

png(filename="plot_ranktype.png", width=680, height=440)
ggplot(rank_tab, aes(type, freq, fill=rank)) + 
  geom_bar(stat="identity") + 
  labs(fill="correct", x="Type of weighting", title="Models 11-15")
dev.off()
