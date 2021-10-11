## plotting the results from simulations LOO_wtd
library(dplyr)
library(magrittr)
library(ggplot2)
load("~/MRP project/tests/simulated100temp_1.RData")
sim_list[1:60]
samp_data_list[1:60]
N = nrow(samp_data_list[[1]])

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
samp_data_list[[1]]$elpd_1

# unweighted
loo_elpd_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model1", "model2", "model3", "model4")) %>% 
  melt(.) %>% 
  rename(Model = Var2)
  
gg1 = ggplot(loo_elpd_tab, aes(Model, value, fill=Model)) +
  geom_violin() + labs(title="Unweighted elpd") +
  scale_fill_brewer(palette = "Dark2") 

# weighted
loo_wtd_elpd_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'wtd_elpd_loo'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c("model1", "model2", "model3", "model4")) %>% 
  melt(.) %>% 
  rename(Model = Var2)

gg2 = ggplot(loo_wtd_elpd_tab, aes(Model, value, fill=Model)) +
  geom_violin() + labs(title="Weighted elpd")  + 
  scale_fill_brewer(palette = "RdYlGn") 

png(file="loo_elpd_WtdvUnwtd.png", width=600, height=790)
gridExtra::grid.arrange(gg1,gg2)
dev.off()



# plotting pairwise comparisons -------------------------------------------

test = samp_data_list[[1]]
sqrt(N) * sd(test$elpd_2 - test$elpd_4)

elpdcol = grep('^elpd',colnames(samp_data_list[[1]]))

pairdiff <- function(a, b, colname1, colname2){
  dif = sum(a-b)
  stderror = sqrt(N) * sd(a - b)
  tab = rbind(dif, stderror)
  rownames(tab) = c(colname1, colname2)
  tab
}

pairdiff(test$elpd_2, test$elpd_4)

samp_data_list2 = samp_data_list[1:60]

## getting all the pairwise elpd_diff and se_diff
diff12 = samp_data_list2 %>% 
  lapply(., function(x)(x[, grep('^elpd',colnames(x))])) %>% 
    lapply(., function(x)pairdiff(x$elpd_1, x$elpd_2, 'elpd_diff_12', 'se_diff_12')) %>% 
  do.call(cbind,.) %>% 
  t() 

diff13 = samp_data_list2 %>% 
  lapply(., function(x)(x[, grep('^elpd',colnames(x))])) %>% 
  lapply(., function(x)pairdiff(x$elpd_1, x$elpd_3, 'elpd_diff_13', 'se_diff_13')) %>% 
  do.call(cbind,.) %>% 
  t()

diff14 = samp_data_list2 %>% 
  lapply(., function(x)(x[, grep('^elpd',colnames(x))])) %>% 
  lapply(., function(x)pairdiff(x$elpd_1, x$elpd_4, 'elpd_diff_14', 'se_diff_14')) %>% 
  do.call(cbind,.) %>% 
  t()

diff23 = samp_data_list2 %>% 
  lapply(., function(x)(x[, grep('^elpd',colnames(x))])) %>% 
  lapply(., function(x)pairdiff(x$elpd_2, x$elpd_3, 'elpd_diff_23', 'se_diff_23')) %>% 
  do.call(cbind,.) %>% 
  t()

diff24 = samp_data_list2 %>% 
  lapply(., function(x)(x[, grep('^elpd',colnames(x))])) %>% 
  lapply(., function(x)pairdiff(x$elpd_2, x$elpd_4, 'elpd_diff_24', 'se_diff_24')) %>% 
  do.call(cbind,.) %>% 
  t()

diff34 = samp_data_list2 %>% 
  lapply(., function(x)(x[, grep('^elpd',colnames(x))])) %>% 
  lapply(., function(x)pairdiff(x$elpd_3, x$elpd_4, 'elpd_diff_34', 'se_diff_34')) %>% 
  do.call(cbind,.) %>% 
  t()

diff_all = (cbind(diff12, diff13, diff14, diff23, diff24, diff34))

diff_elpd_melt = diff_all[,grep('^elpd', colnames(diff_all))] %>% 
  as.data.frame(.) %>% 
  melt(.)

diff_se_melt = diff_all[,grep('^se', colnames(diff_all))] %>% 
  as.data.frame(.) %>% 
  melt(.)

gg1 = ggplot(diff_elpd_melt, aes(x=variable, y=value, fill=variable)) +
  geom_violin() + labs(title="Pairwise elpd_diff") + 
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x="", y="elpd diff")
  
gg2 = ggplot(diff_se_melt, aes(x=variable, y=value, fill=variable)) +
  geom_violin() + labs(title="Pairwise se_diff") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x="", y="std. error diff")

png(file="loo_elpd_se_diff_popn1.png", width=700, height=790)
gridExtra::grid.arrange(gg1,gg2)
dev.off()

# weighted elpd and se diff -----------------------------------------------
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                    weights=~wts, # including raked weights in the survey design
                    data=samp_data_list[[1]])

dat = samp_data_list[1:60]

rake = lapply(dat, function(x)svydesign(ids=~1,weights=~wts,data=x))

diff12 = lapply(1:length(dat), function(x)vector(length = 2))
for(i in 1:length(dat)){
  temp = data.frame(svytotal(dat[[i]]$elpd_1 - dat[[i]]$elpd_2, rake[[i]]))
  colnames(temp) = c('elpd_diff_wtd_12', 'se_diff_wtd_12')
  diff12[[i]] = temp
}
wtd_diff12 = do.call(rbind,diff12)

diff13 = lapply(1:length(dat), function(x)vector(length = 2))
for(i in 1:length(dat)){
  temp = data.frame(svytotal(dat[[i]]$elpd_1 - dat[[i]]$elpd_3, rake[[i]]))
  colnames(temp) = c('elpd_diff_wtd_13', 'se_diff_wtd_13')
  diff13[[i]] = temp
}
wtd_diff13 = do.call(rbind,diff13)

diff14 = lapply(1:length(dat), function(x)vector(length = 2))
for(i in 1:length(dat)){
  temp = data.frame(svytotal(dat[[i]]$elpd_1 - dat[[i]]$elpd_4, rake[[i]]))
  colnames(temp) = c('elpd_diff_wtd_14', 'se_diff_wtd_14')
  diff14[[i]] = temp
}
wtd_diff14 = do.call(rbind,diff14)

diff23 = lapply(1:length(dat), function(x)vector(length = 2))
for(i in 1:length(dat)){
  temp = data.frame(svytotal(dat[[i]]$elpd_2 - dat[[i]]$elpd_3, rake[[i]]))
  colnames(temp) = c('elpd_diff_wtd_23', 'se_diff_wtd_23')
  diff23[[i]] = temp
}
wtd_diff23 = do.call(rbind,diff23)

diff24 = lapply(1:length(dat), function(x)vector(length = 2))
for(i in 1:length(dat)){
  temp = data.frame(svytotal(dat[[i]]$elpd_2 - dat[[i]]$elpd_4, rake[[i]]))
  colnames(temp) = c('elpd_diff_wtd_24', 'se_diff_wtd_24')
  diff24[[i]] = temp
}
wtd_diff24 = do.call(rbind,diff24)

diff34 = lapply(1:length(dat), function(x)vector(length = 2))
for(i in 1:length(dat)){
  temp = data.frame(svytotal(dat[[i]]$elpd_3 - dat[[i]]$elpd_4, rake[[i]]))
  colnames(temp) = c('elpd_diff_wtd_34', 'se_diff_wtd_34')
  diff34[[i]] = temp
}
wtd_diff34 = do.call(rbind,diff34)

## combining and plotting
wtd_diff_all = (cbind(wtd_diff12, wtd_diff13, wtd_diff14, wtd_diff23, wtd_diff24, wtd_diff34))

wtd_diff_elpd_melt = wtd_diff_all[,grep('^elpd', colnames(wtd_diff_all))] %>% 
  as.data.frame(.) %>% 
  melt(.)

wtd_diff_se_melt = wtd_diff_all[,grep('^se', colnames(wtd_diff_all))] %>% 
  as.data.frame(.) %>% 
  melt(.)

gg1 = ggplot(wtd_diff_elpd_melt, aes(x=variable, y=value, fill=variable)) +
  geom_violin() + labs(title="Pairwise elpd_wtd_diff") + 
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(x="", y="elpd wtd_diff")

gg2 = ggplot(wtd_diff_se_melt, aes(x=variable, y=value, fill=variable)) +
  geom_violin() + labs(title="Pairwise se_wtd_diff") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(x="", y="std. error wtd_diff")

png(file="loo_elpd_se_wtd_diff_popn1.png", width=700, height=790)
gridExtra::grid.arrange(gg1,gg2)
dev.off()
