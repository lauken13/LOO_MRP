library(dplyr)
library(ggplot2)

res_list_N02_1000 <- readRDS(here::here("02-super popn approach/experiment1_baseWtdloo/03-data/res_list_N02_1000_new.rds"))
samp_wts <- readRDS(here::here("02-super popn approach/experiment1_baseWtdloo/03-data/samp_wts.rds"))


ls(res_list_N02_1000)
indv_all_tab = res_list_N02_1000$indv_all_tab
popn_indv_tab = res_list_N02_1000$popn_indv_tab
names(popn_indv_tab)
nrow(popn_indv_tab)

t1 = indv_all_tab %>% 
  group_by(model, iteration) %>% 
  summarise(ind_intervalScr_mean = mean(ind_intervalScr))
names(indv_all_tab)
nrow(indv_all_tab)
nrow(t1) # mean for each model in each iteration

t1$model = forcats::fct_relevel(t1$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                            'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                            'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                            'X1', 'X3', 'X1 + X3'))

popn_indv_tab$model = forcats::fct_relevel(popn_indv_tab$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                                                  'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                                                  'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                                                  'X1', 'X3', 'X1 + X3'))
names(popn_indv_tab)

identical(t1$ind_intervalScr_mean, popn_indv_tab$ind_intervalScr_mean)

ggplot(popn_indv_tab, aes(x = wtdElpd_loo, y = ind_intervalScr_mean)) +
  geom_point()




## calculating popn indv interval score 
alph = 0.9
t3 = indv_all_tab %>% 
  mutate(popnindv_intervalScr = (sampestX95 - sampestX5) + 
  ((2 / alph * (sampestX5 - y_prob)) * ifelse(y_prob < sampestX5, 1, 0)) + 
  ((2 / alph * (y_prob - sampestX95)) * ifelse(y_prob > sampestX95, 1, 0))) %>% 
  group_by(model, iteration) %>% 
  summarise(mean_intervalScr_popnind = mean(popnindv_intervalScr))
names(t3)

t3$model = forcats::fct_relevel(t3$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                            'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                            'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                            'X1', 'X3', 'X1 + X3'))

t4 = popn_indv_tab %>% 
  rename(mean_intervalScr_mrp = 'ind_intervalScr_mean') %>% # renaming the interval scr as it's for the mrp estimate
  left_join(., t3, by=c('model', 'iteration'))
names(t4)

colour_palette_var_base  =  c(`X2 + X4` =  "#4477AA",
                              `X1 + X2 + X4` = "#88CCEE", 
                              `X2 + X3 + X4` = "#66CCEE",
                              `X1 + X2 + X3 + X4` = "#332288",
                              `X4` = "#CC6677", `X1 + X4` = "#AA4499", 
                              `X3 + X4` = "#EE6677", `X1 + X3 + X4` = "#882255",
                              `X2` = "#228833", `X1 + X2` = "#117733",
                              `X2 + X3` = "#999933", `X1 + X2 + X3` = "#DDCC77",
                              `X1` = "#DDDDDD", `X3` = "#BBBBBB", `X1 + X3` = "#879195FF")
ggplot(t4, aes(x=wtdElpd_loo, y = mean_intervalScr_popnind, group=model) ) +
  geom_point(shape=16, alpha=0.4, size=2, aes(colour = model)) +
  scale_color_manual(values=colour_palette_var_base) + 
  theme(legend.position = "",
        legend.title = element_blank(),
        plot.margin = margin(10, 70, 10, 10)) 
