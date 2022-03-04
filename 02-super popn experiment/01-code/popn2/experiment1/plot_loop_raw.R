## plotting the raw LOOP values 
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment2/simulated100_1.RData")

# weighted LOOP -----------------------------------------------------------
loop_01 = list()
loop_02 = list()
loop_03 = list()
loop_04 = list()

## calculating LOOP values and the difference in quantile
for(i in 1:length(sim_list)){
  loop_01[[i]] = sim_list[[i]][1,c(11,12,13)] 
  loop_02[[i]] = sim_list[[i]][2,c(11,12,13)]
  loop_03[[i]] = sim_list[[i]][3,c(11,12,13)]
  loop_04[[i]] = sim_list[[i]][4,c(11,12,13)]
}

loop_se_tab1 = do.call(rbind, loop_01) %>% 
  set_rownames(., 1:100) %>% 
  mutate(model = 'model01')

loop_se_tab2 = do.call(rbind, loop_02) %>% 
  set_rownames(., 1:100) %>% 
  mutate(model = 'model02')

loop_se_tab3 = do.call(rbind, loop_03) %>% 
  set_rownames(., 1:100) %>% 
  mutate(model = 'model03')

loop_se_tab4 = do.call(rbind, loop_04) %>% 
  set_rownames(., 1:100) %>% 
  mutate(model = 'model04')

loop_se_tab = bind_rows(loop_se_tab1,
                        loop_se_tab2,
                        loop_se_tab3,
                        loop_se_tab4) %>% 
  dplyr::rename(., LOOP.X5 = elpd.popnestX5, 
         LOOP.X50 = elpd.popnestX50,
         LOOP.X95 = elpd.popnestX95) %>% 
  mutate(iter = rep(1:100, nrow(sim_list[[1]]) - 1))

loop1 = loop_se_tab


# models 05-10 ------------------------------------------------------------
load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment2/simulated100temp_2.RData")

sim_list2 = sim_doub_list[1:40]

# weighted LOOP -----------------------------------------------------------
loop_05 = list()
loop_06 = list()
loop_07 = list()
loop_08 = list()
loop_09 = list()
loop_10 = list()

## calculating LOOP values and the difference in quantile
for(i in 1:length(sim_list2)){
  loop_05[[i]] = sim_list2[[i]][1,c(11,12,13)] 
  loop_06[[i]] = sim_list2[[i]][2,c(11,12,13)] 
  loop_07[[i]] = sim_list2[[i]][3,c(11,12,13)] 
  loop_08[[i]] = sim_list2[[i]][4,c(11,12,13)] 
  loop_09[[i]] = sim_list2[[i]][5,c(11,12,13)] 
  loop_10[[i]] = sim_list2[[i]][6,c(11,12,13)] 
}

loop_se_tab05 = do.call(rbind, loop_05) %>% 
  set_rownames(., 1:length(sim_list2)) %>% 
  mutate(model = "model05")

loop_se_tab06 = do.call(rbind, loop_06) %>% 
  set_rownames(., 1:length(sim_list2)) %>% 
  mutate(model = "model06")

loop_se_tab07 = do.call(rbind, loop_07) %>% 
  set_rownames(., 1:length(sim_list2)) %>% 
  mutate(model = "model07")

loop_se_tab08 = do.call(rbind, loop_08) %>% 
  set_rownames(., 1:length(sim_list2)) %>% 
  mutate(model = "model08")

loop_se_tab09 = do.call(rbind, loop_09) %>% 
  set_rownames(., 1:length(sim_list2)) %>% 
  mutate(model = "model09")

loop_se_tab10 = do.call(rbind, loop_10) %>% 
  set_rownames(., 1:length(sim_list2)) %>% 
  mutate(model = "model10")

loop_se_tab = bind_rows(loop_se_tab05,
                        loop_se_tab06,
                        loop_se_tab07,
                        loop_se_tab08, 
                        loop_se_tab09,
                        loop_se_tab10) %>% 
  dplyr::rename(LOOP.X5 = elpd.popnestX5, 
         LOOP.X50 = elpd.popnestX50,
         LOOP.X95 = elpd.popnestX95) %>% 
  mutate(iter = rep(1:length(sim_list2), nrow(sim_list2[[1]]) - 1))

loop2 = loop_se_tab

# models 11-15 ------------------------------------------------------------
load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment2/simulated100temp_3.RData")

sim_list3 = sim_trip_list[1:52]

# weighted LOOP -----------------------------------------------------------
loop_11 = list()
loop_12 = list()
loop_13 = list()
loop_14 = list()
loop_15 = list()

sim_list3[[i]][,c(11,12,13)]

## calculating LOOP values and the difference in quantile
for(i in 1:length(sim_list3)){
  loop_11[[i]] = sim_list3[[i]][1,c(11,12,13)]
  loop_12[[i]] = sim_list3[[i]][2,c(11,12,13)] 
  loop_13[[i]] = sim_list3[[i]][3,c(11,12,13)] 
  loop_14[[i]] = sim_list3[[i]][4,c(11,12,13)] 
  loop_15[[i]] = sim_list3[[i]][5,c(11,12,13)] 
}

loop_se_tab11 = do.call(rbind, loop_11) %>%
  set_rownames(., 1:length(sim_list3)) %>% 
  mutate(model = "model11")

loop_se_tab12 = do.call(rbind, loop_12) %>% 
  set_rownames(., 1:length(sim_list3)) %>% 
  mutate(model = "model12")

loop_se_tab13 = do.call(rbind, loop_13) %>% 
  set_rownames(., 1:length(sim_list3)) %>% 
  mutate(model = "model13")

loop_se_tab14 = do.call(rbind, loop_14) %>% 
  set_rownames(., 1:length(sim_list3)) %>% 
  mutate(model = "model14")

loop_se_tab15 = do.call(rbind, loop_15) %>% 
  set_rownames(., 1:length(sim_list3)) %>% 
  mutate(model = "model15")

loop_se_tab = bind_rows(loop_se_tab11,
                        loop_se_tab12,
                        loop_se_tab13,
                        loop_se_tab14,
                        loop_se_tab15) %>% 
  dplyr::rename(LOOP.X5 = elpd.popnestX5, 
                LOOP.X50 = elpd.popnestX50,
                LOOP.X95 = elpd.popnestX95) %>% 
  mutate(iter = rep(1:length(sim_list3), nrow(sim_list3[[1]])))


loop3 = loop_se_tab

loop_tab = bind_rows(loop1,loop2,loop3) %>% 
  mutate(model = factor(model)) %>% 
  mutate(model = plyr::revalue(model, c('model01' = 'X1',
                                  'model02' = 'X2',
                                  'model03' = 'X3',
                                  'model04' = 'X4',
                                  'model05' = 'X1 + X2',
                                  'model06' = 'X1 + X3',
                                  'model07' = 'X1 + X4',
                                  'model08' = 'X2 + X3', 
                                  'model09' = 'X2 + X4',
                                  'model10' = 'X3 + X4',
                                  'model11' = 'X1 + X2 + X3', 
                                  'model12' = 'X1 + X2 + X4', 
                                  'model13' = 'X1 + X3 + X4', 
                                  'model14' = 'X2 + X3 + X4', 
                                  'model15' = 'X1 + X2 + X3 + X4'))) 
loop_tab$model = fct_relevel(loop_tab$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                               'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                               'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                               'X1', 'X3', 'X1 + X3'))


## plot all models together
g1 = ggplot(loop_tab, aes(x = LOOP.X50, y = model, group = iter, colour = model)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = LOOP.X5, 
                               xmax = LOOP.X95), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
  labs(title="Raw LOOP values") +
  annotate("label", x = -7000, y = 13.5, label = "X2 and X4") +
  annotate("label", x = -3100, y = 9.5, label = "X4 only") +
  annotate("label", x = -3100, y = 5.5,  label = "X2 only") +
  annotate("label", x = -3100, y = 2, label = "None") 

ggsave("plot_loop.png", g1, width=6, height=7.5, units="in", device="png")

