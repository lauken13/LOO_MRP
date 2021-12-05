## plotting the LOOP values against 'best' model 
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment1/simulated100temp_1.RData")

# weighted LOOP -----------------------------------------------------------
loop_diff_01 = list()
loop_diff_02 = list()
loop_diff_03 = list()
loop_diff_04 = list()

## calculating LOOP values and the difference in quantile
for(i in 1:17){
  loop_diff_01[[i]] = sim_list[[i]][1,c(11,12,13)] - sim_list[[i]][5,c(11,12,13)]
  
  loop_diff_02[[i]] = sim_list[[i]][2,c(11,12,13)] - sim_list[[i]][5,c(11,12,13)]
  
  loop_diff_03[[i]] = sim_list[[i]][3,c(11,12,13)] - sim_list[[i]][5,c(11,12,13)]
  
  loop_diff_04[[i]] = sim_list[[i]][4,c(11,12,13)] - sim_list[[i]][5,c(11,12,13)]
}

loop_se_tab1 = do.call(rbind, loop_diff_01) %>% set_rownames(., 1:17)
loop_se_tab1$model = "model01"

loop_se_tab2 = do.call(rbind, loop_diff_02) %>% set_rownames(., 1:17)
loop_se_tab2$model = "model02"

loop_se_tab3 = do.call(rbind, loop_diff_03) %>% set_rownames(., 1:17)
loop_se_tab3$model = "model03"

loop_se_tab4 = do.call(rbind, loop_diff_04) %>% set_rownames(., 1:17)
loop_se_tab4$model = "model04"

loop_se_tab = bind_rows(loop_se_tab1,
                        loop_se_tab2,
                        loop_se_tab3,
                        loop_se_tab4) %>% 
  rename(LOOP.X5 = elpd.popnestX5, 
         LOOP.X50 = elpd.popnestX50,
         LOOP.X95 = elpd.popnestX95)

loop_se_tab$iter = rep(1:17, nrow(sim_list[[1]]) - 1)

#Example plot
ggplot(loop_se_tab, aes(x = LOOP.X50, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  xlim(c(-3800, 2200)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = LOOP.X5, 
                               xmax = LOOP.X95), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -1000, y = 0.5, label = "Model 15 preferred") +
  annotate("label", x = 1000, y = 0.5, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) 

loop1 = loop_se_tab


# models 05-10 ------------------------------------------------------------
load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment1/simulated100temp_2.RData")
## difference in elpd_loo values
sim_doub_list[[1]][,'elpd.popnestX50']

# weighted LOOP -----------------------------------------------------------
loop_diff_05 = list()
loop_diff_06 = list()
loop_diff_07 = list()
loop_diff_08 = list()
loop_diff_09 = list()
loop_diff_10 = list()

## calculating LOOP values and the difference in quantile
for(i in 1:17){
  loop_diff_05[[i]] = sim_doub_list[[i]][1,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
  
  loop_diff_06[[i]] = sim_doub_list[[i]][2,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
  
  loop_diff_07[[i]] = sim_doub_list[[i]][3,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
  
  loop_diff_08[[i]] = sim_doub_list[[i]][4,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
  
  loop_diff_09[[i]] = sim_doub_list[[i]][5,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
  
  loop_diff_10[[i]] = sim_doub_list[[i]][6,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
}

loop_se_tab05 = do.call(rbind, loop_diff_05) %>% set_rownames(., 1:17)
loop_se_tab05$model = "model05"

loop_se_tab06 = do.call(rbind, loop_diff_06) %>% set_rownames(., 1:17)
loop_se_tab06$model = "model06"

loop_se_tab07 = do.call(rbind, loop_diff_07) %>% set_rownames(., 1:17)
loop_se_tab07$model = "model07"

loop_se_tab08 = do.call(rbind, loop_diff_08) %>% set_rownames(., 1:17)
loop_se_tab08$model = "model08"

loop_se_tab09 = do.call(rbind, loop_diff_09) %>% set_rownames(., 1:17)
loop_se_tab09$model = "model09"

loop_se_tab10 = do.call(rbind, loop_diff_10) %>% set_rownames(., 1:17)
loop_se_tab10$model = "model10"

loop_se_tab = bind_rows(loop_se_tab05,
                        loop_se_tab06,
                        loop_se_tab07,
                        loop_se_tab08, 
                        loop_se_tab09,
                        loop_se_tab10) %>% 
  rename(LOOP.X5 = elpd.popnestX5, 
         LOOP.X50 = elpd.popnestX50,
         LOOP.X95 = elpd.popnestX95)

loop_se_tab$iter = rep(1:17, nrow(sim_doub_list[[1]]) - 1)

#Example plot
ggplot(loop_se_tab, aes(x = LOOP.X50, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  xlim(c(-3800, 2200)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = LOOP.X5, 
                               xmax = LOOP.X95), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -1000, y = 0.5, label = "Model 15 preferred") +
  annotate("label", x = 1000, y = 0.5, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) 

loop2 = loop_se_tab

# models 11-15 ------------------------------------------------------------
load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment1/simulated100temp_3.RData")

## difference in elpd_loo values
sim_trip_list[[1]][,'elpd.popnestX50']

# weighted LOOP -----------------------------------------------------------
loop_diff_11 = list()
loop_diff_12 = list()
loop_diff_13 = list()
loop_diff_14 = list()

sim_trip_list[[i]][,c(11,12,13)]

## calculating LOOP values and the difference in quantile
for(i in 1:17){
  loop_diff_11[[i]] = sim_trip_list[[i]][1,c(11,12,13)] - sim_trip_list[[i]][5,c(11,12,13)]
  
  loop_diff_12[[i]] = sim_trip_list[[i]][2,c(11,12,13)] - sim_trip_list[[i]][5,c(11,12,13)]
  
  loop_diff_13[[i]] = sim_trip_list[[i]][3,c(11,12,13)] - sim_trip_list[[i]][5,c(11,12,13)]
  
  loop_diff_14[[i]] = sim_trip_list[[i]][4,c(11,12,13)] - sim_trip_list[[i]][5,c(11,12,13)]
}

loop_se_tab11 = do.call(rbind, loop_diff_11) %>% set_rownames(., 1:17)
loop_se_tab11$model = "model11"

loop_se_tab12 = do.call(rbind, loop_diff_12) %>% set_rownames(., 1:17)
loop_se_tab12$model = "model12"

loop_se_tab13 = do.call(rbind, loop_diff_13) %>% set_rownames(., 1:17)
loop_se_tab13$model = "model13"

loop_se_tab14 = do.call(rbind, loop_diff_14) %>% set_rownames(., 1:17)
loop_se_tab14$model = "model14"

loop_se_tab = bind_rows(loop_se_tab11,
                        loop_se_tab12,
                        loop_se_tab13,
                        loop_se_tab14) %>% 
  rename(LOOP.X5 = elpd.popnestX5, 
         LOOP.X50 = elpd.popnestX50,
         LOOP.X95 = elpd.popnestX95)

loop_se_tab$iter = rep(1:17, nrow(sim_trip_list[[1]]) - 1)

#Example plot
ggplot(loop_se_tab, aes(x = LOOP.X50, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  xlim(c(-3800, 2200)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = LOOP.X5, 
                               xmax = LOOP.X95), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -1000, y = 0.5, label = "Model 15 preferred") +
  annotate("label", x = 1000, y = 0.5, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) 

loop3 = loop_se_tab

loop_tab = bind_rows(loop1,
                     loop2,
                     loop3)

## plot all models together
g1 = ggplot(loop_tab, aes(x = LOOP.X50, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  xlim(c(-500, 300)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = LOOP.X5, 
                               xmax = LOOP.X95), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -200, y = 0.8, label = "Model 15 preferred") +
  annotate("label", x = 200, y = 0.8, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="LOOP values")

ggsave("plot_loop_diff.pdf", g1, width=6, height=7.5, units="in", device="pdf")


g2 = ggplot(loop_tab, aes(group = model, fill = model))+
  geom_vline(aes(xintercept = 0)) +
  xlim(c(-500, 300)) +
  geom_violin(aes(x = LOOP.X5, y = model),alpha=0.3) +
  geom_violin(aes(x = LOOP.X95, y = model), alpha=0.3) +
  geom_violin(aes(x = LOOP.X50, y = model), alpha=1) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -200, y = 0.8, label = "Model 15 preferred") +
  annotate("label", x = 200, y = 0.8, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="LOOP values")

ggsave("plot_loop_diff_violin.pdf", g2, width=6, height=7.5, units="in", device="pdf")



