## plotting the LOOP values against 'best' model 
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment3e/simulated100_2.RData")


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
for(i in 1:length(sim_doub_list)){
  loop_diff_05[[i]] = sim_doub_list[[i]][1,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
  
  loop_diff_06[[i]] = sim_doub_list[[i]][2,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
  
  loop_diff_07[[i]] = sim_doub_list[[i]][3,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
  
  loop_diff_08[[i]] = sim_doub_list[[i]][4,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
  
  loop_diff_09[[i]] = sim_doub_list[[i]][5,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
  
  loop_diff_10[[i]] = sim_doub_list[[i]][6,c(11,12,13)] - sim_doub_list[[i]][7,c(11,12,13)]
}

loop_se_tab05 = do.call(rbind, loop_diff_05) %>% set_rownames(., 1:100)
loop_se_tab05$model = "model05"

loop_se_tab06 = do.call(rbind, loop_diff_06) %>% set_rownames(., 1:100)
loop_se_tab06$model = "model06"

loop_se_tab07 = do.call(rbind, loop_diff_07) %>% set_rownames(., 1:100)
loop_se_tab07$model = "model07"

loop_se_tab08 = do.call(rbind, loop_diff_08) %>% set_rownames(., 1:100)
loop_se_tab08$model = "model08"

loop_se_tab09 = do.call(rbind, loop_diff_09) %>% set_rownames(., 1:100)
loop_se_tab09$model = "model09"

loop_se_tab10 = do.call(rbind, loop_diff_10) %>% set_rownames(., 1:100)
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

loop_se_tab$iter = rep(1:length(samp_data_list), nrow(sim_doub_list[[1]]) - 1)

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

