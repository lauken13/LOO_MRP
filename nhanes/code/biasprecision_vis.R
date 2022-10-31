library(brms)
library(survey) # svydesign()
library(here)
library(tidyverse)
source(here('02-super popn approach/functions.R'))
nhfinal = readRDS(file="nhanes/data/nhanes_final.rds")

nhsub_dr2 = nhfinal %>% 
  filter(incl_dr2 == 1) 

nhsub_fas = nhfinal %>% 
  filter(incl_fas == 1) 


# ethnicity  --------------------------------------------------------------
# prob of outcome
( tab_popn_eth = nhfinal %>% 
  mutate(high_bp = as.numeric(high_bp)-1) %>% 
  group_by(ethnicity) %>% 
  summarise(prob_highBP = mean(high_bp),
            total = n()) )

( tab_dr2_eth = nhsub_dr2 %>% 
    group_by(ethnicity) %>% 
    summarise(prop_resp = n()/nrow(nhsub_dr2),
              prob_highBP = mean(as.numeric(high_bp)-1),
              total = n())  %>% 
    mutate(sample = "dietary") ) 

( tab_fas_eth = nhsub_fas %>% 
  group_by(ethnicity) %>% 
  summarise(prop_resp = n()/nrow(nhsub_fas),
            prob_highBP = mean(as.numeric(high_bp)-1),
            total = n())  %>% 
    mutate(sample = "fasting") )

tab_samp_eth = rbind(tab_dr2_eth, tab_fas_eth)

( g2_eth =  tab_samp_eth %>% 
ggplot(aes(x=ethnicity, y=prop_resp, colour = sample)) +
  geom_point(size=2) +
   ylab('Proportion of respondents in sample') +
   ggtitle('Proportion of respondents in sample') +
    ylim(0,1) )

( g1_eth = tab_popn_eth %>% 
  ggplot(aes(x=ethnicity, y=prob_highBP)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('Probability of high blood pressure') +
  ylim(0,1) +
    geom_point(aes(x=ethnicity, y = prob_highBP, colour=factor(sample)), data = tab_samp_eth))

ggpubr::ggarrange(g1_eth, g2_eth)

# overweight
( tab_popn_ow = nhfinal %>% 
    mutate(high_bp = as.numeric(high_bp)-1) %>% 
    group_by(overweight) %>% 
    summarise(prob_highBP = mean(high_bp),
              total = n()) )

( tab_dr2_ow = nhsub_dr2 %>% 
    group_by(overweight) %>% 
    summarise(prop_resp = n()/nrow(nhsub_dr2),
              prob_highBP = mean(as.numeric(high_bp)-1),
              total = n())  %>% 
    mutate(sample = "dietary") ) 

( tab_fas_ow = nhsub_fas %>% 
    group_by(overweight) %>% 
    summarise(prop_resp = n()/nrow(nhsub_fas),
              prob_highBP = mean(as.numeric(high_bp)-1),
              total = n())  %>%  
    mutate(sample = "fasting") )

tab_samp_ow = rbind(tab_dr2_ow, tab_fas_ow)

( g2_ow =  tab_samp_ow %>% 
    ggplot(aes(x=overweight, y=prop_resp, colour = sample)) +
    geom_point(size=2) +
    ylab('Proportion of respondents in sample') +
    ggtitle('Proportion of respondents in sample')  +
    ylim(0,1) )

( g1_ow = tab_popn_ow %>% 
  ggplot(aes(x=overweight, y=prob_highBP)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('Probability of high blood pressure') +
  ylim(0,1) + 
  geom_point(aes(x=overweight, y = prob_highBP, colour=factor(sample)), data = tab_samp_ow) ) 


ggpubr::ggarrange(g1_ow, g2_ow)

# age
tab_popn_age = nhfinal %>% 
    mutate(high_bp = as.numeric(high_bp)-1) %>% 
    group_by(age) %>% 
    summarise(prob_highBP = mean(high_bp)) 

( tab_dr2_age = nhsub_dr2 %>% 
    group_by(age) %>% 
    summarise(prop_resp = n()/nrow(nhsub_dr2),
              prob_highBP = mean(as.numeric(high_bp)-1),
              total = n())  %>% 
    mutate(sample = "dietary") ) 

( tab_fas_age = nhsub_fas %>% 
    group_by(age) %>% 
    summarise(prop_resp = n()/nrow(nhsub_fas),
              prob_highBP = mean(as.numeric(high_bp)-1),
              total = n())  %>% 
    mutate(sample = "fasting") )

tab_samp_age =  rbind(tab_dr2_age, tab_fas_age) 

( g2_age = tab_samp_age %>% 
    ggplot(aes(x=age, y=prop_resp, colour = sample)) +
    geom_point(size=2) +
    ylab('Proportion of respondents in sample') +
    ggtitle('Proportion of respondents in sample') +
    ylim(0,1) )

( g1_age = tab_popn_age %>% 
  ggplot(aes(x=age, y=prob_highBP)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('Probability of high blood pressure') +
  ylim(0,1) + 
  geom_point(aes(x=age, y = prob_highBP, colour=factor(sample)), data = tab_samp_age) ) 


ggpubr::ggarrange(g1_age, g2_age)

# marital status
tab_popn_ms = nhfinal %>% 
    mutate(high_bp = as.numeric(high_bp)-1) %>% 
    group_by(marital_status) %>% 
    summarise(prob_highBP = mean(high_bp)) 

( tab_dr2_ms = nhsub_dr2 %>% 
    group_by(marital_status) %>% 
    summarise(prop_resp = n()/nrow(nhsub_dr2),
              prob_highBP = mean(as.numeric(high_bp)-1),
              total = n())  %>% 
    mutate(sample = "dietary") ) 

( tab_fas_ms = nhsub_fas %>% 
    group_by(marital_status) %>% 
    summarise(prop_resp = n()/nrow(nhsub_fas),
              prob_highBP = mean(as.numeric(high_bp)-1),
              total = n())  %>% 
    mutate(sample = "fasting") )

tab_samp_ms =  rbind(tab_dr2_ms, tab_fas_ms)

g2_ms = tab_samp_ms %>% 
    ggplot(aes(x=marital_status, y=prop_resp, colour = sample)) +
    geom_point(size=2) +
    ylab('Proportion of respondents in sample') +
    ggtitle('Proportion of respondents in sample') +
    ylim(0,1)

g1_ms = tab_popn_ms %>% 
    ggplot(aes(x=marital_status, y=prob_highBP)) +
    geom_point(size=2) +
    ggtitle('Probability of high blood pressure') + 
    ylab('Probability of high blood pressure') +
    ylim(0,1) + 
    geom_point(aes(x=marital_status, y = prob_highBP, colour=factor(sample)), data = tab_samp_ms) 

ggpubr::ggarrange(g1_ms, g2_ms)


