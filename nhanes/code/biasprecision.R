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

# ethnicity 
( g1 = nhfinal %>% 
  mutate(high_bp = as.numeric(high_bp)-1) %>% 
  group_by(ethnicity) %>% 
  summarise(prob_highBP = mean(high_bp)) %>% 
ggplot(aes(x=ethnicity, y=prob_highBP)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('Probability of high blood pressure') +
    ylim(0,1) )

( tab_dr2 = nhsub_dr2 %>% 
  group_by(ethnicity) %>% 
  summarise(prop_resp = n()/nrow(nhsub_dr2)) %>% 
    mutate(sample = "dietary") ) 

( tab_fas = nhsub_fas %>% 
  group_by(ethnicity) %>% 
  summarise(prop_resp = n()/nrow(nhsub_fas))  %>% 
    mutate(sample = "fasting") )

( g2 =  rbind(tab_dr2, tab_fas) %>% 
ggplot(aes(x=ethnicity, y=prop_resp, fill=sample, colour = sample)) +
  geom_point(size=2) +
   ylab('Proportion of respondents in sample') +
   ggtitle('Proportion of respondents in sample') +
    ylim(0,1) )

ggpubr::ggarrange(g1, g2)

# overweight
( g1 = nhfinal %>% 
    mutate(high_bp = as.numeric(high_bp)-1) %>% 
    group_by(overweight) %>% 
    summarise(prob_highBP = mean(high_bp)) %>% 
    ggplot(aes(x=overweight, y=prob_highBP)) +
    geom_point(size=2) +
    ggtitle('Probability of high blood pressure') + 
    ylab('Probability of high blood pressure') +
    ylim(0,1) )

( tab_dr2 = nhsub_dr2 %>% 
    group_by(overweight) %>% 
    summarise(prop_resp = n()/nrow(nhsub_dr2)) %>% 
    mutate(sample = "dietary") ) 

( tab_fas = nhsub_fas %>% 
    group_by(overweight) %>% 
    summarise(prop_resp = n()/nrow(nhsub_fas))  %>% 
    mutate(sample = "fasting") )

( g2 =  rbind(tab_dr2, tab_fas) %>% 
    ggplot(aes(x=overweight, y=prop_resp, fill=sample, colour = sample)) +
    geom_point(size=2) +
    ylab('Proportion of respondents in sample') +
    ggtitle('Proportion of respondents in sample')  +
    ylim(0,1) )

ggpubr::ggarrange(g1, g2)

# age
g1 = nhfinal %>% 
    mutate(high_bp = as.numeric(high_bp)-1) %>% 
    group_by(age) %>% 
    summarise(prob_highBP = mean(high_bp)) %>% 
    ggplot(aes(x=age, y=prob_highBP)) +
    geom_point(size=2) +
    ggtitle('Probability of high blood pressure') + 
    ylab('Probability of high blood pressure') +
    ylim(0,1) 

( tab_dr2 = nhsub_dr2 %>% 
    group_by(age) %>% 
    summarise(prop_resp = n()/nrow(nhsub_dr2)) %>% 
    mutate(sample = "dietary") ) 

( tab_fas = nhsub_fas %>% 
    group_by(age) %>% 
    summarise(prop_resp = n()/nrow(nhsub_fas))  %>% 
    mutate(sample = "fasting") )

g2 =  rbind(tab_dr2, tab_fas) %>% 
    ggplot(aes(x=age, y=prop_resp, fill=sample, colour = sample)) +
    geom_point(size=2) +
    ylab('Proportion of respondents in sample') +
    ggtitle('Proportion of respondents in sample') +
    ylim(0,1) 

ggpubr::ggarrange(g1, g2)

# marital status
g1 = nhfinal %>% 
    mutate(high_bp = as.numeric(high_bp)-1) %>% 
    group_by(marital_status) %>% 
    summarise(prob_highBP = mean(high_bp)) %>% 
    ggplot(aes(x=marital_status, y=prob_highBP)) +
    geom_point(size=2) +
    ggtitle('Probability of high blood pressure') + 
    ylab('Probability of high blood pressure') +
  ylim(0,1) 

( tab_dr2 = nhsub_dr2 %>% 
    group_by(marital_status) %>% 
    summarise(prop_resp = n()/nrow(nhsub_dr2)) %>% 
    mutate(sample = "dietary") ) 

( tab_fas = nhsub_fas %>% 
    group_by(marital_status) %>% 
    summarise(prop_resp = n()/nrow(nhsub_fas))  %>% 
    mutate(sample = "fasting") )

g2 =  rbind(tab_dr2, tab_fas) %>% 
    ggplot(aes(x=marital_status, y=prop_resp, fill=sample, colour = sample)) +
    geom_point(size=2) +
    ylab('Proportion of respondents in sample') +
    ggtitle('Proportion of respondents in sample') +
    ylim(0,1)

ggpubr::ggarrange(g1, g2)


