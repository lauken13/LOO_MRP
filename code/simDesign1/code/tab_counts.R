tabX1 = lapply(popn_data_list, function(x){ table(x$X1)}) %>% 
  do.call(rbind, .) %>% 
  apply(., 2, mean) %>% 
  round(.)

tabX2 = lapply(popn_data_list, function(x){ table(x$X2)}) %>% 
  do.call(rbind, .) %>% 
  apply(., 2, mean) %>% 
  round(.)

tabX3 = lapply(popn_data_list, function(x){ table(x$X3)}) %>% 
  do.call(rbind, .) %>% 
  apply(., 2, mean) %>% 
  round(.)

tabX4 = lapply(popn_data_list, function(x){ table(x$X4)}) %>% 
  do.call(rbind, .) %>% 
  apply(., 2, mean) %>% 
  round(.)

# population counts
popn_counts = rbind(tabX1, tabX2, tabX3, tabX4) %>% 
  as_tibble() %>% 
 # rownames_to_column(var = 'sae') %>% 
  mutate(sae =  paste0('X',1:4)) %>% 
  pivot_longer(`1`:`5`, names_to = 'group') 

sampX1 = lapply(samp_data_list, function(x){ table(x$X1)}) %>% 
  do.call(rbind, .) %>% 
  apply(., 2, mean) %>% 
  round(.)

sampX2 = lapply(samp_data_list, function(x){ table(x$X2)}) %>% 
  do.call(rbind, .) %>% 
  apply(., 2, mean) %>% 
  round(.)

sampX3 = lapply(samp_data_list, function(x){ table(x$X3)}) %>% 
  do.call(rbind, .) %>% 
  apply(., 2, mean) %>% 
  round(.)

sampX4 = lapply(samp_data_list, function(x){ table(x$X4)}) %>% 
  do.call(rbind, .) %>% 
  apply(., 2, mean) %>% 
  round(.)

# sample counts
samp_counts = rbind(sampX1, sampX2, sampX3, sampX4) %>% 
  as_tibble() %>% 
  # rownames_to_column(var = 'sae') %>% 
  mutate(sae =  paste0('X',1:4)) %>% 
  pivot_longer(`1`:`5`, names_to = 'group') 