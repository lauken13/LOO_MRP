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

popn_counts = rbind(tabX1, tabX2, tabX3, tabX4) %>% 
  as_tibble() %>% 
 # rownames_to_column(var = 'sae') %>% 
  mutate(sae =  paste0('X',1:4)) %>% 
  pivot_longer(`1`:`5`, names_to = 'group') 

