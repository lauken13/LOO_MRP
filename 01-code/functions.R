## List of external functions 

## wrapper function for calculating weighted loo's for two models ####
loo_compare_wtd <- function(loo_a, loo_b, svydesign_obj){
  # load survey package
  library(survey)
  
  # computing differences in elpd_loo 
  elpd_diffs <- function(loo_1, loo_2) {
    pt_a <- loo_1$pointwise
    pt_b <- loo_2$pointwise
    elpd <- grep("^elpd", colnames(pt_a))
    pt_b[, elpd] - pt_a[, elpd]
  }
  
  # differences between models
  elpd_diff_11 = elpd_diffs(loo_a, loo_a)
  elpd_diff_12 = elpd_diffs(loo_b, loo_a) # diff loo_a - loo_b
  elpd_diff_21 = elpd_diffs(loo_a, loo_b)
  elpd_diff_22 = elpd_diffs(loo_b, loo_b)
  
  # using svytotal to calculate weighted elpd
  diff_11 = svytotal(elpd_diff_11, svydesign_obj) %>% 
    as.data.frame()
  diff_12 = svytotal(elpd_diff_12, svydesign_obj) %>% 
    as.data.frame()
  diff_21 = svytotal(elpd_diff_21, svydesign_obj) %>% 
    as.data.frame()
  diff_22 = svytotal(elpd_diff_22, svydesign_obj) %>% 
    as.data.frame()
  
  
  if (diff_12$total > 0){
    tbl = rbind(diff_22, diff_21)
    rownames(tbl) = c('model2', 'model1')
  } else if (diff_12$total < 0){
    tbl = rbind(diff_11, diff_12)
    rownames(tbl) = c('model2', 'model1')
  } 
  tbl = format(tbl, digits=1, nsmall=1)
  colnames(tbl) = c('elpd_diff', 'se_diff')
  print(tbl)
}


loo_wtd <- function(loo_a, svydesign_obj){
  # load survey package
  library(survey)
  
  pt_a <- loo_a$pointwise
  elpd <- grep("^elpd", colnames(pt_a))
  loo_a_elpd <- pt_a[, elpd]
  
  # using svytotal to calculate weighted elpd
  loo_svydsn <- svytotal(loo_a_elpd, svydesign_obj) %>% data.frame(.)
  colnames(loo_svydsn) = c('wtd_elpd_loo', 'wtd_SE')
  loo_svydsn
}


pairdiff <- function(a, b, colname1, colname2){
  dif = sum(a-b)
  stderror = sqrt(N) * sd(a - b)
  tab = rbind(dif, stderror)
  rownames(tab) = c(colname1, colname2)
  tab
}
