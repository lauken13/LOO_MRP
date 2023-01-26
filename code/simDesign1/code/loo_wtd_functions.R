## List of external functions 

## wrapper function for calculating weighted loo's for two models ####
loo_compare_wtd <- function(loo_a, loo_b, svydesign_obj){
  # load survey package
  library(survey)
  
  # computing differences in elpd_loo 
  elpd_diffs <- function(loo_a, loo_b) {
    pt_a <- loo_a$pointwise
    pt_b <- loo_b$pointwise
    elpd <- grep("^elpd", colnames(pt_a))
    pt_b[, elpd] - pt_a[, elpd]
  }
  
  # differences between models
  elpd_diff_11 = elpd_diffs(loo_a, loo_a)
  elpd_diff_21 = elpd_diffs(loo_a, loo_b)
  
  # using svytotal to calculate weighted elpd
  diff_11 = svytotal(elpd_diff_11, svydesign_obj)
  diff_12 = svytotal(elpd_diff_21, svydesign_obj)
  
  # tabulate the differences
  tbl = rbind(as.data.frame(diff_11), as.data.frame(diff_12))
  tbl = format(tbl, digits=1, nsmall=1)
  rownames(tbl) = c('model1', 'model2')
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
