## Data cleaning
library(here)
library(foreign) # read.xport()
library(tidyverse)
library(questionr) # recode.na()
library(GGally) # ggpairs()
library(forcats) #fct_relevel
library(glmnet)

dat4 = readRDS(file=here("nhanes/data/nhdata_full.rds"))

# checking correlation with inclusion
corIncl = c(2:28)
dat4 = dat4 %>% 
  mutate_if(is.numeric, as.factor)
m2 = model.matrix(~., dat4[corIncl], contrasts.arg = lapply(dat4[,corIncl], contrasts, contrasts=FALSE)) # making model matrix without a baseline category for the variables
corM2 = cor(m2[,-1])

# View(abs(corM2))
View(abs(corM2[81:89,81:89]))
 View(abs(corM2[63:89,84:89])) # ranking the variables 
 
 
# model matrix for lasso
names(dat4)
dat5 = dat4 %>% 
  select(-c(pest_use)) # after looking at correlation
names(dat5)

covInd = c(2:23)[-c(6)] # removing high bp 
m = model.matrix(~ . -1, data = dat5[,covInd], contrasts.arg = lapply(dat5[,covInd], contrasts, contrasts=FALSE))
y = as.numeric(dat5[,7]) - 1 # high bp

# fixing foldids
nfolds = 15
foldid = 1 + (1:nrow(m) %% nfolds) # no need to specify seed if setting fold ID
ysel = cbind(y, dat5$incl_dr2, dat5$incl_fas, dat5$incl_voc, dat5$incl_env)
coef_mat = NULL
for(i in 1:ncol(ysel)){
  (lmb = cv.glmnet(m,ysel[,i],alpha = 1, family = "binomial", intercept=T, type.measure ="auc",foldid=foldid)$lambda.1se)
  coef_vec = glmnet(x=m, y=ysel[,i], family="binomial", intercept=T, type.measure = "auc", lambda = lmb) %>% coef(.)
  coef_mat = cbind(coef_mat, coef_vec)
}
colnames(coef_mat) = c('high_bp', 'dietary', 'fas', 'voc', 'env')
coef_mat


# removing ignorable variables --------------------------------------------
dat6 = dat5 %>% 
  select(-c(dep_severity,smk2_exp,potassium_lvl, den_vst, incl_env, incl_voc))

names(dat6)

covInd = c(2:19)[-c(6)] # removing high bp 
m = model.matrix(~ . -1, data = dat6[,covInd], contrasts.arg = lapply(dat6[,covInd], contrasts, contrasts=FALSE))
y = as.numeric(dat6[,7]) - 1 # high bp

# fixing foldids
nfolds = 15
foldid = 1 + (1:nrow(m) %% nfolds) # no need to specify seed if setting fold ID
ysel = cbind(y, dat6$incl_dr2, dat6$incl_fas)
coef_mat = NULL
for(i in 1:ncol(ysel)){
  (lmb = cv.glmnet(m,ysel[,i],alpha = 1, family = "binomial", intercept=T, type.measure ="auc",foldid=foldid)$lambda.1se)
  coef_vec = glmnet(x=m, y=ysel[,i], family="binomial", intercept=T, type.measure = "auc", lambda = lmb) %>% coef(.)
  coef_mat = cbind(coef_mat, coef_vec)
}
colnames(coef_mat) = c('high_bp', 'dietary', 'fas')
coef_mat

saveRDS(dat6, file=here("nhanes/data/nhanes_final.rds"))

