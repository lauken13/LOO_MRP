# choosing bias precision -------------------------------------------------
nhnew = readRDS(file=here("nhanes/data/nhnew.rds"))
names(nhnew)

corInd = c(2:19, 28:34) 
m1 = model.matrix(~., nhnew[,corInd])
corM = cor(m1[,-1])
sort(abs(corM[,'high_bp1']))

covInd = c(2:6, 8:25) 
m = model.matrix(~., nhnew[,covInd])
y = nhnew[,7]


# cross validation to select lambda
set.seed(1237489)
mod_cv = cv.glmnet(x=m, y=y, family="binomial", intercept=F, nfolds=5)
coef(mod_cv, c(mod_cv$lambda.min,
               mod_cv$lambda.1se))

mod_incl_cv_voc = cv.glmnet(x=m, y=nhnew$incl_voc, family="binomial", intercept=F, nfolds = 5)
coef(mod_incl_cv_voc, c(mod_incl_cv_voc$lambda.min,
                        mod_incl_cv_voc$lambda.1se))

mod_incl_cv_dr1 = cv.glmnet(x=m, y=nhnew$incl_dr1, family="binomial", intercept=F, nfolds = 5)
coef(mod_incl_cv_dr1, c(mod_incl_cv_dr1$lambda.min,
                        mod_incl_cv_dr1$lambda.1se))

mod_incl_cv_urn = cv.glmnet(x=m, y=nhnew$incl_urn, family="binomial", intercept=F, nfolds = 5)
coef(mod_incl_cv_urn, c(mod_incl_cv_urn$lambda.min,
                        mod_incl_cv_urn$lambda.1se))

mod_incl_cv_fas = cv.glmnet(x=m, y=nhnew$incl_fas, family="binomial", intercept=F, nfolds = 5)
coef(mod_incl_cv_fas, c(mod_incl_cv_fas$lambda.min,
                        mod_incl_cv_fas$lambda.1se))

