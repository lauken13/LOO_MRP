library(glmnet)

nhdata = readRDS(file="nhanes/data/nhdata_full.rds")

names(nhdata)
m = model.matrix(~., nhdata[,-c(1,7,20:27)])

y = nhdata[,7]


# cross validation to select lambda
set.seed(1237489)
mod_cv = cv.glmnet(x=m, y=y, family="binomial", intercept=F, nfolds=5)
coef(mod_cv, c(mod_cv$lambda.1se))

mod_incl_cv_voc = cv.glmnet(x=m, y=nhdata$incl_voc, family="binomial", intercept=F, nfolds = 5)
coef(mod_incl_cv_voc, c(mod_incl_cv_voc$lambda.1se))

mod_incl_cv_dr1 = cv.glmnet(x=m, y=nhdata$incl_dr1, family="binomial", intercept=F, nfolds = 5)
coef(mod_incl_cv_dr1, c(mod_incl_cv_dr1$lambda.1se))

mod_incl_cv_urn = cv.glmnet(x=m, y=nhdata$incl_urn, family="binomial", intercept=F, nfolds = 5)
coef(mod_incl_cv_urn, c(mod_incl_cv_urn$lambda.1se))

mod_incl_cv_fas = cv.glmnet(x=m, y=nhdata$incl_fas, family="binomial", intercept=F, nfolds = 5)
coef(mod_incl_cv_fas, c(mod_incl_cv_fas$lambda.1se))




8192 8193 12648 17054
)





