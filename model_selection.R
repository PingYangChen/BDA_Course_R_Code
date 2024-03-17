#
library(ggplot2)
library(GGally)
#
library(glmnet)
library(gglasso)
#devtools::install_github("DataSlingers/ExclusiveLasso")
#library(ExclusiveLasso)
#
library(SIS)
#
datapath <- "James_2023_CSV"
# Read credit dataset
credit_data <- read.csv(file.path(datapath, "Credit.csv"))
names(credit_data)
credit_data$Own <- as.factor(credit_data$Own)
credit_data$Student <- as.factor(credit_data$Student)
credit_data$Married <- as.factor(credit_data$Married)
credit_data$Region <- as.factor(credit_data$Region)
#
head(credit_data, 4)
# Stepwise regression
lm_null <- lm(Balance ~ 1, data = credit_data)
lm_full <- lm(Balance ~ ., data = credit_data)
# use AIC
# aic_param <- 2
# mf <- step(lm_null, scope = list(lower = lm_null, upper = lm_full), 
#            direction = "forward", k = aic_param)
# mb <- step(lm_full, scope = list(lower = lm_null, upper = lm_full), 
#            direction = "backward", k = aic_param)
# m2 <- step(lm_null, scope = list(lower = lm_null, upper = lm_full), 
#            direction = "both", k = aic_param)
# use BIC
bic_param <- log(nrow(credit_data)) # log(n)
# forward method
mf <- step(lm_null, scope = list(lower = lm_null, upper = lm_full), 
           direction = "forward", k = bic_param)
# backward method
mb <- step(lm_full, scope = list(lower = lm_null, upper = lm_full), 
           direction = "backward", k = bic_param)
# both-way method
m2 <- step(lm_null, scope = list(lower = lm_null, upper = lm_full), 
           direction = "both", k = bic_param)

# Regularization
modelmat <- model.matrix(Balance ~ ., data = credit_data)[,-1]
y <- credit_data$Balance
# Ridge
rid_cv <- cv.glmnet(modelmat, y, family = "gaussian", alpha = 0)
names(rid_cv)
rid_cv$lambda.1se
m_rid <- glmnet(modelmat, y, family = "gaussian", alpha = 0,
                lambda = rid_cv$lambda.1se)
m_rid$a0
m_rid$beta
# Lasso
las_cv <- cv.glmnet(modelmat, y, family = "gaussian", alpha = 1)
las_cv$lambda.min
las_cv$lambda.1se
m_las <- glmnet(modelmat, y, family = "gaussian", alpha = 1,
                lambda = las_cv$lambda.1se)
m_las$a0
m_las$beta
# Elastic net
en_cv <- cv.glmnet(modelmat, y, family = "gaussian", alpha = 0.5)
en_cv$lambda.min
en_cv$lambda.1se
m_en <- glmnet(modelmat, y, family = "gaussian", alpha = 0.5,
                lambda = en_cv$lambda.1se)
m_en$a0
m_en$beta
# Group lasso
# library(gglasso)
colnames(modelmat)
xgroup <- c(1, 1, 1, 1, 2, 2, 3, 2, 2, 3, 3)
glas_cv <- cv.gglasso(modelmat, y, group = xgroup, loss = "ls")
glas_cv$lambda.min
glas_cv$lambda.1se
m_glas <- gglasso(modelmat, y, group = xgroup, loss = "ls",
                  lambda = glas_cv$lambda.1se)
m_glas$b0
m_glas$beta

# Group lasso application to modeling 2fi
# library(gglasso)
modelmat2fi <- model.matrix(Balance ~ (.)^2, data = credit_data)[,-1]
colnames(modelmat2fi)
twofi_group <- numeric(ncol(modelmat2fi))
for (i in 1:ncol(modelmat2fi)) {
  colnames(modelmat2fi)[i]
  for (j in 1:10) {
    
  }
  names(credit_data)[i]
}
twofi_group <- c(1, 1, 1, 1, 2, 2, 3, 2, 2, 3, 3)
glas_cv <- cv.gglasso(modelmat2fi, y, group = twofi_group, loss = "ls")
glas_cv$lambda.min
glas_cv$lambda.1se
m_glas <- gglasso(modelmat2fi, y, group = twofi_group, loss = "ls",
                  lambda = glas_cv$lambda.1se)
m_glas$b0
m_glas$beta

# Exclusive lasso
#library(ExclusiveLasso)
# colnames(modelmat)
# xgroup <- c(1, 1, 1, 1, 2, 2, 3, 2, 2, 3, 3)
# elas_cv <- cv.exclusive_lasso(modelmat, y, group = xgroup, family = "gaussian")
# elas_cv$lambda.min
# elas_cv$lambda.1se
# m_elas <- exclusive_lasso(modelmat, y, group = xgroup, family = "gaussian",
#                           lambda = elas_cv$lambda.1se)
# m_elas$intercept
# m_elas$coef


# Dimension reduction


# Sure independence screening
library(SIS)
data(leukemia.train)
# response vatiable: V7130
dim(leukemia.train)
# [1]   38 7130
# WRONG: use glm to fit the ultrahigh dimensional leukemia data
glm_lkm <- glm(V7130 ~ ., data = leukemia.train,
               family = binomial(link = "logit"))
# summary(glm_lkm)
dim(summary(glm_lkm)$coefficients)
# [1] 38  4
head(summary(glm_lkm)$coefficients, 6)
# SIS method
lkm_x <- model.matrix(V7130 ~ ., data = leukemia.train)[,-1]
lkm_y <- leukemia.train[,7130]
sis_lkm <- SIS(lkm_x, lkm_y, family = "binomial", penalty = "lasso",
               tune = "bic") #, varISIS = "vanilla"
# SIS selected
colnames(lkm_x)[sis_lkm$ix]
# [1] "V3320" "V3810"
lkm_x_sis <- lkm_x[,sis_lkm$ix]

ggpairs(data.frame(cbind(lkm_x_sis, y = lkm_y)),
        aes(colour = as.factor(lkm_y), alpha = 0.4))

glm_lkm_s <- glmnet(x = lkm_x_sis, y = lkm_y,
                    family = "binomial", alpha = 0, lambda = 0)
glm_lkm_s$a0
glm_lkm_s$beta

# glm_lkm_cv <- cv.glmnet(x = lkm_x_sis, y = lkm_y,
#                         family = "binomial", alpha = 0)
# glm_lkm_cv$lambda.1se
# glm_lkm_s <- glmnet(x = lkm_x_sis, y = lkm_y,
#                     family = "binomial", alpha = 0, 
#                     lambda = rid_cv$lambda.1se)
# glm_lkm_s$a0
# glm_lkm_s$beta

# data(prostate.train)
# dim(prostate.train)
# names(prostate.train)



