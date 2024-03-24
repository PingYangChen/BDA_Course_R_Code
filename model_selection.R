#
library(ggplot2)
library(GGally)
library(regclass)
#
library(glmnet)
library(gglasso)
library(sparsegl)
#
library(pls)
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
# Examine for multicollinearity
mdl <- lm(Balance ~ Age + Limit + Rating, 
          data = credit_data)
VIF(mdl)
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

summary(mf)
summary(mb)
summary(m2)

# Regularization
names(credit_data)
# Create model matrix
modelmat <- model.matrix(Balance ~ ., data = credit_data)[,-1] # Remove intercept part
colnames(modelmat)
y <- credit_data$Balance
head(modelmat, 6)
# Ridge
rid_cv <- cv.glmnet(modelmat, y, family = "gaussian", alpha = 0)#,
                    #lambda = exp(seq(-3, 1, length = 100)))
names(rid_cv)
plot(rid_cv$lambda, rid_cv$cvm, type = "l")
rid_cv$lambda.min
rid_cv$lambda.1se
m_rid <- glmnet(modelmat, y, family = "gaussian", alpha = 0,
                lambda = rid_cv$lambda.1se)
m_rid$a0
m_rid$beta
# Lasso
las_cv <- cv.glmnet(modelmat, y, family = "gaussian", alpha = 1)
plot(las_cv$lambda, las_cv$cvm, type = "l")
las_cv$lambda.min
las_cv$lambda.1se
m_las <- glmnet(modelmat, y, family = "gaussian", alpha = 1,
                lambda = las_cv$lambda.1se)
m_las$a0
m_las$beta
# Elastic net
en_cv <- cv.glmnet(modelmat, y, family = "gaussian", alpha = 0.3)
plot(en_cv$lambda, en_cv$cvm, type = "l")
en_cv$lambda.min
en_cv$lambda.1se
m_en <- glmnet(modelmat, y, family = "gaussian", alpha = 0.3,
                lambda = en_cv$lambda.1se)
m_en$a0
m_en$beta

# Group lasso application to modeling 2fi
names(credit_data)
modelmat2fi <- model.matrix(
  Balance ~ . + Income * (Age + Education + Own + Student), data = credit_data
)[,-1]
colnames(modelmat2fi)
twofi_group <- c(1, 2, 3, 4, 1, 
                 1, 1, 1, 5, 6, 
                 6, 1, 1, 1, 1)
names(twofi_group) <- colnames(modelmat2fi)
twofi_group
# Group Lasso: choose best lambda
# library(gglasso)
glas_cv <- cv.gglasso(modelmat2fi, y, group = twofi_group, loss = "ls")
glas_cv$lambda.min
glas_cv$lambda.1se
m_glas <- gglasso(modelmat2fi, y, group = twofi_group, loss = "ls",
                  lambda = glas_cv$lambda.1se)
m_glas$b0
m_glas$beta

# Sparse group lasso  application to modeling 2fi
# Sparse Group Lasso: choose best lambda using CV
#library(sparsegl)
sglas_cv <- cv.sparsegl(modelmat2fi, y, group = twofi_group, family = "gaussian",
                        asparse = 0.05) # set weight of L1-penalty as 0.05
sglas_cv$lambda.min
sglas_cv$lambda.1se
m_sglas <- sparsegl(modelmat2fi, y, group = twofi_group, family = "gaussian",
                    lambda = sglas_cv$lambda.1se, asparse = 0.05)
m_sglas$b0
m_sglas$beta



# Dimension reduction
names(credit_data)
# Create model matrix
modelmat <- model.matrix(Balance ~ ., data = credit_data)[,-1] # Remove intercept part
colnames(modelmat)
#
pca <- prcomp(modelmat, scale. = TRUE)
pvars <- (pca$sdev)^2 
pvars_props <- pvars/sum(pvars)
pvars_props_cum <- cumsum(pvars_props)
#
par(mfrow = c(1, 2), mar = c(5, 4, 4, 1))
plot(1:length(pca$sdev), (pca$sdev)^2, type = "l", lwd = 3, 
     xlab = "# of PCs", ylab = "Variance", col = "#555555",
     axes = FALSE)
axis(side = 1, at = 1:length(pca$sdev))
axis(side = 2, at = pretty(range((pca$sdev)^2)))
points(1:length(pca$sdev), (pca$sdev)^2, pch = 16, cex = 1.2)
abline(h = 1, col = "black", lty = 2, lwd = 1)
#
plot(1:length(pca$sdev), pvars_props_cum, type = "h", 
     lend = 3, lwd = 20, col = "dodgerblue2",
     xlab = "# of PCs", ylab = "% variance explained", axes = FALSE)
axis(side = 1, at = 1:length(pca$sdev))
axis(side = 2, at = pretty(0:1))
abline(h = 0.9, col = "navy", lty = 2, lwd = 2)
#
n_pcs <- min(which(pvars_props_cum >= 0.9))
used_pcs <- pca$x[,1:n_pcs]
colnames(used_pcs) <- paste0("PC", 1:n_pcs)
credit_pc_data <- data.frame(
  Balance = credit_data$Balance, used_pcs
)
summary(lm(Balance ~ ., data = credit_pc_data))
#
fit_pcr <- lm(Balance ~ ., data = credit_pc_data)
y_pcr_fit <- predict(fit_pcr, credit_pc_data)
mse_pcr <- mean((y - y_pcr_fit)^2)
print(mse_pcr)

# Partial Least Squares
library(pls)
m_pls <- plsr(Balance ~ ., data = credit_data, 
              scale = TRUE, validation = "CV")
summary(m_pls)


# Sure independence screening
library(SIS)
modelmat <- model.matrix(Balance ~ ., data = credit_data)[,-1] # Remove intercept part
y <- credit_data$Balance
m_sis <- SIS(modelmat, y, family = "gaussian", penalty = "SCAD",
             tune = "cv", seed = 1) #, varISIS = "vanilla"
#
colnames(modelmat)[m_sis$ix]
modelmat_sis <- modelmat[,m_sis$ix]
credit_data_sis <- data.frame(cbind(modelmat_sis, Balance = y))
ggpairs(credit_data_sis)

summary(lm(Balance ~ ., data = credit_data_sis))


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
               tune = "bic", seed = 1) #, varISIS = "vanilla"
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

leukemia_sis <- data.frame(cbind(lkm_x_sis, y = lkm_y))
summary(glm(y ~ ., data = leukemia_sis, family = binomial(link = "logit")))

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



