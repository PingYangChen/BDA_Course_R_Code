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
datapath <- "James_2023_CSV"
# Read credit dataset
auto_data <- read.csv(file.path(datapath, "auto.csv"))
clean_auto_data <- auto_data[which(auto_data$horsepower != "?"),]

names(auto_data)
clean_auto_data$horsepower <- as.numeric(clean_auto_data$horsepower)
lm(mpg ~ cylinders + horsepower, data = clean_auto_data)




library(RCurl)
credit_data <- read.csv(text = RCurl::getURL(
  "https://raw.githubusercontent.com/PingYangChen/BDA_Course_R_Code/main/James_2023_CSV/Credit.csv"
))
print(names(credit_data))
print(nrow(credit_data))
#
credit_data$Own <- as.factor(credit_data$Own)
credit_data$Student <- as.factor(credit_data$Student)
credit_data$Married <- as.factor(credit_data$Married)
credit_data$Region <- as.factor(credit_data$Region)

# For convenience, our practicing examples today are all based on glmnet 
# Create model matrix and response vector
modelmat <- model.matrix(Balance ~ ., data = credit_data)[,-1]
y <- credit_data$Balance
# fit ordinary linear regression model
lm_fit <- glmnet(modelmat, y, family = "gaussian", alpha = 0, lambda = 0)
y_fit <- predict(lm_fit, modelmat)
# in fact, it is similar to the result obtained by 
# lm_fit <- lm(Balance ~ ., data = credit_data)
# y_fit <- lm_fit$fitted.values
# Compute MSE
mse_fit <- mean((y - y_fit)^2)
print(mse_fit)


#install.packages(c("RCurl", "pracma", "glmnet"))



# Implement 10-fold CV
n <- length(y)
nfold <- 10 # set number of folds
# Make folds by yourself
library(pracma)
# Always remember to set seed before actions with randomness involved!!
seed <- 1
set.seed(seed)
# Create belonging folds for each data point
folds <- pracma::randperm( # randomly permute the index of folds
  rep(1:nfold, time = ceiling(n/nfold))[1:n]
)
# See the distribution of 
# testing data sizes in each fold
table(folds)

# Create space for storing predicted values of 10-fold CV 
y_cv <- numeric(n)
# Start 10-fold CV
for (k in 1:nfold) {
  # Get data id for training and testing
  train_id <- which(folds != k)
  test_id <- which(folds == k)
  # Get training data (X and y)
  train_x <- modelmat[train_id,]
  train_y <- y[train_id]
  # Get testing data (X)
  test_x <- modelmat[test_id,]
  test_y <- y[test_id]
  # Fit OLS model with training data
  lm_fold <- glmnet(train_x, train_y, family = "gaussian", alpha = 0, lambda = 0)
  # Get prediction of the testing data
  yhat_fold <- predict(lm_fold, test_x)
  # Allocate those predicted values into the corresponding fold
  y_cv[test_id] <- yhat_fold
}
# Compute MSE of the cross-validation
mse_cv <- mean((y - y_cv)^2)
print(mse_cv)


#
# fit elastic-net  regression model
en_fit <- glmnet(modelmat, y, family = "gaussian", alpha = 0.5, lambda = 5)
y_en_fit <- predict(en_fit, modelmat)
# Compute MSE of the fitted values
mse_en <- mean((y - y_en_fit)^2)
print(mse_en)
# [1] 9471.651



# tuning sets of alpha and lambda
alpha_set <- c(0.3, 0.5, 0.7)
lambda_set <- exp(seq(-3, 1, length = 50))
#
param_cand <- cbind(
  rep(alpha_set, time = length(lambda_set)),
  rep(lambda_set, each = length(alpha_set))
)
# Implement 10-fold CV
n <- length(y)
nfold <- 5 # set number of folds
# Make folds by yourself
library(pracma)
# Always remember to set seed before actions with randomness involved!!
seed <- 1
set.seed(seed)
# Create belonging folds for each data point
folds <- pracma::randperm( # randomly permute the index of folds
  rep(1:nfold, time = ceiling(n/nfold))[1:n]
)
#
mse_cv_tune <- numeric(nrow(param_cand))
for (ipar in 1:nrow(param_cand)) {
  # Create space for storing predicted values of 10-fold CV 
  y_cv <- numeric(n)
  # Start 10-fold CV
  for (k in 1:nfold) {
    # Get data id for training and testing
    train_id <- which(folds != k)
    test_id <- which(folds == k)
    # Get training data (X and y)
    train_x <- modelmat[train_id,]
    train_y <- y[train_id]
    # Get testing data (X only)
    test_x <- modelmat[test_id,]
    # Fit OLS model with training data
    en_fold <- glmnet(train_x, train_y, family = "gaussian", 
                      alpha = param_cand[ipar,1], lambda = param_cand[ipar,2])
    # Get prediction of the testing data
    yhat_fold <- predict(en_fold, test_x)
    # Allocate those predicted values into the corresponding fold
    y_cv[test_id] <- yhat_fold
  }
  # Compute MSE of the cross-validation
  mse_cv <- mean((y - y_cv)^2)
  #
  mse_cv_tune[ipar] <- mse_cv
}


min(mse_cv_tune)
best_param <- param_cand[which.min(mse_cv_tune),]
sprintf("Best alpha = %.1f, lambda = %f", 
        best_param[1], best_param[2])

lcolors <- c("dodgerblue2", "darkorange1", "forestgreen")
plot(0, col = 'white', xlim = range(lambda_set), ylim = range(mse_cv_tune),
     xlab = "lambda", ylab = "MSE")
for (ia in 1:3) {
  loc <- which(param_cand[,1] == alpha_set[ia])
  points(lambda_set, mse_cv_tune[loc], type = "l", col = lcolors[ia], lwd = 3)
}
legend("topleft", sprintf("alpha = %.1f", alpha_set), 
       col = lcolors, lty = 1, lwd = 3)






# Create model matrix and response vector
modelmat <- model.matrix(Balance ~ ., data = credit_data)[,-1]
y <- credit_data$Balance
# Compute principal components
pca <- prcomp(modelmat, scale. = TRUE) # scale the data due to inconsistent unit
pvars <- (pca$sdev)^2 # compute variances
pvars_props_cum <- cumsum(pvars/sum(pvars)) # compute cumulative variances
# Use the PCs with cumulated variance exceeding 90%
n_pcs <- min(which(pvars_props_cum >= 0.9))
modelmat_pc <- pca$x[,1:n_pcs]
colnames(modelmat_pc) <- paste0("PC", 1:n_pcs)
# Fit OLS model for the transformed data
pcr_fit <- glmnet(modelmat_pc, y, family = "gaussian", alpha = 0, lambda = 0)
# Compute MSE
y_pcr_fit <- predict(pcr_fit, modelmat_pc)
mse_pcr <- mean((y - y_pcr_fit)^2)
print(mse_pcr)



#?prcomp # read document
# Compute principal components
pca <- prcomp(modelmat, scale. = TRUE) # scale the data due to inconsistent unit
# The result of principal components is
pca$x
# Manually compute the principal components 
# First, create matrices for standardizing the original data, ‘modelmat’
center_mat <- matrix(
  pca$center, nrow = nrow(modelmat), ncol = length(pca$center), byrow = TRUE)  
scale_mat <- matrix(
  pca$scale, nrow = nrow(modelmat), ncol = length(pca$scale), byrow = TRUE) 
# Second, standardize the original data, 
modelmat_std <- (modelmat - center_mat)/scale_mat
# Third, rotate the standardized data to the principal component space
modelmat_pcs <- modelmat_std %*% pca$rotation
# See is the result identical to pca$x
all((modelmat_pcs - pca$x) == 0)


# Implement 10-fold CV
n <- length(y)
nfold <- 10 # set number of folds
# Make folds by yourself
library(pracma)
# Always remember to set seed before actions with randomness involved!!
seed <- 1
set.seed(seed)
# Create belonging folds for each data point
folds <- pracma::randperm( # randomly permute the index of folds
  rep(1:nfold, time = ceiling(n/nfold))[1:n]
)
# Create space for storing predicted values of 10-fold CV 
y_cv <- numeric(n)
# Start 10-fold CV
for (k in 1:nfold) {
  # Get data id for training and testing
  train_id <- which(folds != k)
  test_id <- which(folds == k)
  # Get training data (X and y)
  train_x <- modelmat[train_id,]
  train_y <- y[train_id]
  # Get testing data (X)
  test_x <- modelmat[test_id,]
  # Compute principal components
  pca_cv <- prcomp(train_x, scale. = TRUE) # scale the data due to inconsistent unit
  pvars_cv <- (pca_cv$sdev)^2 # compute variances
  pvars_props_cum_cv <- cumsum(pvars_cv/sum(pvars_cv)) # compute cumulative variances
  # Use the PCs with cumulated variance exceeding 90%
  n_pcs_cv <- min(which(pvars_props_cum_cv >= 0.9))
  # Create training data based on PCs
  train_x_pc <- pca$x[,1:n_pcs_cv]
  colnames(train_x_pc) <- paste0("PC", 1:n_pcs_cv)
  # Fit OLS model for the transformed data
  pcr_fold <- glmnet(train_x_pc, y, family = "gaussian", alpha = 0, lambda = 0)
  #
  center_mat <- matrix(pca$center, nrow = nrow(test_x), ncol = length(pca$center), byrow = TRUE)
  scale_mat <- matrix(pca$scale, nrow = nrow(test_x), ncol = length(pca$center), byrow = TRUE)
  test_x_pc <- (((test_x - center_mat)/scale_mat) %*% pca_cv$rotation)[,1:n_pcs_cv]
  # Get prediction of the testing data
  yhat_fold <- predict(pcr_fold, test_x_pc)
  # Allocate those predicted values into the corresponding fold
  y_cv[test_id] <- yhat_fold
}
# Compute MSE of the cross-validation
mse_cv <- mean((y - y_cv)^2)
print(mse_cv)







default_data <- read.csv(text = RCurl::getURL(
  "https://raw.githubusercontent.com/PingYangChen/BDA_Course_R_Code/main/James_2023_CSV/Default.csv"
))
print(names(default_data))
print(dim(default_data))

default_data$default <- as.factor(default_data$default)
default_data$student <- as.factor(default_data$student)



confusion <- function(true_y, pred_y) {
  unique_y <- unique(true_y)
  ncateg <- length(unique_y)
  confmat <- matrix(0, ncateg, ncateg)
  for (i in 1:ncateg) {
    loc <- which(true_y == unique_y[i])
    pred_count <- table(c(pred_y[loc], unique_y)) - 1
    confmat[i,] <- pred_count
  }
  dimnames(confmat) <- list(unique_y, unique_y)
  return(confmat)
}

# For convenience, our practicing examples today are all based on glmnet 
# Create model matrix and response vector
modelmat <- model.matrix(default ~ ., data = default_data)[,-1]
y <- as.numeric(default_data$default) - 1
# fit logistic regression model
lm_fit <- glmnet(modelmat, y, family = "binomial", alpha = 0, lambda = 0)
y_fit <- (predict(lm_fit, modelmat, type = "response") > 0.5)
# Compute Accuracy using confusion matrix
confmat <- confusion(y, y_fit)
print(confmat)
acc <- 100*sum(diag(confmat))/sum(confmat)
print(acc)
tpr <- 100*confmat[2,2]/sum(confmat[2,])
print(tpr)

# Implement 10-fold CV
n <- length(y)
nfold <- 10 # set number of folds
# Make folds by yourself
library(pracma)
# Always remember to set seed before actions with randomness involved!!
seed <- 1
print(seed)
set.seed(seed)
# Create belonging folds for each data point
folds <- pracma::randperm( # randomly permute the index of folds
  rep(1:nfold, time = ceiling(n/nfold))[1:n]
)
# Create space for storing predicted values of 10-fold CV 
y_cv <- numeric(n)
# Start 10-fold CV
for (k in 1:nfold) {
  # Get data id for training and testing
  train_id <- which(folds != k)
  test_id <- which(folds == k)
  # Get training data (X and y)
  train_x <- modelmat[train_id,]
  train_y <- y[train_id]
  # Get testing data (X)
  test_x <- modelmat[test_id,]
  test_y <- y[test_id]
  # Fit OLS model with training data
  lm_fold <- glmnet(train_x, train_y, family = "binomial", alpha = 0, lambda = 0)
  # Get prediction of the testing data
  yhat_fold <- (predict(lm_fold, test_x, type = "response") > 0.5)
  # Allocate those predicted values into the corresponding fold
  y_cv[test_id] <- yhat_fold
}
# Compute Accuracy
confmat_cv <- confusion(y, y_cv)
print(confmat_cv)
acc_cv <- 100*sum(diag(confmat_cv))/sum(confmat_cv)
print(acc_cv)


tpr <- 100*confmat[2,2]/sum(confmat[2,])
tpr_cv <- 100*confmat_cv[2,2]/sum(confmat_cv[2,])

table(default_data$default)
plot(default_data$default, ylab = "Count", xlab = "default")




# For convenience, our practicing examples today are all based on glmnet 
# Create model matrix and response vector
modelmat <- model.matrix(default ~ ., data = default_data)[,-1]
y <- as.numeric(default_data$default) - 1

cutoff <- seq(0, 1, length = 101)
smat <- matrix(0, length(cutoff), 2)
f1score <- numeric(length(cutoff))
for (i in 1:length(cutoff)) {
  # fit logistic regression model
  glm_fit <- glmnet(modelmat, y, family = "binomial", 
                    alpha = 0, lambda = 0)
  y_glm <- (predict(glm_fit, modelmat, type = "response") > cutoff[i])
  # Compute Accuracy using confusion matrix
  cm_glm <- confusion(y, y_glm)
  tpr_glm <- 100*cm_glm[2,2]/sum(cm_glm[2,])
  tnr_glm <- 100 - 100*cm_glm[1,1]/sum(cm_glm[1,])
  smat[i,] <- c(tnr_glm, tpr_glm)
  f1score[i] <- 2*cm_glm[2,2]/(2*cm_glm[2,2] + cm_glm[1,2] + cm_glm[2,1])
}

plot(smat, type = "l", lwd = 2, ylim = c(-.5, 105),
     ylab = "True Positive Rate", xlab = "False Positive Rate")
for (k in c(2, 11, 26, 51, 76, 91, which.max(f1score))) {
  points(smat[k,1], smat[k,2], pch = 16)
  if (k == 2) { tpos = 3 } else { tpos = 4 }
  text(smat[k,1], smat[k,2], 
       sprintf("Cutoff = %.2f, FPR = %.2f, TPR = %.2f", 
               cutoff[k], smat[k,1], smat[k,2]),
       pos = tpos)
}

plot(cutoff, f1score, type = "l", lwd = 2, 
     xlab = "Cutoff", ylab = "F1-score")
points(cutoff[which.max(f1score)], max(f1score))



# fit logistic regression model
glm_fit <- glmnet(modelmat, y, family = "binomial", 
                  alpha = 0, lambda = 0)
y_glm <- (predict(glm_fit, modelmat, type = "response") > 0.5)
# Compute Accuracy using confusion matrix
cm_glm <- confusion(y, y_glm)
print(cm_glm)
acc_glm <- 100*sum(diag(cm_glm))/sum(cm_glm)
print(acc_glm)
tpr_glm <- 100*cm_glm[2,2]/sum(cm_glm[2,])
print(tpr_glm)

# Imbalanced Data
wt <- numeric(length(y))
wt[y == 0] <- 1 - sum(1 - y)/length(y)
wt[y == 1] <- 1 - sum(y)/length(y)
# fit logistic regression model
wglm_fit <- glmnet(modelmat, y, family = "binomial", 
                  alpha = 0, lambda = 0, weights = wt)
y_wglm <- (predict(wglm_fit, modelmat, type = "response") > 0.5)
# Compute Accuracy using confusion matrix
cm_wglm <- confusion(y, y_wglm)
print(cm_wglm)
acc_wglm <- 100*sum(diag(cm_wglm))/sum(cm_wglm)
print(acc_wglm)
tpr_wglm <- 100*cm_wglm[2,2]/sum(cm_wglm[2,])
print(tpr_wglm)



install.packages("smotefamily")
library(smotefamily)
?SMOTE

imbdata <- read.csv("James_2023_CSV/imbbinary.csv")



{
  y0Loc <- which(imbdata$Y == 0)
  y1Loc <- which(imbdata$Y == 1)
  plot(imbdata[y0Loc, 1:2], col = "dodgerblue2", pch = 20, 
       main = "Original Data",
       xlim = range(imbdata$X1), ylim = range(imbdata$X2))
  points(imbdata[y1Loc, 1:2], col = "darkorange1", pch = 15)
  legend("bottomright", c("Y = 0", "Y = 1"), col = c("dodgerblue2", "darkorange1"), pch = c(20, 15))
}

table(imbdata$Y)


{
  y0Loc <- which(imbdata$Y == 0)
  y1Loc <- which(imbdata$Y == 1)
  set.seed(1)
  upsam <- sample(y1Loc, length(y0Loc), replace = TRUE)
  upsam_data <- imbdata[upsam, 1:2] + matrix(rnorm(length(upsam)*2, 0, 0.015), length(upsam), 2) 
  plot(imbdata[y0Loc, 1:2], col = adjustcolor("dodgerblue2", alpha.f = 0.3), pch = 20, 
       main = "Up-sampled Data",
       xlim = range(imbdata$X1), ylim = range(imbdata$X2))
  points(upsam_data, col = adjustcolor("darkorange1", alpha.f = 0.3), pch = 15)
  legend("bottomright", c("Y = 0", "Y = 1"), col = c("dodgerblue2", "darkorange1"), pch = c(20, 15))
}

upsam_imbdata <- rbind(imbdata[y0Loc,], imbdata[upsam,])
table(upsam_imbdata$Y)


{
  y0Loc <- which(imbdata$Y == 0)
  y1Loc <- which(imbdata$Y == 1)
  dwsam <- sample(y0Loc, length(y1Loc), replace = FALSE)
  dwsam_data <- imbdata[dwsam, 1:2]
  plot(dwsam_data, col = adjustcolor("dodgerblue2", alpha.f = 1), pch = 16, 
       main = "Down-sampled Data",
       xlim = range(imbdata$X1), ylim = range(imbdata$X2))
  points(imbdata[y1Loc, 1:2], col = adjustcolor("darkorange1", alpha.f = 1), pch = 15)
  legend("bottomright", c("Y = 0", "Y = 1"), col = c("dodgerblue2", "darkorange1"), pch = c(16, 15))
}

dwsam_imbdata <- rbind(imbdata[dwsam,], imbdata[y1Loc,])
table(dwsam_imbdata$Y)



resam <- SMOTE(imbdata[,1:2], imbdata[,3])

{
  y0Loc <- which(resam$data$class == 0)
  y1Loc <- which(resam$data$class == 1)
  plot(resam$data[y0Loc, 1:2], col = "dodgerblue2", pch = 20, 
       main = "SMOTE Sampled Data",
       xlim = range(resam$data$X1), ylim = range(resam$data$X2))
  points(resam$data[y1Loc, 1:2], col = "darkorange1", pch = 15)
  legend("bottomright", c("Y = 0", "Y = 1"), 
         col = c("dodgerblue2", "darkorange1"), pch = c(20, 15))
}

table(resam$data$class)



imbdata_tmp <- imbdata
names(imbdata_tmp)[3] <- "class"
smote_ext <- list(
  list(name = "Original", res = list(data = imbdata_tmp)),
  list(name = "ADAS Sampled", res = ADAS(imbdata[,1:2], imbdata[,3])),
  list(name = "ANS Sampled", res = ANS(imbdata[,1:2], imbdata[,3])),
  list(name = "BLSMOTE Sampled", res = BLSMOTE(imbdata[,1:2], imbdata[,3])),
  list(name = "DBSMOTE Sampled", res = DBSMOTE(imbdata[,1:2], imbdata[,3])),
  list(name = "RSLS Sampled", res = RSLS(imbdata[,1:2], imbdata[,3])),
  list(name = "SLS Sampled", res = SLS(imbdata[,1:2], imbdata[,3])),
  list(name = "SMOTE Sampled", res = SMOTE(imbdata[,1:2], imbdata[,3]))
)


par(mfrow = c(2, 4))
for (i in 1:8) {
  metname <- smote_ext[[i]]$name
  tmp <- smote_ext[[i]]$res
  y0Loc <- which(tmp$data$class == 0)
  y1Loc <- which(tmp$data$class == 1)
  plot(tmp$data[y0Loc, 1:2], col = "dodgerblue2", pch = 20, 
       main = sprintf("%s Data", metname),
       xlim = range(tmp$data$X1), ylim = range(tmp$data$X2))
  points(tmp$data[y1Loc, 1:2], col = "darkorange1", pch = 15)
  legend("bottomright", c("Y = 0", "Y = 1"), 
         col = c("dodgerblue2", "darkorange1"), pch = c(20, 15))
}















# imb_fit <- glm(Y ~ ., data = imbdata, family = binomial())
# y_fit <- as.numeric(predict(imb_fit, imbdata, type = "response") > 0.5)
# confmat <- confusion(imbdata$Y, y_fit)
# print(confmat)
# acc <- 100*sum(diag(confmat))/sum(confmat)
# print(acc)
# tpr <- 100*confmat[2,2]/sum(confmat[2,])
# print(tpr)




# Use SMOTE for for Imbalanced Data
rs_default <- SMOTE(data.frame(modelmat), y)
rs_modelmat <- as.matrix(
  rs_default$data[,-ncol(rs_default$data)])
rs_y <- as.numeric(rs_default$data$class)
# fit logistic regression model
rs_glm_fit <- glmnet(
  rs_modelmat, rs_y, 
  family = "binomial", alpha = 0, lambda = 0)
rs_y_glm <- predict(
  rs_glm_fit, modelmat, type = "response") > 0.5
# Compute Accuracy using confusion matrix
rs_cm_glm <- confusion(y, rs_y_glm)
print(rs_cm_glm)
rs_acc <- 100*sum(diag(rs_cm_glm))/sum(rs_cm_glm)
print(rs_acc)
rs_tpr <- 100*rs_cm_glm[2,2]/sum(rs_cm_glm[2,])
print(rs_tpr)




