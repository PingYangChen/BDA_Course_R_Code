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
names(default_data)
head(default_data, 5)

default_data$default <- as.factor(default_data$default)
default_data$student <- as.factor(default_data$student)







library(foreach)
library(doParallel)
foreach (i=1:3) %do% {
  sqrt(i)
}
system.time({})[3]
