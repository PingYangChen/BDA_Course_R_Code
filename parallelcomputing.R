#
library(ggplot2)
library(GGally)
#
library(glmnet)
library(pracma)
#
library(foreach)
library(doParallel)


# Tracking computing time

# 輸出矩陣每列的最大值
n <- 1e7
p <- 10
A <- matrix(runif(n*p, 0, 1), n, p)

t1 <- system.time({
  res1 <- numeric(n)
  for (i in 1:n) {
    res1[i] <- max(A[i,])
  }
})[3]
print(t1)

t2 <- system.time({
  res2 <- sapply(seq(n), 
    function(i) max(A[i,])
  )
})[3]
print(t2)

library(matrixStats)
t3 <- system.time({
  res3 <- rowOrderStats(
    A, which = p)
})[3]
print(t3)

t4 <- system.time({
  res4 <- A[cbind(1:n, max.col(A))]
})[3]
print(t4)








n_tasks <- 10000


cl = makeCluster(4)    # initialize parallel cluster
registerDoParallel(cl) # sets up the cluster for the foreach loop
cputime_par <- system.time({
  
  result <- foreach (i = 1:10000, .combine = 'c', .packages = NULL) %dopar% { 
    sqrt(i) # codes of parallelized task
  }

})[3] # seconds
stopCluster(cl) # shutdown parallel cluster

print(cputime_par)

cputime_ser <- system.time({
serial_result <- numeric(10000)
for (i in 1:10000) { 
  serial_result[i] <- sqrt(i) 
}
})[3]

print(cputime_ser)




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

# tuning sets of alpha and lambda
alpha_set <- c(0.3, 0.5, 0.7)
lambda_set <- exp(seq(-3, 1, length = 100))
#
param_cand <- cbind(
  rep(alpha_set, time = length(lambda_set)),
  rep(lambda_set, each = length(alpha_set))
)
# Implement 10-fold CV
n <- length(y)
nfold <- 5 # set number of folds
# Make folds by yourself

# Always remember to set seed before actions with randomness involved!!
seed <- 1
set.seed(seed)
# Create belonging folds for each data point
folds <- pracma::randperm( # randomly permute the index of folds
  rep(1:nfold, time = ceiling(n/nfold))[1:n]
)
mse_cv_tune <- numeric(nrow(param_cand))

cv_time <- system.time({
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
})[3]

print(cv_time)

min(mse_cv_tune)
best_param <- param_cand[which.min(mse_cv_tune),]
sprintf("Best alpha = %.1f, lambda = %f", 
        best_param[1], best_param[2])




# 開啟平行核心數
cl = makeCluster(4)
registerDoParallel(cl)
cv_time_par <- system.time({
  xp = foreach (i = 1:nrow(param_cand), .combine = 'c', .packages = "glmnet") %dopar% {
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
    mse_cv
  }
})[3]
# 關閉平行核心數
stopCluster(cl)


print(cv_time_par)
