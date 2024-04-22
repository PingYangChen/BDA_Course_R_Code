
# install.packages(c("partykit", "gbm", "xgboost", "lightgbm"))

library(ggplot2)
library(GGally)
library(plotly)
library(gg3D)

datapath <- "James_2023_CSV"
# Read hitter's salary dataset
baseball_data <- read.csv(file.path(datapath, "Hitters.csv"))
names(baseball_data)
head(baseball_data, 6)
baseball_data <- baseball_data[!is.na(baseball_data$Salary),]
baseball_data$League <- as.factor(baseball_data$League)
baseball_data$Division <- as.factor(baseball_data$Division)
baseball_data$NewLeague <- as.factor(baseball_data$NewLeague)

#ggpairs(baseball_data[,-ncol(baseball_data)], aes(colour = "firebrick", alpha = 0.4)) 

library(pracma)
library(rpart)
library(rpart.plot)
library(partykit)
reg_tree_0 <- rpart(Salary ~ Years + Hits, data = baseball_data,
                    control = rpart.control(minsplit = 20, maxdepth = 30, xval = 5, cp = 0.01, usesurrogate = 0))
reg_tree_0$cptable
reg_tree_0 <- prune(reg_tree_0, cp = 0.15)

summary(reg_tree_0)
rpart.plot(reg_tree_0)

lm_0 <- lm(Salary ~ Years * Hits, data = baseball_data)
summary(lm_0)

sum(residuals(lm_0)^2)

x_mesh <- meshgrid(
  seq(min(baseball_data$Years), max(baseball_data$Years), length = 100),
  seq(min(baseball_data$Hits), max(baseball_data$Hits), length = 100)
)

tree_pred <- lm_pred <- matrix(0, 100, 100)
for (i in 1:100) {
  test_tmp <- data.frame(Years = x_mesh$X[i,], Hits = x_mesh$Y[i,])
  tree_pred[i,] <- predict(reg_tree_0, test_tmp)
  lm_pred[i,] <- predict(lm_0, test_tmp)
}

plot3d_data <- data.frame(
  Years = (as.vector(x_mesh$X)), 
  Hits = (as.vector(x_mesh$Y)),
  z_tree = as.vector(tree_pred), z_lm = as.vector(lm_pred)
)

ggplot(plot3d_data) +
  geom_tile(aes(Years, Hits, fill = z_tree)) +
  scale_fill_distiller(palette = "Spectral") 


ggplot(plot3d_data) +
  geom_tile(aes(Years, Hits, fill = z_lm)) +
  scale_fill_distiller(palette = "Spectral") 
 
### ===================================================================== ###

reg_tree <- rpart(Salary ~ . - NewLeague, data = baseball_data,
                  control = rpart.control(minsplit = 20, maxdepth = 30, 
                                          xval = 5, cp = 0.01, 
                                          usesurrogate = 1))
names(reg_tree)
summary(reg_tree)
rpart.plot(reg_tree)

reg_tree$splits


reg_tree$cptable
plotcp(reg_tree)

optim_cp <- reg_tree$cptable[which.min(reg_tree$cptable[,4]),1]
reg_tree_opt <- prune(reg_tree, cp = optim_cp)
reg_tree_opt
summary(reg_tree_opt)

rpart.plot(reg_tree_opt)

reg_tree_vi <- reg_tree_opt$variable.importance

par(mar = c(5, 4, 4, 2))
plot(0, pch = -1, xlim = c(0, max(reg_tree_vi)), ylim = c(0, length(reg_tree_vi)), 
     axes = FALSE, ylab = "", xlab = "Variable Importance")
axis(1)
vorder <- order(reg_tree_vi)
for (i in 1:length(reg_tree_vi)) {
  loc <- vorder[i]
  arrows(0, i - .5, reg_tree_vi[loc], i - .5, lwd = 12, 
         lend = 2, code = 0, col = "firebrick")
  text(0, i - .5, names(reg_tree_vi)[loc], pos = 2, xpd  = TRUE)
}

### ===================================================================== ###
# Read Heart dataset
heart_data <- read.csv(file.path(datapath, "Heart.csv"))
heart_data <- heart_data[,-1]
na_loc <- which(sapply(1:nrow(heart_data), function(i) sum(is.na(heart_data[i,])) > 0))
heart_data <- heart_data[-na_loc,]
names(heart_data)
head(heart_data, 6)
heart_data$ChestPain <- as.factor(heart_data$ChestPain)
heart_data$Thal <- as.factor(heart_data$Thal)
heart_data$AHD <- as.factor(heart_data$AHD)



#ggpairs(heart_data, aes(colour = "firebrick", alpha = 0.4)) 




cla_tree <- rpart(AHD ~ ., data = heart_data,
                  control = rpart.control(minsplit = 20, xval = 5, cp = 0.001))
names(cla_tree)
summary(cla_tree)
rpart.plot(cla_tree)

cla_tree$cptable
plotcp(cla_tree)


rpart.plot(prune(cla_tree, cp = 0.05))

optim_cp <- cla_tree$cptable[which.min(cla_tree$cptable[,4]),1]
cla_tree_opt <- prune(cla_tree, cp = optim_cp)
cla_tree_opt
rpart.plot(cla_tree_opt)


### ===================================================================== ###
# Read Heart dataset
heart_data <- read.csv(file.path(datapath, "Heart.csv"))
heart_data <- heart_data[,-1]
na_loc <- which(sapply(1:nrow(heart_data), function(i) sum(is.na(heart_data[i,])) > 0))
heart_data <- heart_data[-na_loc,]
names(heart_data)
head(heart_data, 6)
heart_data$ChestPain <- as.factor(heart_data$ChestPain)
heart_data$Thal <- as.factor(heart_data$Thal)
heart_data$AHD <- as.factor(heart_data$AHD)



library(randomForest)
# Fit random forest model
cla_rf <- randomForest(AHD ~ ., data = heart_data, ntree = 100)


cla_rf$confusion

cla_rf$

#cla_rf$forest

cla_rf$importance

heart_data_encoded <- heart_data
heart_data_encoded$AHD <- as.integer(heart_data$AHD == "Yes")
library(gbm)
cla_gbm <- gbm(AHD ~ ., data = heart_data_encoded, distribution = "bernoulli",
               n.trees = 100)
cla_gbm


cla_ada <- gbm(AHD ~ ., data = heart_data_encoded, distribution = "adaboost",
               n.trees = 100)


library(xgboost)

model_mat <- model.matrix(AHD ~ . - AHD, data = heart_data_encoded)[,-1]


system.time({

cla_xgb <- xgboost(data = model_mat, label = heart_data_encoded$AHD, 
                   nrounds = 100,
                   params = list(
                     max.depth = 1, 
                     eta = 0.01, 
                     objective = "binary:logistic")
                   )
})[3]



library(lightgbm)

library(randomForest)
# Fit random forest model
cla_rf <- randomForest(AHD ~ ., data = heart_data, ntree = 100)

library(DALEX)

exp_rf <- explain(cla_rf, data = heart_data[,-ncol(heart_data)], y = as.numeric(heart_data$AHD) - 1)

shap_1 <- predict_parts(explainer = exp_rf, 
                        new_observation = heart_data[1,], 
                        type = "shap",
                        B = 25)
plot(shap_1)




shap_2 <- predict_parts(explainer = exp_rf, 
                        new_observation = heart_data[2,], 
                        type = "shap",
                        B = 25)
plot(shap_2)

