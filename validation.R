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
