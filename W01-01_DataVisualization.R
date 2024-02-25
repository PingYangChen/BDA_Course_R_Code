# Install packages of example datasets
install.packages("datasauRus")
# Load packages of data process
#library(dplyr)
# Load packages of example datasets
library(datasauRus)
# Load packages of visualization
library(ggplot2)

# Descriptive Statistics and Graphs of datasauRus datasets
library(datasauRus)
unique_dataset <- unique(datasauRus::datasaurus_dozen$dataset)
datasaurus_dozen_summary <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(datasaurus_dozen_summary) <- c("dataset", "mean_x", "mean_y", "sd_x", "sd_y", "corr_x_y")
for (i in 1:length(unique_dataset)) {
  loc <- which(datasauRus::datasaurus_dozen$dataset == unique_dataset[i])
  tmp <- datasauRus::datasaurus_dozen[loc,2:3]
  sum_stats <- round(c(mean(tmp$x), mean(tmp$y), sd(tmp$x), sd(tmp$y), cor(tmp$x, tmp$y)), 3)
  datasaurus_dozen_summary[i,] <- c(unique_dataset[i], sum_stats)
}
print(datasaurus_dozen_summary)

# If using dplyr package
# datasauRus::datasaurus_dozen %>% 
#   group_by(dataset) %>% 
#     summarize(
#       mean_x    = mean(x),
#       mean_y    = mean(y),
#       std_dev_x = sd(x),
#       std_dev_y = sd(y),
#       corr_x_y  = cor(x, y)
#     )

par(mfrow = c(3, 5))
for (i in 1:length(unique_dataset)) {
  loc <- which(datasauRus::datasaurus_dozen$dataset == unique_dataset[i])
  tmp <- datasauRus::datasaurus_dozen[loc,2:3]
  plot(tmp$x, tmp$y, pch = 16, xlab = "x", ylab = "y", main = unique_dataset[i])
}
par(mfrow = c(1, 1))

# Descriptive Statistics and Graphs of Anscombe's quartet datasets
head(datasets::anscombe, n = 6)
anscombe_summary <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(anscombe_summary) <- c(
  "dataset", "mean_x", "mean_y", "sd_x", "sd_y", "corr_x_y",
  "reg_coef_0", "reg_coef_1", "r_square"
)
par(mfrow = c(2, 2))
for (i in 1:4) {
  x <- datasets::anscombe[[sprintf("x%d", i)]]
  y <- datasets::anscombe[[sprintf("y%d", i)]]
  mdl <- lm(y ~ x)
  reg_coefs <- mdl$coefficients
  rsq <- summary(mdl)$r.square
  sum_stats <- round(c(mean(x), mean(y), sd(x), sd(y), cor(x, y), reg_coefs, rsq), 3)
  anscombe_summary[i,] <- c(i, sum_stats)
  plot(x, y, pch = 16, 
       xlim = range(datasets::anscombe[,1:4]), ylim = range(datasets::anscombe[,5:8]))
  abline(a = reg_coefs[1], b = reg_coefs[2])
}
par(mfrow = c(1, 1))
print(anscombe_summary)

# Descriptive Statistics and Graphs of Simpson's Paradox datasets
library(datasauRus)
head(datasauRus::simpsons_paradox, n = 6)
unique_dataset <- unique(datasauRus::simpsons_paradox$dataset)
simpsons_paradox_summary <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(simpsons_paradox_summary) <- c("dataset", "mean_x", "mean_y", "sd_x", "sd_y", "corr_x_y")
par(mfrow = c(1, 2))
for (i in 1:length(unique_dataset)) {
  loc <- which(datasauRus::simpsons_paradox$dataset == unique_dataset[i])
  tmp <- datasauRus::simpsons_paradox[loc,2:3]
  sum_stats <- round(c(mean(tmp$x), mean(tmp$y), sd(tmp$x), sd(tmp$y), cor(tmp$x, tmp$y)), 3)
  simpsons_paradox_summary[i,] <- c(unique_dataset[i], sum_stats)
  plot(tmp$x, tmp$y, pch = 16, xlab = "x", ylab = "y", main = unique_dataset[i])
}
print(simpsons_paradox_summary)
par(mfrow = c(1, 1))

# Graphs of Histogram vs. Boxplot
library(datasauRus)
head(datasauRus::box_plots, n = 6)
box_plots_names <- colnames(datasauRus::box_plots)
par(mfrow = c(5, 2), mar = c(2, 4, 4, .5))
for (i in 1:ncol(datasauRus::box_plots)) {
  hist(datasauRus::box_plots[[box_plots_names[i]]], 
       main = box_plots_names[i], xlab = " ")
  boxplot(datasauRus::box_plots[[box_plots_names[i]]], horizontal = TRUE, 
          main = box_plots_names[i])
}
par(mfrow = c(1, 1))