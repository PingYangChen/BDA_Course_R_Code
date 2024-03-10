
datapath <- "James_2023_CSV"

adv_data <- read.csv(file.path(datapath, "Advertising.csv"))

head(adv_data, 5)
nrow(adv_data)
summary(adv_data[,2:5])
i=2

par(mfrow = c(1, 3), mar = c(5, 4.5, 1, 1))
for (i in 2:4) {
  plot(adv_data[,i],adv_data[,5], ylab = "Sales", xlab = names(adv_data)[i],
       pch = 20, cex.lab = 1.5, cex.axis = 1.2, col = "firebrick")
  mdl <- lm(adv_data[,5] ~ adv_data[,i])
  abline(a = mdl$coefficients[1], b = mdl$coefficients[2], 
         col = '#0000FF', lwd = 3)
}
par(mfrow = c(1, 1))

mdl <- lm(sales ~ TV, data = adv_data)
summary(mdl)
mdl$residuals

plot(adv_data[,2], adv_data[,5], ylab = "Sales", xlab = "TV",
     pch = 20, cex.lab = 1.5, cex.axis = 1.2, col = "firebrick")
abline(a = mdl$coefficients[1], b = mdl$coefficients[2], 
       col = '#0000FF', lwd = 1)
for (i in 1:nrow(adv_data)) {
  yhat <- sum(mdl$coefficients * c(1, adv_data[i,2]))
  arrows(adv_data[i,2], yhat, adv_data[i,2], adv_data[i,5], code = 0)
}


round(cor(adv_data[,2:5]), 4)

mdl_mlr <- lm(sales ~ TV + radio + newspaper, data = adv_data)
summary(mdl_mlr)

new_adv_data <- data.frame(TV = 100000, radio = 20000, newspaper = 1000)
predict(mdl_mlr, new_adv_data, interval = "prediction", level = .95)

mdl_1 <- lm(sales ~ TV, data = adv_data)
summary(mdl_1)

mdl_2 <- lm(sales ~ TV + radio, data = adv_data)
summary(mdl_2)

mdl_3 <- lm(sales ~ TV + radio + newspaper, data = adv_data)
summary(mdl_3)

mdl_2fi <- lm(sales ~ TV * radio, data = adv_data)
summary(mdl_2fi)



credit_data <- read.csv(file.path(datapath, "Credit.csv"))
names(credit_data)
head(credit_data, 5)

library(ggplot2)
library(GGally)
plot(credit_data, col = "firebrick", cex = 0.8) 

summary(lm(Balance ~ Own, data = credit_data))

summary(lm(Balance ~ Region, data = credit_data))




auto_data <- read.csv(file.path(datapath, "auto.csv"))
names(auto_data)
head(auto_data, 5)

clean_auto_data <- auto_data[which(auto_data$horsepower != "?"),]

clean_auto_data$horsepower <- as.numeric(clean_auto_data$horsepower)

mdl_auto_2 <- lm(mpg ~ horsepower + I(horsepower^2), data = clean_auto_data)


x_range <- range(clean_auto_data$horsepower)
x_grid <- seq(x_range[1], x_range[2], length = 200)
y_pred <- predict(mdl_auto_2, data.frame(horsepower = x_grid))
plot(clean_auto_data$horsepower, clean_auto_data$mpg)
points(x_grid, y_pred, type = "l")
