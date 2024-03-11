
datapath <- "James_2023_CSV"

adv_data <- read.csv(file.path(datapath, "Advertising.csv"))

head(adv_data, 5)
nrow(adv_data)
summary(adv_data[,2:5])

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

mdl_2fi$coefficients[1]
mdl_2fi$coefficients[2]*1000
mdl_2fi$coefficients[3]*1000
mdl_2fi$coefficients[4]*1000

credit_data <- read.csv(file.path(datapath, "Credit.csv"))
names(credit_data)
head(credit_data, 5)

plot(credit_data, col = "firebrick", cex = 0.8) 

library(ggplot2)
ggplot(credit_data) +
  geom_boxplot( aes(
    x = Own,
    y = Balance
  )) +
  labs(y = "Balance")
ggplot(credit_data) +
  geom_boxplot( aes(
    x = Region,
    y = Balance
  )) +
  labs(y = "Balance")


summary(lm(Balance ~ Own, data = credit_data))

summary(lm(Balance ~ Region, data = credit_data))

mdl_qq <- lm(Balance ~ Income + Student, data = credit_data)
mdl_2fiqq <- lm(Balance ~ Income * Student, data = credit_data)

s_level <- c("Yes", "No")
s_color <- c("dodgerblue2", "darkorange1")
qq_a <- c(mdl_2fiqq$coefficients[1] + mdl_2fiqq$coefficients[3], mdl_2fiqq$coefficients[1])
qq_b <- c(mdl_2fiqq$coefficients[2], mdl_2fiqq$coefficients[2])
fiqq_a <- c(mdl_2fiqq$coefficients[1] + mdl_2fiqq$coefficients[3], mdl_2fiqq$coefficients[1])
fiqq_b <- c(mdl_2fiqq$coefficients[2] + mdl_2fiqq$coefficients[4], mdl_2fiqq$coefficients[2])
par(mfrow = c(1, 2))
for (im in 1:2) {
  plot(credit_data$Income, credit_data$Balance, col = "#FFFFFF",
       xlab = "Income", ylab = "Balance")
  for (i in 1:2) {
    loc <- which(credit_data$Student == s_level[i])
    points(credit_data$Income[loc], credit_data$Balance[loc], 
           pch = 20, col = adjustcolor(s_color[i], alpha.f = .5))
    if (im == 1) {
      abline(a = qq_a[i], b = qq_b[i], col = s_color[i], lwd = 3)
    } else {
      abline(a = fiqq_a[i], b = fiqq_b[i], col = s_color[i], lwd = 3)
    }
  }
  legend("topleft", c("student", "not student"), 
         col = s_color, lwd = 3, lty = 1)
}
par(mfrow = c(1, 1))

auto_data <- read.csv(file.path(datapath, "auto.csv"))
names(auto_data)
head(auto_data, 5)

clean_auto_data <- auto_data[which(auto_data$horsepower != "?"),]

clean_auto_data$horsepower <- as.numeric(clean_auto_data$horsepower)

plot(clean_auto_data$horsepower, clean_auto_data$mpg, 
     pch = 20, xlab = "horsepower", ylab = "mpg")

mdl_auto_1 <- lm(mpg ~ horsepower, data = clean_auto_data)
plot(clean_auto_data$horsepower, clean_auto_data$mpg, 
     pch = 20, xlab = "horsepower", ylab = "mpg")
abline(a = mdl_auto_1$coefficients[1], b = mdl_auto_1$coefficients[2], col = "dodgerblue2", lwd = 3)
plot(mdl_auto_1, 1)
summary(mdl_auto_1)

mdl_auto_2 <- lm(mpg ~ horsepower + I(horsepower^2), data = clean_auto_data)
summary(mdl_auto_2)
plot(mdl_auto_2, 1)
mdl_auto_5 <- lm(mpg ~ poly(horsepower, 5, raw=T), data = clean_auto_data)
summary(mdl_auto_5)

x_range <- range(clean_auto_data$horsepower)
x_grid <- seq(x_range[1], x_range[2], length = 200)
y_pred_1 <- predict(mdl_auto_1, data.frame(horsepower = x_grid))
y_pred_2 <- predict(mdl_auto_2, data.frame(horsepower = x_grid))
y_pred_5 <- predict(mdl_auto_5, data.frame(horsepower = x_grid))
plot(clean_auto_data$horsepower, clean_auto_data$mpg, 
     pch = 20, xlab = "horsepower", ylab = "mpg", col = '#666666')
points(x_grid, y_pred_1, type = "l", col = "dodgerblue2", lwd = 3)
points(x_grid, y_pred_2, type = "l", col = "darkorange1", lwd = 3)
points(x_grid, y_pred_5, type = "l", col = "forestgreen", lwd = 3)
legend("topright", c("Linear", "Degree 2", "Degree 5"), 
       col = c("dodgerblue2", "darkorange1", "forestgreen"), 
       lty = 1, lwd = 3)




default_data <- read.csv(file.path(datapath, "Default.csv"))
names(default_data)
head(default_data, 5)




library(ggplot2)
library(gridExtra)
p1 <- ggplot(default_data) +
  geom_point(aes(x = balance, y = income, color = default), alpha = 0.5) +
  labs(x = "balance", y = "income")
p2 <- ggplot(default_data) +
  geom_boxplot( aes(x = default, y = balance, fill = default)) +
  labs(y = "balance")
p3 <- ggplot(default_data) +
  geom_boxplot( aes(x = default, y = income, fill = default)) +
  labs(y = "income")
grid.arrange(p1, p2, p3, nrow = 1, widths = c(2, 1, 1))

default_num <- as.numeric(default_data$default == "Yes")


mdl_def_lm <- lm(default_num ~ default_data$balance)

mdl_def <- glm(factor(default) ~ balance, data = default_data,
               family = binomial(link = "logit"))
summary(mdl_def)

par(mfrow = c(1, 2))
plot(default_data$balance, default_num, col = "orange", pch = 16, 
     xlab = "balance", ylab = "Pr(default)", cex.lab = 1.5)
abline(h = c(0, 1), col = "#CCCCCC", lty = 3)
abline(mdl_def_lm, col = "dodgerblue2", lwd = 3)

x_range <- range(default_data$balance)
x_grid <- seq(x_range[1], x_range[2], length = 200)
y_pred <- predict(mdl_def, data.frame(balance = x_grid), type = "link")
logit_y_pred <- exp(y_pred)/(1 + exp(y_pred))
plot(default_data$balance, default_num, col = "orange", pch = 16, 
     xlab = "balance", ylab = "Pr(default)", cex.lab = 1.5)
abline(h = c(0, 1), col = "#CCCCCC", lty = 3)
points(x_grid, logit_y_pred, col = "dodgerblue2", type = "l", lwd = 3)
par(mfrow = c(1, 1))




mdl_def_stu <- glm(factor(default) ~ factor(student), data = default_data,
               family = binomial(link = "logit"))
summary(mdl_def_stu)



mdl_def_mul <- glm(factor(default) ~ balance + income + factor(student), 
                   data = default_data, family = binomial(link = "logit"))
summary(mdl_def_mul)

mdl_def_2 <- glm(factor(default) ~ balance + factor(student), 
                 data = default_data, family = binomial(link = "logit"))
summary(mdl_def_2)

par(mfrow = c(1, 2))

x_range <- range(default_data$balance)
x_grid <- seq(x_range[1], x_range[2], length = 200)
y_pred_0 <- predict(
  mdl_def_2, data.frame(
    balance = x_grid, student = rep("No", 200)
  ), type = "link")
logit_y_pred_0 <- exp(y_pred_0)/(1 + exp(y_pred_0))
y_pred_1 <- predict(
  mdl_def_2, data.frame(
    balance = x_grid, student = rep("Yes", 200)
  ), type = "link")
logit_y_pred_1 <- exp(y_pred_1)/(1 + exp(y_pred_1))
plot(x_grid, logit_y_pred_0, col = "#FFFFFF", pch = 16, 
     xlab = "balance", ylab = "Pr(default)", cex.lab = 1.2)

points(x_grid, logit_y_pred_0, col = "dodgerblue2", type = "l", lty = 1, lwd = 3)
points(x_grid, logit_y_pred_1, col = "darkorange1", type = "l", lty = 1, lwd = 3)

boxplot(balance ~ student, data = default_data, 
        col = c("dodgerblue2", "darkorange1"), cex.lab = 1.2)
par(mfrow = c(1, 1))





sa_data <- read.csv(file.path(datapath, "SouthAfrican.csv"))
names(sa_data)
head(sa_data, 5)

library(GGally)
ggpairs(sa_data, aes(colour = as.factor(sa_data$CLASS), alpha = 0.4)) 

sa_data$CLASS <- as.factor(sa_data$CLASS)
sa_data$famhist <- as.factor(sa_data$famhist)
sa_mdl <- glm(CLASS ~ ., data = sa_data, family = binomial(link = "logit"))
summary(sa_mdl)

# hsb_df <- foreign::read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
hsb_df <- foreign::read.dta(file.path(datapath, "hsbdemo.dta"))
names(hsb_df)
head(hsb_df, 5)
nrow(hsb_df)
xtabs( ~ ses + prog, data = hsb_df)


boxplot(write ~ prog, data = hsb_df, 
        col = c("dodgerblue2", "darkorange1", "forestgreen"), cex.lab = 1.2)

hsb_df$prog
hsb_df$ses

library(glmnet)
# Create model matrix
xmat <- model.matrix(prog ~ ses + write, data = hsb_df)
# Fit multinomial glmnet model using model matrix
mdl_mc <- glmnet(x = xmat[,2:ncol(xmat)], y = hsb_df$prog, 
                 family = "multinomial", standardize = TRUE, intercept = TRUE,
                 lambda = 0, alpha = 0)
# Read multinomial glmnet coefficients
mdl_mc_coef <- t(sapply(1:length(mdl_mc$beta), function(i) {
  as.matrix(mdl_mc$beta[[i]])
}))
mdl_mc_coef <- cbind(mdl_mc$a0, mdl_mc_coef)
rownames(mdl_mc_coef) <- names(mdl_mc$beta)
colnames(mdl_mc_coef) <- c("(Intercept)", rownames(mdl_mc$beta[[1]]))
mdl_mc_coef



