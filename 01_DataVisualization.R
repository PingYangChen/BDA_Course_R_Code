#install.packages(c("rcompanion", "ltm"))
# Install packages of example datasets
#install.packages("datasauRus")
#
#install.packages("plotly")
# Load packages of data process
library(dplyr)
# Load packages of example datasets
library(datasauRus)
# Load packages of visualization
library(ggplot2)

# Review of Descriptive Statistics
# ------------------------------------------------------------
xn <- round(rnorm(100, 100, 15))
xc <- rbinom(100, 1, .6)
## Univariate Descriptive Statistics
### Central Tendency of a numerical data
mean(xn) # mean
median(xn) # median
quantile(xn, c(.0, .25, .5, .75, 1.)) # quartiles
# To my best knowledge, R does not provide MODE function
data_mode <- function(x) {
  tx <- table(xn)
  return(as.numeric(names(tx)[which.max(tx)]))
}
data_mode(xn) # mode
### Variability of a numerical data
var(xn) # variance
sd(xn) # standard deviation
diff(range(xn)) # range
IQR(xn) # interquartile range
### High order statistics of a numerical data
e1071::skewness(xn) # skewness
e1071::kurtosis(xn) # kurtosis
### Frequency table for categorical data
table(xc)
## Bivariate Descriptive Statistics
### Common correlations between two numerical, ordinal categorial data
yn <- xn + rnorm(100, 0, 10)
cor(xn, yn, method = "pearson")
cor(xn, yn, method = "kendall")
cor(xn, yn, method = "spearman")
### Cramer's V Correlation between two nominal categorical data
yc <- rbinom(100, 2, .7)
library(rcompanion)
cramerV(xtabs(~ yc + xc))
### Point-biserial Correlation between numerical and binary data
library(ltm)
biserial.cor(xn, xc, use = c("all.obs"), level = length(unique(xc)))


# Descriptive Statistics and Graphs of datasauRus datasets
# ------------------------------------------------------------
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
### If using dplyr package
# datasauRus::datasaurus_dozen %>% 
#   group_by(dataset) %>% 
#     summarize(
#       mean_x    = mean(x),
#       mean_y    = mean(y),
#       std_dev_x = sd(x),
#       std_dev_y = sd(y),
#       corr_x_y  = cor(x, y)
#     )
## Draw scatter plots for each dataset
par(mfrow = c(3, 5))
for (i in 1:length(unique_dataset)) {
  loc <- which(datasauRus::datasaurus_dozen$dataset == unique_dataset[i])
  tmp <- datasauRus::datasaurus_dozen[loc,2:3]
  plot(tmp$x, tmp$y, pch = 16, xlab = "x", ylab = "y", main = unique_dataset[i])
}
par(mfrow = c(1, 1))


# Descriptive Statistics and Graphs of Anscombe's quartet datasets
# ------------------------------------------------------------
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
# ------------------------------------------------------------
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
# ------------------------------------------------------------
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


# ggplot2 Tutorial
# ------------------------------------------------------------
## Histograms
## ===========================================================
library(ggplot2)
set.seed(1)
exp_data <- data.frame(x = rexp(1000, 0.1))
ggplot(exp_data) +
  geom_histogram( aes(x = x), binwidth = 0.5 ) +
  labs(title = "binwidth = 0.5")
ggplot(exp_data) +
  geom_histogram( aes(x = x), binwidth = 2 ) +
  labs(title = "binwidth = 2")
ggplot(exp_data) +
  geom_histogram( aes(x = x), binwidth = 20 ) +
  labs(title = "binwidth = 20")
## Histogram's Example (thinking process of drawing a statistical chart)
data(iris)
### Default Histogram
ggplot(iris) +
  geom_histogram( aes(x = iris[,1]), binwidth = 0.2) +
  labs(x = names(iris)[1])
### Change the scale of the Y-axis to be "denisty" of the bins for Histogram
### Separately draw histograms for each group in the iris data
### However, the bars seem to be stacked together
ggplot(iris) +
  geom_histogram( aes(
    x = iris[,1], 
    y = after_stat(density), 
    fill = Species
  ), binwidth = 0.2) +
  labs(x = names(iris)[1])
### Make the bars be plotted indivisually
ggplot(iris) +
  geom_histogram( aes(
    x = iris[,1], 
    y = after_stat(density), 
    fill = Species
  ), binwidth = 0.2, alpha = 0.6, position = 'identity') +
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) +
  labs(x = names(iris)[1])
### Add estimated density curve
ggplot(iris) +
  geom_histogram( aes(
    x = iris[,1], 
    y = after_stat(density), 
    fill = Species
  ), binwidth = 0.2, alpha = 0.6, position = 'identity') +
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) +
  geom_density( aes(x = iris[,1], fill = Species), alpha = .3) +
  labs(x = names(iris)[1])

## Boxplots
## ===========================================================
### Boxplot for One dataset
data(iris)
ggplot(iris) +
  geom_boxplot( aes(
    y = iris[,1]
  )) +
  labs(y = names(iris)[1]) +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
  )
### Boxplot for subgroups in the dataset
ggplot(iris) +
  geom_boxplot( aes(
    x = Species,
    y = iris[,1]
  )) +
  labs(y = names(iris)[1])
### Violin plot for subgroups in the dataset
ggplot(iris) +
  geom_violin( aes(
    x = Species,
    y = iris[,1]
  )) +
  labs(y = names(iris)[1])
### Boxplot for subgroups that separated by more than 1 factor 
data(mtcars)
mtcars$am <- as.factor(mtcars$am)
mtcars$cyl <- as.factor(mtcars$cyl)
ggplot(mtcars) +
  geom_boxplot( aes(
    x = cyl,
    y = disp,
    fill = am
  )) +
  labs(y = "disp")

## Bar Chart
## ===========================================================
data(mtcars)
mtcars$am <- as.factor(mtcars$am)
mtcars$cyl <- as.factor(mtcars$cyl)
#- Direct draw bar plot using stat = "count"
ggplot(mtcars) + 
  geom_bar(
    aes(x = cyl),
    position = "identity", stat = "count"
  )
#- Create frequency table fist
library(dplyr)
mtcars_freq_table <- mtcars %>%
  group_by(cyl) %>%
  summarise(counts = n()) 
#- Then draw bar plot using stat = "identity"
ggplot(mtcars_freq_table) +  
  geom_bar(
    aes(x = cyl, y = counts),
    position = "identity", stat = "identity"
  )
### Bar chart for subgroups in the dataset
library(dplyr)
mtcars_freq_table_2 <- mtcars %>%
  group_by(cyl, am) %>%
  summarise(counts = n()) 
ggplot(mtcars_freq_table_2) + 
  geom_bar(
    aes(x = cyl, y = counts, fill = am),
    position = "dodge", stat = "identity"
  )

## Line plot
## ===========================================================
data(longley)
library(ggplot2)
ggplot(longley) +
  geom_line(
    aes(x = 1:nrow(longley), y = GNP.deflator)
  ) +
  scale_x_continuous(
    breaks = 1:nrow(longley), 
    labels = rownames(longley)
  ) +
  labs(x = "Year", y = "GNP.deflator")

## Scatter Plots
## ===========================================================
data(iris)
library(ggplot2)
### Basic Scatter plot 
ggplot(iris, aes(x = iris[,1], y = iris[,2])) +
  geom_point() +
  labs(x = names(iris)[1], y = names(iris)[2])
### Scatter plot for subgroups in the dataset
ggplot(iris, aes(x = iris[,1], y = iris[,2], color = Species)) +
  geom_point() +
  labs(x = names(iris)[1], y = names(iris)[2])
### Scatter plot matrix for multiple variables
library(ggplot2)
library(gridExtra)
#- Draw scatter plots for Iris dataset
iris_scatter_plot <- function(i, j) {
  ggplot(iris, aes(x = iris[,i], y = iris[,j], color = Species)) +
    geom_point() +
    labs(x = names(iris)[i], y = names(iris)[j])
}
scatter_plots <- vector("list", 16)
counter <- 1
for (i in 1:4) {
  for (j in 1:4) {
    scatter_plots[[counter]] <- iris_scatter_plot(i, j)
    counter <- counter + 1
  }
}
marrangeGrob(scatter_plots, nrow = 4, ncol = 4, top = "")
#- Or
library(ggplot2)
library(GGally)
ggpairs(iris, aes(colour = Species, alpha = 0.4))

## Contour Plots
## ===========================================================
#- Reshape matrix to long data
volcano_long_num <- data.frame(
  x = rep(1:ncol(volcano), each = nrow(volcano)), 
  y = rep(1:nrow(volcano), time = ncol(volcano)), 
  value = as.vector(volcano)
)
#- Draw contour
ggplot(volcano_long_num) +
  geom_contour(aes(x, y, z = value)) 
#- Add color to the contour plot
ggplot(volcano_long_num) +
  geom_contour(aes(x, y, z = value), colour = "white") +
  geom_contour_filled(aes(x, y, z = value))  

## Heatmaps
## ===========================================================
#- Reshape matrix to long data
data(volcano)
volcano_long_fac <- data.frame(
  x = as.factor(rep(1:ncol(volcano), each = nrow(volcano))), 
  y = as.factor(rep(1:nrow(volcano), time = ncol(volcano))), 
  value = as.vector(volcano)
)
#- Draw heatmap
ggplot(volcano_long_fac) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_distiller(palette = "Spectral") +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank()
  )
### Choose colors for heatmap
iris_cor <- cor(iris[1:4])
round(iris_cor[c(3,4,1,2),c(3,4,1,2)]*100, 2)
iris_cor_long <- data.frame(
  x1 = rep(colnames(iris_cor), each = length(rownames(iris_cor))),
  x2 = rep(rownames(iris_cor), time = length(colnames(iris_cor))),
  corr = as.vector(iris_cor)
)
ggplot(iris_cor_long) +
  geom_tile(aes(x1, x2, fill = corr)) +
  scale_fill_distiller(
    palette = "RdBu", limits = c(-1, 1)
  ) 

# Visualization for Big Data
# ------------------------------------------------------------
## Observe the appropriateness of using charts for the big data
## ===========================================================
set.seed(1)
#- Change n <- 1e5 and try again
n <- 1e+02
y <- as.factor(sample(LETTERS[1:4], n, replace=T, prob=c(0.1, 0.1, 0.5, 0.3)))
x1 <- rnorm(n)
x2 <- rbeta(n, 0.5, 0.5)
xydata <- data.frame(y, x1, x2)
par(mfrow=c(1,4))
boxplot(x1~y, data=xydata, ylab="x1", main="boxplot")
hist(x2, xlab="x2", main="hist")
barplot(table(y), xlab="y", col = 2:5, main="barplot")
plot(x1, x2, main="plot", col=as.integer(y)+1)
par(mfrow=c(1,1))

## Smooth Scatter Plots
## ===========================================================
### Simulate a demo data
n <- 1e+04
x1 <- rnorm(n, mean = -1, sd = 1)
y1 <- rnorm(n, mean = -1, sd = 1)
x2 <- rnorm(n, mean = 2, sd = 1)
y2 <- rnorm(n, mean = 2, sd = 1)
### Compare original scatter plot and the smooth scatter plot
par(mfrow=c(1, 2))
#- Original scatter plot
plot(x1, y1, main="black")
#- Smoothed scatter plot
smoothScatter(
  x1, y1, 
  col = "black", 
  colramp = colorRampPalette(c("white", "black")),
  main = 'Smoothed by Color Ramp'
)
par(mfrow=c(1, 1))
### Learn to set a transparent background for the Smoothed scatter plot
par(mfrow=c(2, 2))
smoothScatter(x1, y1, col = "black", 
              colramp = colorRampPalette(c("white", "black")),
              main = 'no transparency (white)')
smoothScatter(x1, y1, col = "black", 
              colramp = colorRampPalette(c("yellow", "black")),
              main = 'no transparency (yellow)')
#- Adjust the colors to be transparent
transparency_white <- colorRampPalette(
  c(adjustcolor("white", alpha.f = .05), "black"), alpha = TRUE
)
transparency_yellow <- colorRampPalette(
  c(adjustcolor("yellow", alpha.f = .05), "black"), alpha = TRUE
)
smoothScatter(x1, y1, col = "black", colramp = transparency_white, 
              main="transparency (white)")
smoothScatter(x1, y1, col = "black", colramp = transparency_yellow,
              main="transparency (yellow)")
par(mfrow=c(1, 1))
### Use the transparency setup to draw more than 1 Smoothed scatter plots on the same graph
pl <- c(-6, 6)
color_ramp_1 <- colorRampPalette(c("white", "#00BA38"))
color_ramp_2 <- colorRampPalette(c("white", "#619CFF"))
#- Adjust the colors to be transparent
transparency_1 <- colorRampPalette(
  c(adjustcolor("white", alpha.f = .05), "#00BA38"), alpha = TRUE
)
transparency_2 <- colorRampPalette(
  c(adjustcolor("white", alpha.f = .05), "#619CFF"), alpha = TRUE
)
par(mfrow = c(1, 3))
#- Original Scatterplot of Two Groups
plot(x1, y1, col = "#00BA38", xlim = pl, ylim = pl, main = "scatterplot")
points(x2, y2, col = "#619CFF")
#- Smooth Scatters of Two Groups Without Transparency
smoothScatter(x1, y1, col = "black", colramp = color_ramp_1,
              xlim = pl, ylim = pl, main = 'no transparency')
smoothScatter(x2, y2, col = "black", colramp = color_ramp_2, add = TRUE)
#- Smooth Scatters of Two Groups With Transparency
smoothScatter(x1, y1, col="black", colramp = transparency_1,
              xlim = pl, ylim = pl, main = "with transparency")
smoothScatter(x2, y2, col="black", colramp = transparency_2, add = TRUE)
par(mfrow = c(1, 1))

## Rectangle Binning and Hexbin Plot
## ===========================================================
large_data <- data.frame(
  x1 = x1, x2 = x2, y1 = y1, y2 = y2
)
### Rectangle Binning Plot
#- Original Scatterplot
ggplot(large_data) +
  geom_point(aes(x = x1, y = y1))
#- Default bins
ggplot(large_data) +
  geom_bin2d(aes(x = x1, y = y1))
#- User-defined bins
ggplot(large_data) +
  geom_bin2d(aes(x1, y1), bins = 10) +
  scale_fill_distiller(palette = "Reds") 
### Hexbin Plot
#- Original Scatterplot
ggplot(large_data) +
  geom_point(aes(x = x1, y = y1))
#- Default bins
ggplot(large_data) +
  geom_hex(aes(x1, y1))
#- User-defined bins
ggplot(large_data) +
  geom_hex(aes(x1, y1), bins = 10) +
  scale_fill_distiller(palette = "Reds") 
### Hexbin Plot for small dataset and draw for subgroups
library(ggplot2)
data(iris)
ggplot(iris) + 
  geom_point(aes(Sepal.Length, Sepal.Width))
ggplot(iris) + 
  geom_point(aes(Sepal.Length, Sepal.Width, color = Species))
ggplot(iris) +
  geom_hex(aes(Sepal.Length, Sepal.Width))
ggplot(iris) +
  geom_hex(aes(Sepal.Length, Sepal.Width, color = Species))
### Hexbin Plot Matrix
library(hexbin)
loc <- which(iris$Species == "setosa")
hexplom(iris[loc,1:4], xbins = 20, colramp = terrain.colors, upper.panel = panel.hexboxplot)
unique(iris$Species)


# Interactive Plot Using Plotly
# ------------------------------------------------------------
## Plotly Scatter plot
## ===========================================================
library(plotly)
data(iris)
fig <- plot_ly(x = iris$Species, y = iris[,1], type = "box")
fig <- layout(fig,
              xaxis = list(title = ''), 
              yaxis = list(title = names(iris)[1]) )
fig

## Plotly Boxplot
## ===========================================================
library(dplyr)
library(plotly)
data(mtcars)
mtcars$am <- as.factor(mtcars$am)
mtcars$cyl <- as.factor(mtcars$cyl)
fig <- plot_ly(
  x = mtcars$cyl, y = mtcars$disp, color = mtcars$am, type = "box"
  ) %>% layout(
    boxmode = "group", 
    xaxis = list(title = 'cyl'), 
    yaxis = list(title = 'disp')
  )
fig

## Plotly Heatmap
## ===========================================================
library(plotly)
fig <- plot_ly(
  z = volcano, type = "heatmap",
  colors = "Blues"
)
fig

## Rendering the ggplot2 Graphs to the Plotly Interactive UI
## ===========================================================
### Example: Heatmap
#- Reshape matrix to long data
data(volcano)
volcano_long_fac <- data.frame(
  x = as.factor(rep(1:ncol(volcano), each = nrow(volcano))), 
  y = as.factor(rep(1:nrow(volcano), time = ncol(volcano))), 
  value = as.vector(volcano)
)
library(ggplot2)
gp2 <- ggplot(volcano_long_fac) +
  geom_tile(aes(x, y, fill = value)) +
  #scale_fill_distiller(palette = "Spectral") +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank()
  )
library(plotly)
fig <- ggplotly(gp2)
fig
### Example: Hexbin Plot 
### (the R version of plotly does not provide hexbin function, but you can render ggplot hexbin into it)
library(ggplot2)
gp2 <- ggplot(large_data) +
  geom_hex(aes(x1, y1))
library(plotly)
fig <- ggplotly(gp2)
fig
## Save Plotly interactive graph
## ===========================================================
htmlwidgets::saveWidget(as_widget(fig), "plotly_demo.html")


