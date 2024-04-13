datapath <- "James_2023_CSV"
# Read credit dataset
baseball_data <- read.csv(file.path(datapath, "Hitters.csv"))
names(baseball_data)
head(baseball_data, 6)

library(rpart)


