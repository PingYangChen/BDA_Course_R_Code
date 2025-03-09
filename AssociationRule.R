#install.packages(c("arules", "arulesViz"))
library(arules)
library(arulesViz)


data(Groceries)
head(DATAFRAME(Groceries), 12)

summary(Groceries)
itemFrequencyPlot(Groceries, topN = 20)
#inspect(Groceries)




rules <- apriori(Groceries, 
  parameter = list(
    support = 0.001,   # minimal support of an item set
    confidence = 0.5, # minimal confidence of rules/association hyperedges
    minlen = 1 # minimal number of items per item set
))

rules

# Select a subset of rules using partial matching on the items
# in the right-hand-side and a quality measure
rules.sub <- subset(rules, subset = rhs %pin% "whole milk" & lift > 1.3)

# Display the top 3 support rules
inspect(head(rules.sub, n = 3, by = "support"))
# Display the top 3 confidence rules
inspect(head(rules.sub, n = 3, by = "confidence"))
# Display the top 3 lift rules
inspect(head(rules.sub, n = 3, by = "lift"))




plot(rules.sub, method = "scatterplot")


plot(rules.sub, method = "graph")

plot(rules, method="grouped", limit = 20)

plot(rules, method = "matrix", limit = 6)






df <- read.csv('sample_data/lotto_2023-202503037.csv')
names(df)

SplitNumbers <- function(num_list) {
  tmp <- strsplit(num_list, ",")[[1]]
  n_num <- length(tmp)
  out <- as.integer(c(strsplit(tmp[1], "\\[")[[1]][2],
                      tmp[2:(n_num-1)],
                      strsplit(tmp[n_num], "\\]")[[1]][1]))
  return(out)
}

sn <- tid <- c()
for (i in 1:nrow(df)) {
  stmp <- SplitNumbers(df$獎號[i])
  sn <- c(sn, stmp)
  tid <- c(tid, rep(df$期別[i], length(stmp)))
}

df_splitted <- data.frame(
  tid = tid,
  item = sn
)


adf <- as(split(df_splitted$item, df_splitted$tid), "transactions")
head(DATAFRAME(adf), 12)
inspect(adf)


rules <- apriori(adf, 
                 parameter = list(
                   support = 0.001,   # minimal support of an item set
                   confidence = 0.5, # minimal confidence of rules/association hyperedges
                   minlen = 1 # minimal number of items per item set
                 ))
# Display the top 3 support rules
inspect(head(rules, n = 5, by = "support"))
