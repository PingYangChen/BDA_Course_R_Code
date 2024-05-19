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
