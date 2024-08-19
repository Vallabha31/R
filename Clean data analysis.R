# Install and load required libraries
library(arules)
library(arulesViz)

# Read the dataset (assuming 'luxury_beauty.csv' is in your working directory)
data <- read.csv("C:/tmp/datamining/Luxury_Beauty.csv", header = TRUE)

# Remove missing data entries
data <- na.omit(data)

# Convert data to transactions
transactions <- as(data, "transactions")

# Apply Apriori algorithm
association_rules <- apriori(transactions, parameter = list(support = 0.01, confidence = 0.7))

# Visualize the rules with adjusted max.overlaps
plot(association_rules, method = "graph", control = list(type = "items", max = 1200, verbose = TRUE, max.overlaps = 100,  alpha = 1))

