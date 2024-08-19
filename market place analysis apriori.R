#part-1: Load and explore data
# Load necessary libraries
library(arules)
library(arulesViz)

# Load the dataset
luxury_data <- read.csv("C:/tmp/datamining/Luxury_Beauty.csv")

# Explore the structure of the dataset
str(luxury_data)

# Data Processing

# Depending on your dataset structure, you might need to select relevant columns and convert them into transactions

# Example: Assuming you have columns 'reviewerID' and 'asin' for transactions
transactions <- with(luxury_data, split(asin, reviewerID))

# Create a transaction object
transactions <- as(transactions, "transactions")

# Summary of transactions
summary(transactions)

#Run apriori algorithm

# Run Apriori algorithm
frequent_itemsets <- apriori(transactions, parameter = list(support = 0.01, confidence = 0.7))

# Display frequent itemsets
inspect(frequent_itemsets)

#Generate and Inspect Association Rules

# Generate association rules
association_rules <- as(rules(frequent_itemsets), "data.frame")

# Filter rules based on confidence (adjust threshold as needed)
association_rules <- subset(association_rules, confidence > 0.7)

# Display association rules
inspect(association_rules)

#Visualize Association rules

# Visualize the rules (you may need to install 'arulesViz' package if not installed)
library(arulesViz)


# Plot the association rules with a higher value for max
plot(association_rules, method = "graph", control = list(type = "items", max = 1200, verbose = TRUE, max.overlaps = 500))

