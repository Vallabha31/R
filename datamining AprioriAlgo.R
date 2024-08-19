# Install and load required packages
if (!requireNamespace("arulesViz", quietly = TRUE)) {
  install.packages("arulesViz")
}

library(arules)
library(arulesViz)
# Load the dataset with corrected file path
dataset <- read.csv("C:/tmp/datamining/Luxury_Beauty.csv")

# Check the structure of the dataset
str(dataset)

# Make sure the column names match the actual column names in your dataset
# Adjust these column names based on your dataset structure
selected_data <- dataset[, c("reviewerID", "asin")]

# Convert data to transactions
transactions <- as(split(selected_data$asin, selected_data$reviewerID), "transactions")

# Apply the Apriori algorithm
frequent_itemsets <- apriori(transactions, parameter = list(support = 0.01, minlen = 2))

# Mine association rules from frequent itemsets
association_rules <- apriori(transactions, parameter = list(support = 0.01, confidence = 0.7))

# Display the association rules
inspect(association_rules)

# Check the structure of transactions
str(transactions)

# Display the frequent itemsets
inspect(frequent_itemsets)

# Display the association rules
inspect(association_rules)

# Check if association_rules is correctly created
str(association_rules)

# Plot the association rules with increased 'max' and circular layout
plot(association_rules, method = "interactive", control = list(type = "items")



