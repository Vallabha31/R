# Load required library
library(caret)

# Load dataset
data <- read.csv("C:/Users/pande/Downloads/Amazon ratings.csv")

# Check for missing values and impute if necessary
if (sum(is.na(data)) > 0) {
  # Impute missing values (replace 'median' with 'mean' or 'mode' if needed)
  data <- na.omit(data)  # Remove rows with missing values
}

# Convert categorical variables to factors
categorical_columns <- c("UserID", "ProductID")
data[categorical_columns] <- lapply(data[categorical_columns], as.factor)

# Split data into train and test sets
set.seed(123)  # for reproducibility
train_index <- createDataPartition(data$Ratings, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Check if any factor levels are missing in test data
for (col in categorical_columns) {
  if (!all(levels(test_data[[col]]) %in% levels(train_data[[col]]))) {
    stop(paste("Factor levels in", col, "are not consistent between train and test data."))
  }
}

# Check if Ratings_binary column exists in train_data
if (!"Ratings_binary" %in% names(train_data)) {
  stop("Ratings_binary column does not exist in train_data.")
}

# Check the unique values of Ratings_binary
unique(train_data$Ratings_binary)

# Ensure that Ratings_binary is a factor with two levels (0 and 1)
train_data$Ratings_binary <- as.factor(ifelse(train_data$Ratings >= 4, 1, 0))

# Train Logistic Regression model
log_model <- train(Ratings_binary ~ ., 
                   data = train_data, 
                   method = "glmnet",
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.001, 0.1, by = 0.001)))

# Make predictions on the test set
predictions <- predict(log_model, newdata = test_data, type = "raw")

# Evaluate the model
conf_matrix <- table(predictions, test_data$Ratings_binary)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Print confusion matrix and accuracy
print(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

