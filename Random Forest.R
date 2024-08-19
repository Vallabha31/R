# Install and load necessary libraries
install.packages("randomForest")
library(randomForest)

# Load your dataset (replace 'your_dataset.csv' with your actual file path or URL)
df <- read.csv("C:/tmp/datamining/Book1.csv")

# Display the first few rows of the dataset
head(df)

# Assume 'price' is the target variable and other columns are features
features <- c("bed", "bath", "acre_lot", "house_size")
target <- "price"

# Split the dataset into training and testing sets
set.seed(123)
index <- createDataPartition(df$price, p = 0.8, list = FALSE)
train_data <- df[index, ]
test_data <- df[-index, ]

# Create a Random Forest model
rf_model <- randomForest(price ~ ., data = train_data[, c(features, target)], ntree = 100)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model
mse <- mean((test_data$price - predictions)^2)
r2 <- 1 - mse / var(test_data$price)

cat("Mean Squared Error:", mse, "\n")
cat("R-squared:", r2, "\n")

# Visualize predictions vs actual values
plot(test_data$price, predictions, xlab = "Actual Prices", ylab = "Predicted Prices", main = "Actual Prices vs Predicted Prices (Random Forest)")
abline(0, 1, col = "red")  # Add a diagonal line for comparison
