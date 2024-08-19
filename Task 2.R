# Load necessary libraries
library(tm)
library(sentimentr)
library(RColorBrewer)

# Define a function to clean text
clean_text <- function(text) {
  # Replace invalid characters with empty string
  cleaned_text <- iconv(text, to = "UTF-8", sub = " ")
  return(cleaned_text)
}

# Read the data
data <- read.csv("C:/Users/pande/Downloads/AmazonTouristerReviewPosts_UK.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

# Apply cleaning function to the text data
if (!is.null(data$x)) {
  data$x <- sapply(data$x, clean_text)
} else {
  stop("Error: 'x' column not found in the data.")
}

# Check if the 'sentiment' function exists in the 'sentimentr' package
if ("sentiment" %in% ls("package:sentimentr")) {
  # Perform sentiment analysis
  sentiment <- sentimentr::sentiment(data$x)
  
  # Convert sentiment score to numeric
  sentiment_numeric <- as.numeric(sentiment$ave_sentiment)
  
  # Create a matrix for sentiment analysis
  sentiment_matrix <- table(ifelse(sentiment_numeric > 0, "Positive", "Negative"))
  
  # Create a heatmap
  heatmap(as.matrix(sentiment_matrix), col = brewer.pal(9, "RdBu"), 
          main = "Sentiment Analysis Heatmap", xlab = "Sentiment", ylab = "Frequency")
} else {
  print("Error: 'sentiment' function not found in the 'sentimentr' package.")
}
# Check the structure of sentiment_matrix
str(sentiment_matrix)

# Check the structure of sentiment_numeric
str(sentiment_numeric)

# Perform sentiment analysis
sentiment <- sentimentr::sentiment(data$x)

# Check the structure of the sentiment object
str(sentiment)

# Check the structure of data$x
str(data$x)

# Check for missing values
summary(data$x)

# Perform sentiment analysis
sentiment <- sentimentr::sentiment(data$x)

# Check the structure of the sentiment object
str(sentiment)

# Load necessary libraries
library(ggplot2)

# Plot the sentiment distribution
ggplot(sentiment, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Sentiment Distribution",
       x = "Sentiment Score",
       y = "Frequency")

