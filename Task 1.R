# Load necessary libraries
library(tm)
library(topicmodels)
library(igraph)
library(ggraph)

# Custom function to clean text
clean_text <- function(text) {
  # Remove invalid multibyte strings
  iconv(text, to = "UTF-8", sub = " ")
}

# Read the data
data <- read.csv("C:/Users/pande/Downloads/AmazonTouristerReviewPosts_UK.csv", stringsAsFactors = FALSE)

# Preprocess the text data
corpus <- Corpus(VectorSource(data$x))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, clean_text)  # Apply custom clean_text function
corpus <- tm_map(corpus, stripWhitespace)

# Remove empty documents
corpus <- corpus[!sapply(corpus, function(x) length(content(x)) == 0)]

# Create document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Fit LDA model
lda_model <- LDA(dtm, k = 5, method = "Gibbs", control = list(seed = 1234))

# Get terms associated with topics
terms <- terms(lda_model, 10)

# Check the dimensions of the terms matrix
dim(terms)

# Create network graph
term_graph <- graph_from_adjacency_matrix(as.matrix(terms), mode = "max", weighted = TRUE)

# Plot network graph
ggraph(term_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = weight), edge_colour = "blue") +
  geom_node_text(aes(label = name), size = 5, color = "red") +
  theme_void()

# Topic proportion plot
document_topics <- posterior(lda_model)$topics
topic_proportions <- colMeans(document_topics)
# Automatically extract keywords as labels for each topic
topic_labels <- lapply(terms, function(words) paste(head(words, 3), collapse = ", "))
plot_labels <- unname(topic_labels)
# Plotting the topic proportion plot with updated labels
barplot(topic_proportions, names.arg = paste("Topic", 1:length(topic_proportions)), 
        xlab = "Topics", ylab = "Proportion", main = "Topic Proportion Plot",
        col = rainbow(length(topic_proportions)))

