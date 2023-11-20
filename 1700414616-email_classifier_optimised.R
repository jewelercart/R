library(tm)
library(data.table)

# Function to load texts from a directory
load_texts <- function(folder) {
  file_paths <- list.files(folder, full.names = TRUE)
  texts <- vector("list", length(file_paths))
  for (i in seq_along(file_paths)) {
    texts[[i]] <- tolower(readLines(file_paths[i], warn = FALSE, n = 10, encoding = "UTF-8"))  # Read only the first few lines
  }
  return(unlist(texts))
}

clean_text <- function(text) {
  iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")
}
# Load spam and ham data
# Use a subset of data if necessary
spam <- load_texts("/cloud/project/spam")
ham <- load_texts("/cloud/project/easy_ham")


spam <- lapply(spam, clean_text)
ham <- lapply(ham, clean_text)

# Combine and label data
data <- data.table(text = c(spam, ham), label = c(rep(1, length(spam)), rep(0, length(ham))))

# Text preprocessing
corpus <- VCorpus(VectorSource(data$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Create a document-term matrix with more efficient settings
dtm_controls <- list(wordLengths = c(1, Inf), weighting = weightTfIdf, bounds = list(global = c(5, Inf)))
dtm <- DocumentTermMatrix(corpus, control = dtm_controls)

library(e1071)

# Reduce the size of the DTM (select only top terms)
top_terms <- findFreqTerms(dtm, lowfreq = 50)
dtm_reduced <- dtm[, top_terms]

# Split data into training and testing sets
set.seed(42)
train_indices <- sample(seq_len(nrow(dtm_reduced)), size = 0.8 * nrow(dtm_reduced))
train_data <- dtm_reduced[train_indices, ]
test_data <- dtm_reduced[-train_indices, ]
train_labels <- data$label[train_indices]
test_labels <- data$label[-train_indices]

# Train Naive Bayes model
model <- naiveBayes(as.matrix(train_data), as.factor(train_labels))

# Predict and evaluate
predictions <- predict(model, as.matrix(test_data))
confusionMatrix <- table(Predicted = predictions, Actual = test_labels)
print(confusionMatrix)

# Predict and evaluate
predictions <- predict(model, as.matrix(test_data))
confusionMatrix <- table(Predicted = predictions, Actual = test_labels)
print(confusionMatrix)

# Function to predict new document
# Function to predict new document
predict_new <- function(new_doc) {
  new_corpus <- VCorpus(VectorSource(new_doc))
  new_corpus <- tm_map(new_corpus, content_transformer(tolower))
  new_corpus <- tm_map(new_corpus, removePunctuation)
  new_corpus <- tm_map(new_corpus, removeWords, stopwords("en"))
  new_corpus <- tm_map(new_corpus, stripWhitespace)
  
  # Create a DTM for the new document
  new_dtm <- DocumentTermMatrix(new_corpus, control = list(dictionary = Terms(dtm)))
  
  # Predict using the model
  prediction <- predict(model, as.matrix(new_dtm))
  
  # Return a user-friendly message
  if (length(prediction) > 0) {
    if (prediction[1] == 1) {
      return("The email given is spam.")
    } else {
      return("The email given is not spam.")
    }
  } else {
    return("Unable to classify the email.")
  }
}

# Example usage
new_doc <- "Congratulations! You have been selected for an exclusive offer. Get a 70% discount on our new range of designer watches. This is a once-in-a-lifetime opportunity just for you.

But hurry! This offer is valid for a limited time only. Visit our website now and use the code EXCLUSIVE70 at checkout to avail of this amazing discount.

Don't miss out on this incredible deal. Act now and be the envy of all your friends with a stylish new watch."
print(predict_new(new_doc))
