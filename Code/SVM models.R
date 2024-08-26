
#######################################################
##### PRESIDENTIAL TWEETS CLASSIFICATION MODELS #######
#######################################################


library(tidyverse)
library(text)
library(tm)
library(e1071)
library(naivebayes)
library(caret)
library(ggplot2)

# Load your training and test datasets
training_data <- rio::import("/Users/Fede/Desktop/Poster/Data/training_set.xlsx")
test_data <- rio::import("/Users/Fede/Desktop/Poster/Data/test_set.xlsx")

#training_data <- training_data[training_data$Classification_personalism != 0,]
#test_data <- test_data[test_data$Classification_personalism != 0,]
# Ensure labels are factors
training_data$label <- as.factor(training_data$GPT_class_es)
test_data$label <- as.factor(test_data$Classification_personalism)


# Función para limpiar tweets
limpiar_tweet <- function(tweet) {
        # Combinar todas las operaciones de limpieza en una sola expresión regular
        tweet <- tweet %>%
                tolower() %>%
                str_remove_all("http\\S+|www\\S+") %>%  # Eliminar URLs
                str_remove_all("@\\w+") %>%  # Eliminar menciones
                str_remove_all("#\\w+") %>%  # Eliminar hashtags
                str_remove_all("[[:punct:]]") %>%  # Eliminar signos de puntuación
                str_remove_all("\\d+") %>%  # Eliminar números
                str_squish()  # Eliminar espacios en blanco adicionales
        
        return(tweet)
}



training_data$text <- limpiar_tweet(training_data$text)
test_data$text <- limpiar_tweet(test_data$text)


training_data <- select(training_data, text, label)
test_data <-select(test_data, text, label)
        


# Combine training and test data to ensure consistent DTM
combined_data <- rbind(training_data, test_data)

# Create a corpus from the combined text data
corpus_combined <- Corpus(VectorSource(combined_data$text))

# Preprocess the corpus: lowercase, remove punctuation, stopwords, and whitespace
corpus_combined <- tm_map(corpus_combined, content_transformer(tolower))
corpus_combined <- tm_map(corpus_combined, removePunctuation)
corpus_combined <- tm_map(corpus_combined, removeWords, stopwords("es"))
corpus_combined <- tm_map(corpus_combined, stripWhitespace)
corpus_combined <- tm_map(corpus_combined, stemDocument, language = "spanish")

# Create a Document-Term Matrix (DTM) from the combined data
dtm_combined <- DocumentTermMatrix(corpus_combined)

# Split the combined DTM back into training and test DTMs
dtm_training <- dtm_combined[1:nrow(training_data), ]
dtm_test <- dtm_combined[(nrow(training_data) + 1):nrow(combined_data), ]

# Convert the DTMs to matrices
train_matrix <- as.matrix(dtm_training)
test_matrix <- as.matrix(dtm_test)

# Train the SVM model
model_svm <- train(
        x = train_matrix,
        y = training_data$label,
        method = "svmLinear",
        trControl = trainControl(method = "cv", number = 5)
)

# Make predictions using the SVM model
predictions_svm <- predict(model_svm, test_matrix)

# Convert predictions and true labels to factors with the same levels
predictions_svm <- factor(predictions_svm, levels = levels(training_data$label))
test_labels <- factor(test_data$label, levels = levels(training_data$label))

# Compute the confusion matrix
confusion_matrix_svm <- confusionMatrix(predictions_svm, test_labels)

# Print the confusion matrix
print(confusion_matrix_svm)



# Compute the confusion matrix
confusion_matrix_svm <- confusionMatrix(predictions_svm, test_labels)
confusion_matrix_data <- as.data.frame(confusion_matrix_svm$table)

# Calculate the relative frequencies
confusion_matrix_data <- confusion_matrix_data %>%
        group_by(Reference) %>%
        mutate(RelativeFreq = Freq / sum(Freq))

# Create a plot for the confusion matrix with relative frequencies
ggplot(confusion_matrix_data, aes(x = Reference, y = Prediction)) +
        geom_tile(aes(fill = RelativeFreq), color = "black", size = 0.5) +  # Add borders to tiles
        scale_fill_gradient(low = "#D9D9D9", high = "#215732", name = "Relative Frequency") +  # Custom color palette
        geom_text(aes(label = sprintf("%.2f", RelativeFreq * 100)), color = "black", size = 4) +  # Show percentages
        theme_minimal(base_size = 15) +  # Increase base font size for better readability
        theme(
                axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better fit
                plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
                legend.position = "none"  # Place legend on the right
        ) +
        labs(title = "Confusion Matrix (Relative Frequencies)", x = "Actual Label", y = "Predicted Label")


