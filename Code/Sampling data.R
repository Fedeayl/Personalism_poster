#### Randomize tweet extraction to manual classification 
setwd("/Users/Fede/Desktop/PAPERS/Gtrends_personalism/Data/Extracciones Twitter")

file_list <- list.files(pattern = "*.csv")

# Import all files and combine them
combined_data <- lapply(file_list, read.csv) %>%
        bind_rows()

data <- combined_data[combined_data$is_retweet == F & combined_data$is_quote == F,]
data <- dplyr::select(data, user_id, screen_name, created_at, text, favorite_count, retweet_count, hashtags)

candidates_list <- rio::import("/Users/Fede/Desktop/PAPERS/Gtrends_personalism/Data/TWITTER Base Candidatos LA.xlsx")
candidates <- select(candidates_list, Country, Year, Party, Candidate_exact, Party_exact,Party_Acronym, Candidate, Twitter_account, Votes_perc, Date_election)

data2 <- left_join(data, candidates, by = c("screen_name" = "Twitter_account"))
data2 <- data2[data2$Country != "BRASIL",]

# Convert the date columns to proper Date/Datetime format
# Remove the time and 'Z' part, keeping only the year-month-day
data2$created_at <- sub("T.*", "", data2$created_at)
data2$created_at <- as.Date(data2$created_at, format="%Y-%m-%d")
data2$Date_election <-  as.Date(data2$Date_election, format="%Y-%m-%d")

# Filter the dataset to remove rows where created_at > Date_election
filtered_data <- data2 %>%
        filter(created_at <= Date_election) %>%
        filter(created_at >= (Date_election -180))

# Filter to keep only candidates with at least 100 tweets
candidate_count <- filtered_data %>%
        count(Candidate, name = "count")

candidates_100 <- candidate_count %>% filter(count >= 100)
filtered_data <- filtered_data %>% filter(Candidate %in% candidates_100$Candidate)

## Random subset
set.seed(456) # Set a seed for reproducibility

#  Randomly sample 1000 tweets
sampled_tweets <- filtered_data[sample(1:nrow(filtered_data), 1000), ]

# Split the sampled tweets into training and test sets
train_indices <- sample(1:nrow(sampled_tweets), 0.6* nrow(sampled_tweets))

training_set <- sampled_tweets[train_indices, ]
test_set <- sampled_tweets[-train_indices, ]

# Save the datasets
rio::export(training_set, "training_set.xlsx", format="xlsx")
rio::export(test_set, "test_set.xlsx", format="xlsx")



# New sample extension

set.seed(456) # Set a seed for reproducibility

# Initialize a vector to store indices of sampled tweets
sampled_indices <- c()
sampled_indices <- sample(1:nrow(filtered_data), 1000)
sampled_tweets <- filtered_data[sampled_indices, ]

# Split the sampled tweets into training and test sets
train_indices <- sample(1:nrow(sampled_tweets), 0.6 * nrow(sampled_tweets))
training_set <- sampled_tweets[train_indices, ]
test_set <- sampled_tweets[-train_indices, ]

# Expand the sample by excluding already sampled tweets
remaining_indices <- setdiff(1:nrow(filtered_data), sampled_indices)
new_sample_size <- 1000  # Adjust this number as needed
new_sampled_indices <- sample(remaining_indices, new_sample_size)

# Add the new sampled indices to the existing ones
sampled_indices <- c(sampled_indices, new_sampled_indices)
new_sampled_tweets <- filtered_data[new_sampled_indices, ]

# Update the training and test sets with the expanded sample
train_indices <- sample(1:nrow(new_sampled_tweets), 0.6 * nrow(new_sampled_tweets))
training_set <- new_sampled_tweets[train_indices, ]
test_set <- new_sampled_tweets[-train_indices, ]

rio::export(training_set, "training_set_exp.xlsx", format="xlsx")
rio::export(test_set, "test_set_exp.xlsx", format="xlsx")

