library(dplyr)

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

# Convert the date columns to proper Date/Datetime format
# Remove the time and 'Z' part, keeping only the year-month-day
data2$created_at <- sub("T.*", "", data2$created_at)
data2$created_at <- as.Date(data2$created_at, format="%Y-%m-%d")
data2$Date_election <-  as.Date(data2$Date_election, format="%Y-%m-%d")

# Filter the dataset to remove rows where created_at > Date_election
filtered_data <- data2 %>%
        filter(created_at <= Date_election)

candidate_count <- filtered_data %>%
        count(Candidate, name = "count")


# Filter to keep only candidates with at least 100 tweets
candidates_100 <- candidate_count %>% filter(count >= 100)
filtered_data <- filtered_data %>% filter(Candidate %in% candidates_100$Candidate)
filtered_data$min_date <- filtered_data$Date_election - 60
filtered_data <- filtered_data %>% filter(created_at >= min_date)
filtered_data <- select(filtered_data, Candidate, Party, text)
names(filtered_data) <- c("candidate_name", "party_name", "text")
filtered_data$id <- seq_len(nrow(filtered_data))

rio::export(filtered_data, "Twitter_data.csv", format="csv")
