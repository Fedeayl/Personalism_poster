#### Model GPT model

# Merge classification

setwd("/Users/Fede/Desktop/Intentos")

file_list <- list.files(pattern = "*.csv")

# Import all files and combine them
combined_data_tweets <- lapply(file_list, read.csv) %>%
        bind_rows()

class_data <- combined_data_tweets[!duplicated(combined_data_tweets$ID), ]
names(class_data)[1] <- "id"

tweets <- rio::import("/Users/Fede/Desktop/Poster/Personalism_poster/Data/Twitter_data.csv")

tweets <- left_join(class_data, tweets)


setwd("/Users/Fede/Desktop/PAPERS/Gtrends_personalism/Data/Extracciones Twitter")

file_list <- list.files(pattern = "*.csv")

# Import all files and combine them
combined_data <- lapply(file_list, read.csv) %>%
        bind_rows()

data <- combined_data[combined_data$is_retweet == F & combined_data$is_quote == F,]
data <- dplyr::select(data, screen_name, created_at, text)

names(data)


data_tw <- left_join(data, tweets)
data_tw <- data_tw[!is.na(data_tw$id),]
names(data_tw)

names(data_tw) <- c("Twitter_account", "created_at", "text", "id",
                    "Classification.Score", "Candidate", "Party")

candidates_list <- rio::import("/Users/Fede/Desktop/PAPERS/Gtrends_personalism/Data/TWITTER Base Candidatos LA.xlsx")
candidates_list[1:6,]$Votes_perc <- c(0.4716, 0.4137, 0.0614, 0.0215, 0.0170, 0.0147)
candidates <- select(candidates_list, Country, Year, Party, Candidate, Twitter_account, Votes_perc, Date_election)

new_data <- left_join(data_tw, candidates)



results <- new_data %>%
        group_by(Candidate, Date_election, Country, Year) %>%
        summarize(
                sum_positive = sum(Classification.Score == 1),
                sum_negative = sum(Classification.Score == -1),
                ratio = sum_positive / (sum_positive + sum_negative),
                Votes_perc = Votes_perc
        ) %>%
        ungroup() 

names(results)

country <- results %>%
        group_by(Country, Candidate, Year)  %>%
        summarize(personalism = ratio)
country <- country[!duplicated(country$Candidate) & !duplicated(country$Candidate), ]

country_ag <- country %>%
        group_by(Country, Year)  %>%
        summarize(personalism = mean(personalism, na.rm=T))


country_data <- country_ag
candidate_data <- country

rio::export(candidate_data,here::here("Data", "GPT_candidates.csv"), format="csv")
rio::export(country_data,here::here("Data", "GPT_country.csv"), format="csv")
