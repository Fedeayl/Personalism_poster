#### DICTIONARY BASED CLASSIFICATION

library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
# Function to calculate the personalism and partisanship counts
calculate_personalism <- function(tweets, candidate_name, party_name) {
        
        # Define the base dictionary without candidate's name or party's name
        personalization_keywords <- c(
                "yo", "me", "mí", "mi", "mis", "mío", "mía", "mías", "míos",
                "logré", "lograré", "conseguí", "conseguiré", "estoy", "estuve", "estaré",
                "lidero", "lideraré", "guiaré", "manejo", "manejaré", "mi programa",
                "mi gobierno", "mi gestión", "mi administración","cumplí", "mi pasión", "mi entrega", 
                "mi experiencia", "mi visión", "mi liderazgo", "mi capacidad", "mi compromiso", 
                "mi dedicación", "mi esfuerzo", "mi trabajo", "mis logros", "mi responsabilidad", 
                "mi valentía", "mi honor", "mi honestidad", "el único","solo yo", 
                "represento", "yo mismo","mi voluntad", "mi coraje", "mi fuerza", "mi futuro", "mi proyecto", 
                "yo lo hice", "mi mano", "mi decisión personal", "mi compromiso personal", "como cristiano",
                "yo propongo", "mi idea", "mi propuesta", "he logrado", "he decidido", "he gestionado", 
                "mi gestión pasada", "mi historia", "mi historial", "mi récord",  "mi trayectoria", 
                "soy una mujer", "soy un hombre","como mujer", "como hombre"
        )
        
        
        partisanship_keywords <- c(
                "partido", "movimiento", "coalición", "frente", "agrupación",
                "nuestra plataforma", "nuestra organización", "nosotras",
                "nosotros", "nuestro", "nuestra", "nuestros", "nuestras", "nuestr@", "nos", "nosotrxs", 
                "nuestrxs", "somos", "creemos", "nuestra visión", "nuestra propuesta", "nuestra misión", 
                "nuestra estrategia", "nuestro proyecto", "nuestros principios", "nuestros valores",
                "nuestro éxito", "nuestro esfuerzo", "nuestra lucha", "nuestra causa", "nuestra victoria",
                "nuestro compromiso", "nuestra responsabilidad", "nuestro liderazgo", "nuestra dedicación",
                "unidos", "juntos", "todos", "en conjunto", "trabajamos juntos", "trabajamos unidos", 
                "construimos juntos", "luchamos", "representamos", "defendemos", "apoyamos", "promovemos", 
                "creamos", "nuestro legado", "nuestro futuro", "nuestro trabajo", "nuestro camino", 
                "nuestro objetivo común", "nuestro sueño", "nuestra meta", "nuestro desafío", "movimiento", 
                "militancia", "militantes", "convención", "asamblea", "simpatizantes"
        )
        
        # Include the candidate's name in the personalization keywords
        candidate_name_parts <- c(unique(tweets$Candidate), 
                                  unlist(strsplit(unique(tweets$Candidate), split = " ")),
                                  unique(tweets$Candidate_exact))
        personalization_keywords <- tolower(c(personalization_keywords, candidate_name_parts))
        
        # Include the party's name in the partisanship keywords
        party_name_parts <- c(
                unique(tweets$Party), 
                unique(tweets$Party_exact),
                if (!all(is.na(tweets$Party_Acronym))) unique(tweets$Party_Acronym) else NULL
        )
        
        partisanship_keywords <- tolower(c(partisanship_keywords, party_name_parts))
        
        # Function to count occurrences of keywords in a tweet
        count_references <- function(tweet, personalization_keywords, partisanship_keywords) {
                personalization_count <- sum(str_count(tweet, personalization_keywords))
                partisanship_count <- sum(str_count(tweet, partisanship_keywords))
                return(list(personalization_count = personalization_count, partisanship_count = partisanship_count))
        }
        
        # Apply the counting function to each tweet
        tweets <- tweets %>%
                rowwise() %>%
                mutate(references = list(count_references(text, personalization_keywords, partisanship_keywords))) %>%
                unnest_wider(references)
        
        # Aggregate the counts
        total_personalization_count <- sum(tweets$personalization_count)
        total_partisanship_count <- sum(tweets$partisanship_count)
        
        # Return the aggregated counts
        return(data.frame(candidate = candidate_name, 
                          total_personalization_count = total_personalization_count, 
                          total_partisanship_count = total_partisanship_count,
                          Personalism_score = total_personalization_count / 
                                  (total_partisanship_count+total_personalization_count)))
}




## Data for processing


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

# Only candidates with more than 100 tweets
candidate_count_100 <- filtered_data %>%
        count(Candidate, name = "count")



results <- filtered_data %>%
        group_by(Candidate, Party, Country, Date_election) %>%
        do(calculate_personalism(., .$Candidate[1], .$Party[1]))


compare <- rio::import(here::here("Data","Gtrends_Multilevel.xlsx"))
compare <- dplyr::select(compare,Candidate, Date_election, Woman, Indicador_exact, Indicador_aprox)
compare$Date_election <-  as.Date(compare$Date_election, format="%Y-%m-%d")
compare$Indicador_exact <- as.numeric(compare$Indicador_exact)
compare$Indicador_aprox <- as.numeric(compare$Indicador_aprox)
compare <- left_join(results, compare)
compare_filtered <- compare[compare$Date_election > as.Date("2010-01-01") & compare$Country != "BRASIL",]

with(compare_filtered, (cor.test(Personalism_score,Indicador_exact)))

Cor1 <- ggplot(compare_filtered, aes(x = Personalism_score, y = Indicador_exact)) +
        geom_point(color = "#215732", size = 1.2, alpha = 0.6) +       # Scatter plot with transparency
        geom_smooth(method = "lm", color = "#BA0C2F", size = 1.0, se = F) +   # Thicker regression line
        labs(title = "Correlation. Dictionary-based vs GTrends",
             x = "Dictionary-based candidate score",
             y = "G-Trends candidate score") +
        xlim(0,1)+
        theme_minimal(base_size = 12) +                                # Larger base font size
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),   # Center and bold the title
              axis.title.x = element_text(margin = margin(t = 10)),    # Add space between axis and title
              axis.title.y = element_text(margin = margin(r = 10)),
              panel.grid.major = element_line(size = 0.5, li, color = 'lightgray'), # Subtle grid
              panel.grid.minor = element_blank()) 


jpeg(filename = here::here("Figures","Dictionary_Gtrends.jpg"), 
     width = 1750, height = 1900, res = 300)
Cor1
dev.off()


GPT_candidates <- rio::import(here::here("Data", "GPT_candidates_all.csv"))
names(GPT_candidates)[4] <- "Personalism_GPT"
compare2 <- left_join(compare,GPT_candidates )

with(compare2, (cor.test(Personalism_GPT, Indicador_exact)))

names(compare2)
Cor2 <- ggplot(compare2, aes(x = Personalism_GPT, y = Indicador_exact)) +
        geom_point(color = "#215732", size = 1.2, alpha = 0.6) +       # Scatter plot with transparency
        geom_smooth(method = "lm", color = "#BA0C2F", size = 1.0, se = F) +   # Thicker regression line
        labs(title = "Correlation. GPT 4o mini score vs G-Trends",
             x = "GPT candidate score",
             y = "G-Trends candidate score") +
        xlim(0,1)+
        theme_minimal(base_size = 12) +                                # Larger base font size
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),   # Center and bold the title
              axis.title.x = element_text(margin = margin(t = 10)),    # Add space between axis and title
              axis.title.y = element_text(margin = margin(r = 10)),
              panel.grid.major = element_line(size = 0.5, li, color = 'lightgray'), # Subtle grid
              panel.grid.minor = element_blank()) 

jpeg(filename = here::here("Figures","Dictionary_GPT.jpg"), 
     width = 1750, height = 1900, res = 300)
Cor2
dev.off()


