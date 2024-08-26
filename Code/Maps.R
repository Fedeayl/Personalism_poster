library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

base <- rio::import("/Users/Fede/Desktop/PAPERS/Gtrends_personalism/Modelo/base_agregada_v4.csv")

base_2020 <- base[base$anio == 2021 | base$anio == 2021 | 
                          base$anio == 2020 | base$anio == 2019
                  | base$anio == 2018,]
base_2020 <- select(base_2020, pais, pers_gt_exact)
base_2020 <- base_2020[-2,]



world <- ne_countries(scale = "medium", returnclass = "sf")

latin_america <- world %>% 
        filter(region_un == "Americas", 
               subregion %in% c("South America", "Central America", "Caribbean"))xxa

# Correct country names to match those in the map data
base_2020$pais[base_2020$pais == "Dominican R."] <- "Dominican Republic"

latin_america <- latin_america %>%
        left_join(base_2020, by = c("name" = "pais"))

Map1 <- ggplot(data = latin_america) +
        geom_sf(aes(fill = pers_gt_exact), color = NA) +  # Remove grid lines and borders
        scale_fill_gradient(low = "#D7D2CB", high = "#215732", na.value = "darkgrey") +  # Custom colors
        coord_sf(clip = "off") +  # Ensure nothing is clipped outside the map area
        theme_void() +  # Remove all background and grid elements
        theme(
                legend.position = "bottom",  # Move the legend to the bottom
                legend.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Bold legend title
                legend.text = element_text(size = 10),  # Larger legend text
                plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(t = 20, b = 10)),  # Centered and bold title
                plot.margin = margin(20, 20, 20, 20)  # Add some margin around the plot
        ) +
        labs(
                title = "Personalism in Latin America",  # Title of the map
                fill = "Personalism Score"  # Legend title
        )

jpeg(filename = here::here("Figures","Map_gtrends.jpg"), 
     width = 1500, height = 2200, res = 300)
Map1
dev.off()






base_gpt <- rio::import(here::here("Data", "GPT_country.csv"))

base_gpt <- base_gpt[base_gpt$Year == 2022 | base_gpt$Year == 2021 | base_gpt$Year == 2021 | 
                             base_gpt$Year == 2020 | base_gpt$Year == 2019
                                | base_gpt$Year == 2018 | base_gpt$Year == 2017,]
base_gpt <- select(base_gpt, Country, personalism)
base_gpt <- base_gpt[-c(2,5,8,13),]


base_gpt$Country <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Ecuador",
                      "El Salvador", "Guatemala", "Honduras", "Mexico", "Panama", "Paraguay",
                      "Peru",  "Dominican Republic", "Uruguay","Venezuela")

world <- ne_countries(scale = "medium", returnclass = "sf")

latin_america <- world %>% 
        filter(region_un == "Americas", 
               subregion %in% c("South America", "Central America", "Caribbean"))

# Correct country names to match those in the map data

latin_america <- latin_america %>%
        left_join(base_gpt, by = c("name" = "Country"))

Map2 <- ggplot(data = latin_america) +
        geom_sf(aes(fill = personalism), color = NA) +  # Remove grid lines and borders
        scale_fill_gradient(low = "#D7D2CB", high = "#215732", na.value = "darkgrey") +  # Custom colors
        coord_sf(clip = "off") +  # Ensure nothing is clipped outside the map area
        theme_void() +  # Remove all background and grid elements
        theme(
                legend.position = "bottom",  # Move the legend to the bottom
                legend.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Bold legend title
                legend.text = element_text(size = 10),  # Larger legend text
                plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(t = 20, b = 10)),  # Centered and bold title
                plot.margin = margin(20, 20, 20, 20)  # Add some margin around the plot
        ) +
        labs(
                title = "Personalism in Latin America",  # Title of the map
                fill = "Personalism Score"  # Legend title
        )

jpeg(filename = here::here("Figures","Map_gpt.jpg"), 
     width = 1500, height = 2200, res = 300)
Map2
dev.off()




