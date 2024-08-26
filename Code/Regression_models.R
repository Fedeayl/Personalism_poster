
library(ggplot2)
library(sandwich)
library(lmtest)
library(broom)
library(ggplot2)

base <- rio::import("/Users/Fede/Desktop/PAPERS/Gtrends_personalism/Modelo/base_agregada_v4.csv")

# creo una variable laggeada 1 tiempo de protestas antigov
base$lag.antigov <- sapply(1:nrow(base), function(x) base$antigov[x-1])
base$lag.antigov <- as.numeric(base$lag.antigov)

# llevo la escala de conf_pp_mucha a base 100
base$conf_pp_mucha <- base$conf_pp_mucha * 100

# normalizo wspi basado en v-party para que varie entre 0 y 1 igual que el de GT
base$ipsp_pc_norm <- (base$ipsp_prom_coal - min(base$ipsp_prom_coal, na.rm=T)) / (max(base$ipsp_prom_coal, na.rm=T) - min(base$ipsp_prom_coal, na.rm=T))
summary(base$ipsp_pc_norm)

summary(model <- lm(pers_gt_exact ~ independ + primarias + incumbent + federal +
                              lag.antigov + conf_pp_mucha + mysvolatility + 
                              myspolar + mysln_infl1 +Internet_wb +factor(pais), data = base))

# Extract coefficients and standard errors from coeftest object
model_summary <- coeftest(model, vcov = vcovCL(model, type = "HC0", cluster = ~ base$pais))

# Manually create a data frame with the coefficients, standard errors, and confidence intervals
model_coeffs <- data.frame(
        term = rownames(model_summary),
        estimate = model_summary[, "Estimate"],
        std.error = model_summary[, "Std. Error"]
)

# Calculate the confidence intervals (95%)
model_coeffs <- model_coeffs %>%
        mutate(lower_ci = estimate - 1.96 * std.error,
               upper_ci = estimate + 1.96 * std.error,
               term = factor(term, levels = term[order(estimate)]))  # Order terms by the estimate

# List of terms to exclude from the plot
exclude_terms <- c("(Intercept)", "Internet_wb", "conf_pp_mucha", "mysvolatility", "lag.antigov","mysln_infl1")

# Filter out the factor(pais) levels and the specific terms to exclude
model_coeffs <- model_coeffs %>%
        filter(!grepl("^factor\\(pais\\)", term) & !term %in% exclude_terms)

model_coeffs <- model_coeffs %>%
        mutate(term = case_when(
                term == "independ" ~ "Independent",
                term == "primarias" ~ "Primary",
                term == "incumbent" ~ "Incumbent",
                term == "federal" ~ "Federal",
                term == "myspolar" ~ "Polarization",
                TRUE ~ term  # Keep other terms unchanged
        ))


CoefPlot <- ggplot(model_coeffs, aes(x = estimate, y = term)) +
                        geom_point(size = 3, color = "#215732") +  # Larger points with color
                        geom_vline(xintercept = 0, color = "#BA0C2F", size = 0.5) +  # Vertical line at 0
                        geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, color = "gray50") +  # Thicker error bars with color
                        labs(title = "Coefficients (95% CI) - Gtrends",
                                x = "Coefficient Estimate",
                                y = "") +
        
                        theme_minimal(base_size = 12) +  # Increase base font size for better readability
                        theme(
                                plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Center and bold title
                                axis.text = element_text(size = 10),
                                panel.grid.major = element_line(color = "gray80", size = 0.5),  # Subtle grid lines
                                panel.grid.minor = element_blank()
        )

jpeg(filename = here::here("Figures","CoefPlot_1.jpg"), 
     width = 1500, height = 1300, res = 300)
CoefPlot
dev.off()



base <- rio::import("/Users/Fede/Desktop/PAPERS/Gtrends_personalism/Modelo/base_agregada_v4.csv")
base_gpt <- rio::import(here::here("Data", "GPT_country.csv"))

names(base_gpt) <- c("pais", "anio", "personalism_gpt")
base_gpt$pais <- tools::toTitleCase(tolower(base_gpt$pais))
base_gpt$pais <- ifelse(base_gpt$pais=="Brasil", "Brazil", base_gpt$pais)
base_gpt$pais <- ifelse(base_gpt$pais=="República Dominicana", "Dominican R.", base_gpt$pais)

data <- left_join(base, base_gpt)

summary(model <- lm(personalism_gpt ~ independ + primarias + incumbent + federal +
                            lag.antigov + conf_pp_mucha + mysvolatility + 
                            myspolar + mysln_infl1 +Internet_wb, data = data))

# Extract coefficients and standard errors from coeftest object
model_summary <- coeftest(model, vcov = vcovCL(model, type = "HC0", cluster = ~ data$pais))

# Manually create a data frame with the coefficients, standard errors, and confidence intervals
model_coeffs <- data.frame(
        term = rownames(model_summary),
        estimate = model_summary[, "Estimate"],
        std.error = model_summary[, "Std. Error"]
)

# Calculate the confidence intervals (95%)
model_coeffs <- model_coeffs %>%
        mutate(lower_ci = estimate - 1.96 * std.error,
               upper_ci = estimate + 1.96 * std.error,
               term = factor(term, levels = term[order(estimate)]))  # Order terms by the estimate

# List of terms to exclude from the plot
exclude_terms <- c("(Intercept)", "Internet_wb", "conf_pp_mucha", "mysvolatility", "lag.antigov","mysln_infl1")

# Filter out the factor(pais) levels and the specific terms to exclude
model_coeffs <- model_coeffs %>%
        filter(!grepl("^factor\\(pais\\)", term) & !term %in% exclude_terms)

model_coeffs <- model_coeffs %>%
        mutate(term = case_when(
                term == "independ" ~ "Independent",
                term == "primarias" ~ "Primary",
                term == "incumbent" ~ "Incumbent",
                term == "federal" ~ "Federal",
                term == "myspolar" ~ "Polarization",
                TRUE ~ term  # Keep other terms unchanged
        ))


CoefPlot2 <- ggplot(model_coeffs, aes(x = estimate, y = term)) +
        geom_point(size = 3, color = "#215732") +  # Larger points with color
        geom_vline(xintercept = 0, color = "#BA0C2F", size = 0.5) +  # Vertical line at 0
        geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, color = "gray50") +  # Thicker error bars with color
        labs(title = "Coefficients (95% CI) - GPT score",
             x = "Coefficient Estimate",
             y = "") +
        
        theme_minimal(base_size = 12) +  # Increase base font size for better readability
        theme(
                plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Center and bold title
                axis.text = element_text(size = 10),
                panel.grid.major = element_line(color = "gray80", size = 0.5),  # Subtle grid lines
                panel.grid.minor = element_blank()
        )


jpeg(filename = here::here("Figures","CoefPlot_2.jpg"), 
     width = 1500, height = 1300, res = 300)
CoefPlot2
dev.off()

