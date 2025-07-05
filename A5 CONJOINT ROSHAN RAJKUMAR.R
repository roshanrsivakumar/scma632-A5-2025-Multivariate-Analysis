# COMPLETE PIZZA CONJOINT ANALYSIS IN R
library(tidyverse)
library(broom)
library(ggplot2)
library(ggrepel)

# 1. Load and Prepare Data
pizza_data <- read_csv("pizza_data.csv") %>%
  mutate(across(-ranking, as.factor),
         profile_id = row_number())

# 2. Conjoint Analysis Function
run_conjoint_analysis <- function(data) {
  # Set sum contrasts for effects coding
  options(contrasts = c("contr.sum", "contr.poly"))
  
  # Fit linear model
  model <- lm(ranking ~ brand + price + weight + crust + cheese + size + toppings + spicy,
              data = data)
  
  # Extract and process coefficients
  coeffs <- tidy(model) %>%
    filter(term != "(Intercept)") %>%
    separate(term, into = c("attribute", "level"), sep = "(?<=[a-z])(?=[0-9])", fill = "right") %>%
    mutate(level = ifelse(is.na(level), "Reference", level))
  
  # Calculate importance scores
  importance <- coeffs %>%
    group_by(attribute) %>%
    summarise(range = max(estimate) - min(estimate)) %>%
    mutate(importance = (range / sum(range)) * 100) %>%
    arrange(desc(importance))
  
  # Calculate complete part-worths (including reference levels)
  part_worths <- coeffs %>%
    group_by(attribute) %>%
    mutate(last_level = -sum(estimate[level != "Reference"]),
           utility = ifelse(level == "Reference", last_level, estimate)) %>%
    ungroup() %>%
    select(attribute, level, utility)
  
  # Create utility lookup tables
  utility_lookup <- part_worths %>%
    pivot_wider(names_from = attribute, values_from = utility)
  
  # Calculate total utility for each profile
  profiles <- data %>%
    mutate(total_utility = 
             utility_lookup$brand[brand] +
             utility_lookup$price[price] +
             utility_lookup$weight[weight] +
             utility_lookup$crust[crust] +
             utility_lookup$cheese[cheese] +
             utility_lookup$size[size] +
             utility_lookup$toppings[toppings] +
             utility_lookup$spicy[spicy]) %>%
    arrange(desc(total_utility))
  
  return(list(
    model = model,
    coefficients = coeffs,
    importance = importance,
    part_worths = part_worths,
    utility_lookup = utility_lookup,
    profiles = profiles
  ))
}

# 3. Run Analysis
results <- run_conjoint_analysis(pizza_data)

# 4. Visualization Functions
plot_attribute_importance <- function(importance) {
  ggplot(importance, aes(x = reorder(attribute, importance), y = importance)) +
    geom_col(fill = "#3498db") +
    geom_text(aes(label = sprintf("%.1f%%", importance)), 
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(title = "Relative Importance of Pizza Attributes",
         x = NULL, y = "Importance (%)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank())
}

plot_part_worths <- function(part_worths) {
  ggplot(part_worths, aes(x = reorder(level, utility), y = utility)) +
    geom_col(fill = "#e74c3c", alpha = 0.8) +
    facet_wrap(~attribute, scales = "free_x", ncol = 3) +
    labs(title = "Part-Worth Utilities by Attribute Level",
         x = "Level", y = "Utility") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_rect(fill = "#f5f5f5"))
}

# 5. Generate Plots
importance_plot <- plot_attribute_importance(results$importance)
part_worth_plot <- plot_part_worths(results$part_worths)

# 6. Display Key Results
cat("=== CONJOINT ANALYSIS RESULTS ===\n\n")

# Model summary
cat("MODEL FIT SUMMARY:\n")
print(summary(results$model))

# Attribute importance
cat("\nATTRIBUTE IMPORTANCE:\n")
print(results$importance, n = Inf)

# Top profiles
cat("\nTOP 3 PIZZA PROFILES:\n")
print(results$profiles %>% select(-profile_id) %>% head(3))

# Worst profiles
cat("\nWORST 3 PIZZA PROFILES:\n")
print(results$profiles %>% select(-profile_id) %>% tail(3))
