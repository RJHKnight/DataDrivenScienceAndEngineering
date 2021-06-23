library(tidyverse)
library(patchwork)

housing <- read_csv("Data/boston.csv")

# Add dummy column for intercept
housing <- mutate(housing, dummy = 1)

# Extract Matrix
A <- as.matrix(select(housing, -MEDV))
b <- pull(housing, MEDV)

x <- lm(MEDV  ~ -1 + ., data = housing)

results <- rbind(
  data.frame(id = 1:length(b), value = b, type = "Housing value"),
  data.frame(id = 1:length(b), value = as.matrix(A) %*% coef(x), type = "Regression"))

unordered_plot <- ggplot(results, aes(id, value, colour = type)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = "bottom")

ordered_plot <- results %>% 
  # Wide format so we can arrange by true value
  pivot_wider(names_from = type, values_from = value) %>% 
  arrange(`Housing value`) %>% 
  # Re-label the id with the new order
  mutate(id = 1:n()) %>% 
  # Back to long format for plotting
  pivot_longer(names_to = "type", values_to = "value", -id) %>% 
  ggplot(aes(id, value, colour = type)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = "bottom")



# Significance Plot

# Demean and scale all except dummy
housing_norm <- mutate(housing, across(!matches("dummy|MEDV"), ~ (.x - mean(.x)) / sd(.x)))

x <- lm(MEDV ~ -1 + ., data = housing_norm)

x_coef <- coef(x)[-14]
correlation_df <- data.frame(attribute = 1:length(x_coef), correlation = x_coef)

significance_plot <- ggplot(correlation_df, aes(attribute, correlation)) + 
  geom_col()


(unordered_plot + ordered_plot) / significance_plot