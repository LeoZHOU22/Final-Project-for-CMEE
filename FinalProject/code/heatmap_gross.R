# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)

# Load the data
stats <- read.csv("Results/gross_photosynthesis_aquatic.csv")
plot.new()
par(mfrow=c(1,1))
ggplot(stats, aes(enzyme, model, fill= AIC)) + 
  geom_tile() +
  scale_fill_continuous(na.value = "grey90") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        panel.background = element_blank())
filtered_stats <- stats %>%
  filter(!is.na(AIC) & is.finite(AIC))
heatmap_aic <- ggplot(filtered_stats, aes(factor(enzyme), model, fill = AIC)) + 
  geom_tile() +
  scale_fill_continuous(na.value = "grey90") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.ticks.x = element_blank(),
        panel.background = element_blank()) +
  labs(fill = "AIC", x = "Enzyme ID", y = "Model", 
       title = "AIC of each model in fitting")
# Print the heatmap
print(heatmap_aic)
cln_stats <- filtered_stats
nona_stats <- cln_stats
calculate_aicw <- function(df) {
  unique_enzyme_ids <- unique(df$enzyme)
  output_df <- data.frame()
  
  for (i in seq_along(unique_enzyme_ids)) {
    enzyme_id <- unique_enzyme_ids[i]
    AIC.scores <- subset(df, enzyme == enzyme_id) %>% .$AIC
    model_names <- subset(df, enzyme == enzyme_id) %>% .$model
    names(AIC.scores) <- model_names
    aicw_df <- aicw(AIC.scores) %>% tibble::rownames_to_column("model")
    aicw_df$enzyme <- enzyme_id
    
    output_df <- bind_rows(output_df, aicw_df)
  }
  return(output_df)
}

aic_weights <- calculate_aicw(nona_stats)

model_order <- c(
  "Hinshelwood", "Johnson-Lewin", "extended_Johnson-Lewin", "simplified_Johnson_Lewin", 
  "simplified_extended_Johnson_Lewin", "Sharpe-Schoolfield", "extended_Schoolfield", 
  "simplified_Schoolfield", "simplified_extended_Schoolfield", "Ratkowsky", "Hobbs", 
  "EAAR", "Ritchie"
)

# Convert 'model' column to factor with specified levels
aic_weights <- aic_weights %>%
  mutate(model = factor(model, levels = model_order))
filtered_aic_weights <- aic_weights %>%
  filter(enzyme != 41000 & enzyme != 42000 & !is.na(w) & is.finite(w))

# Create the heatmap with increased plot size
heatmap_aic_weight <- ggplot(filtered_aic_weights, aes(factor(enzyme), model, fill = w)) + 
  geom_tile() + 
  theme_grey(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        panel.background = element_blank()) +
  labs(fill = "AIC weights", x = "Enzyme ID", y = "Model", 
       title = "AIC weight of each model in fitting") +
  scale_fill_gradient(low = "white", high = "red")

# Print the heatmap
print(heatmap_aic_weight)
ggsave("aic_weight_heatmap_gross.png", plot = heatmap_aic_weight, width = 20, height = 15)
