# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)

# Load the data
stats <- read.csv("Results/enzyme.csv")
plot.new()
par(mfrow=c(1,1))

ggplot(stats, aes(enzyme, model, fill= AIC)) + 
  geom_tile() +
  scale_fill_continuous(na.value = "grey90") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        panel.background = element_blank())
remove_list <- c("AMA001", "GAC001", "GLB001", "HEP001", "SPS001", "XIS001")
cln_stats <- stats %>% filter(!enzyme %in% remove_list)
ggplot(cln_stats, aes(enzyme, model, fill= AIC)) + 
  geom_tile() +
  theme_grey(base_size = 18)+
  scale_fill_continuous(na.value = "grey80") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 12),
        axis.ticks.x=element_blank(),
        panel.background = element_blank()) +
  scale_fill_gradient(low = "red", high = "white")+
  labs(x="Enzyme ID",y="Model")

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

# Create the heatmap
heatmap_aic_weight <- ggplot(aic_weights, aes(enzyme, model, fill= w)) + 
  geom_tile() + 
  theme_grey(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.ticks.x = element_blank(),
        panel.background = element_blank()) +
  labs(fill = "AIC weights", x = "Enzyme ID", y = "Model", 
       title = "AIC weight of each model in fitting") +
  scale_fill_gradient(low = "white", high = "red")

# Save the heatmap
ggsave("aic_weight_heatmap_enzyme.png", plot = heatmap_aic_weight, width = 12, height = 10)
