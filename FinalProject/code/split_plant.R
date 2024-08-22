library(dplyr)

# Function to process each subset
process_subset <- function(df, id) {
  # Filter out rows with NA in StandardisedTraitName
  filtered_data <- df %>% filter(!is.na(StandardisedTraitName))
  
  # Categorize data
  
  net_photosynthesis_data <- filtered_data %>% filter(StandardisedTraitName == "net photosynthesis rate")
  gross_photosynthesis_data <- filtered_data %>% filter(StandardisedTraitName == "gross photosynthesis rate")
  respiration_rate_data <- filtered_data %>% filter(StandardisedTraitName == "respiration rate")
  
  # Further split by habitat
  
  net_photosynthesis_terrestrial <- net_photosynthesis_data %>% filter(Habitat == "terrestrial") %>% mutate(ID = id)
  net_photosynthesis_aquatic <- net_photosynthesis_data %>% filter(Habitat %in% c("marine", "freshwater", "freshwater / terrestrial", "aquatic")) %>% mutate(ID = id)
  
  gross_photosynthesis_terrestrial <- gross_photosynthesis_data %>% filter(Habitat == "terrestrial") %>% mutate(ID = id)
  gross_photosynthesis_aquatic <- gross_photosynthesis_data %>% filter(Habitat %in% c("marine", "freshwater", "freshwater / terrestrial", "aquatic")) %>% mutate(ID = id)
  
  respiration_rate_terrestrial <- respiration_rate_data %>% filter(Habitat == "terrestrial") %>% mutate(ID = id)
  respiration_rate_aquatic <- respiration_rate_data %>% filter(Habitat %in% c("marine", "freshwater", "freshwater / terrestrial", "aquatic")) %>% mutate(ID = id)
  
  # Combine results into a list
  list(
    net_photosynthesis_terrestrial = net_photosynthesis_terrestrial,
    net_photosynthesis_aquatic = net_photosynthesis_aquatic,
    gross_photosynthesis_terrestrial = gross_photosynthesis_terrestrial,
    gross_photosynthesis_aquatic = gross_photosynthesis_aquatic,
    respiration_rate_terrestrial = respiration_rate_terrestrial,
    respiration_rate_aquatic = respiration_rate_aquatic
  )
}
# Assuming final_subsets is a named list where names are IDs
processed_subsets <- lapply(names(final_subsets), function(id) {
  process_subset(final_subsets[[id]], id)
})
# Combine all terrestrial growth rate data

all_net_photosynthesis_terrestrial <- do.call(rbind, lapply(processed_subsets, `[[`, "net_photosynthesis_terrestrial"))
all_net_photosynthesis_aquatic <- do.call(rbind, lapply(processed_subsets, `[[`, "net_photosynthesis_aquatic"))

all_gross_photosynthesis_terrestrial <- do.call(rbind, lapply(processed_subsets, `[[`, "gross_photosynthesis_terrestrial"))
all_gross_photosynthesis_aquatic <- do.call(rbind, lapply(processed_subsets, `[[`, "gross_photosynthesis_aquatic"))

all_respiration_rate_terrestrial <- do.call(rbind, lapply(processed_subsets, `[[`, "respiration_rate_terrestrial"))
all_respiration_rate_aquatic <- do.call(rbind, lapply(processed_subsets, `[[`, "respiration_rate_aquatic"))

# Display combined results

id_all_net_photosynthesis_terrestrial <- list(all_net_photosynthesis_aquatic$id)
unique(id_all_net_photosynthesis_terrestrial)

id_all_net_photosynthesis_aquatic <- list(all_gross_photosynthesis_aquatic)
unique(id_all_net_photosynthesis_aquatic)


id_all_gross_photosynthesis_terrestrial <-list(all_gross_photosynthesis_terrestrial)

id_all_gross_photosynthesis_aquatic<-list(all_gross_photosynthesis_aquatic)

id_all_respiration_rate_terrestrial<-list(all_respiration_rate_terrestrial)
id_all_respiration_rate_aquatic <-list(all_respiration_rate_aquatic)
#select ids
# Assuming id_all_net_photosynthesis_terrestrial is already defined
unique_ids <- unique(unlist(id_all_respiration_rate_terrestrial))
results_summary <- read.csv("Results/results_summary3.csv")
# Filter the original data using the unique IDs
filtered_results <- results_summary %>% filter(ID %in% unique_ids)
# Save the filtered results to a new CSV file
write.csv(filtered_results, "Results/filtered_results.csv", row.names = FALSE)


