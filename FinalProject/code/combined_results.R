results_population <- read.csv("Results/growth.csv")
result_respiration <- read.csv("Results/respiration.csv")
result_gross <- read.csv("Results/gross_photosynthesis_aquatic.csv")
result_fungi <- read.csv("Results/fungi.csv")
result_enzyme <- read.csv("Results/enzyme.csv")
combined_data_result <- rbind(results_population, result_respiration, result_gross, result_fungi, result_enzyme)
# Split the data frame by the 'model' column
model_split <- split(combined_data_result, combined_data_result$model)

# The result is a list of data frames, one for each model

growth_split <- split(population_growth_rate_data,population_growth_rate_data$phylum)

# Loop through each element in the growth_split list
for (name in names(growth_split)) {
  cat("Unique IDs in", name, "subset:\n")
  print(unique(growth_split[[name]]$id))
  cat("\n") # Add an empty line for better readability
}
# Create an empty list to store the unique IDs
unique_ids_list <- list()

# Loop through each element in the growth_split list and store the unique IDs
for (name in names(growth_split)) {
  unique_ids_list[[name]] <- unique(growth_split[[name]]$id)
}

# Now you can access the unique IDs for each subset from unique_ids_list
# Create a data frame to store the results
result_df <- data.frame(Model = character(), UniqueIDs = I(list()))

# Loop through each element in the growth_split list and store the unique IDs
for (name in names(growth_split)) {
  result_df <- rbind(result_df, data.frame(Model = name, UniqueIDs = I(list(unique(growth_split[[name]]$id)))))
}

# View the result
print(result_df)
# Create an empty list to store the count of unique IDs
unique_id_counts <- list()

# Loop through each element in the growth_split list and count the unique IDs
for (name in names(growth_split)) {
  unique_id_counts[[name]] <- length(unique(growth_split[[name]]$id))
}

# Convert the list to a data frame for better readability
unique_id_counts_df <- data.frame(Model = names(unique_id_counts), UniqueIDCount = unlist(unique_id_counts))

# Print the result
print(unique_id_counts_df)

