library(ggplot2)
library(dplyr)
library(minpack.lm)
library(nls.multstart)
library(rTPC)
library(broom)
library(MuMIn)
library(geiger)
library(tidyr)

setwd("~/Documents/FinalProject")
source("code/TPC_fitting_functions.R")
enzyme <- selected_subset_fungal
selected_subset_enzyme <- enzymeData%>%
  dplyr::select(trait_value = trait_value , temp = temperature, id)
# Load functions all 7 functions from "mech_model_functions" before running multi_fit()

multi_fit <- function(df, model_func, model_name) {
  # Load necessary libraries
  library(dplyr)
  
  # Initialize an empty data frame to store the statistics
  stats <- data.frame(model = character(), 
                      enzyme = character(), 
                      Rsquared = numeric(), 
                      AIC = numeric(),
                      BIC = numeric(),
                      stringsAsFactors = FALSE)
  
  # Get distinct enzyme ids
  enz_id <- df %>%
    distinct(id)
  
  # Get the dataframe to fit
  tpc_to_fit <- df 
  
  unique_enzyme_ids <- unique(tpc_to_fit$id)
  num_enzymes <- length(unique_enzyme_ids)
  
  par(mfrow = c(6, 5), mar = c(2, 2, 1, 1), oma = c(0, 0, 2, 0))
  
  # Loop through each unique enzyme id
  for (i in 1:num_enzymes) {
    enzyme_id <- unique_enzyme_ids[i]
    enzyme_data <- tpc_to_fit %>%
      filter(id == enzyme_id) %>%
      dplyr::select(temp, trait_value)  # Explicitly specify dplyr::select
    
    fit <- NULL
    
    # Skip fitting if fitting function produces some sort of error
    tryCatch({
      fit <- model_func(enzyme_data)
    }, error = function(e) {
      warning(paste("Error fitting enzyme", enzyme_id, ":", e$message))
    })
    
    if (!is.null(fit)) {
      # Calculate AIC and BIC
      aic_val <- AIC(fit)
      bic_val <- BIC(fit)
      
      # Calculate R-squared value
      predictions <- predict(fit, enzyme_data)
      r_squared <- 1 - sum((log(enzyme_data$trait_value) - predictions)^2) / sum((log(enzyme_data$trait_value) - mean(log(enzyme_data$trait_value)))^2)
      
      # Update stats dataframe
      stats <- rbind(stats, data.frame(model = model_name, enzyme = enzyme_id, Rsquared = r_squared, AIC = aic_val, BIC = bic_val, stringsAsFactors = FALSE))
      
      print(paste("Fit for", enzyme_id, "is completed.", i, "out of", num_enzymes))
      
      temp_pts <- data.frame(temp = seq(min(enzyme_data$temp), max(enzyme_data$temp), 0.5))
      preds <- predict(fit, temp_pts)
    }
  }
  
  return(stats)
}


# List of models and their corresponding fitting functions
models <- list(
  list(name = "Hinshelwood", func = fit_Hinshelwood_4_pars),
  list(name = "Johnson-Lewin", func = fit_Johnson_Lewin_4_pars),
  list(name = "extended_Johnson-Lewin", func = fit_extended_Johnson_Lewin_5_pars),
  list(name = "simplified_Johnson_Lewin", func = fit_simplified_Johnson_Lewin_4_pars),
  list(name = "simplified_extended_Johnson_Lewin", func = fit_simplified_extended_Johnson_Lewin_5_pars),
  list(name = "Sharpe-Schoolfield", func = fit_Sharpe_Schoolfield_6_pars),
  list(name = "extended_Schoolfield", func = fit_extended_Sharpe_Schoolfield_7_pars),
  list(name = "simplified_Schoolfield", func = fit_simplified_Sharpe_Schoolfield_6_pars),
  list(name = "simplified_extended_Schoolfield", func = fit_simplified_extended_Sharpe_Schoolfield_7_pars),
  list(name = "Ratkowsky", func = fit_Ross_Ratkowsky_5_pars),
  list(name = "Hobbs", func = fit_Hobbs_4_pars),
  list(name = "EAAR", func = fit_Enzyme_Assisted_Arrhenius_5_pars),
  list(name = "Ritchie", func = fit_Ritchie_4_pars)
)

# Data to be used for fitting
data_to_fit <- enzyme  # Make sure to use the correct dataframe name

# Initialize an empty list to store results
results <- list()

# Loop over the models and apply the multi_fit function
for (model in models) {
  result <- multi_fit(df = data_to_fit, model_func = model$func, model_name = model$name)
  results[[model$name]] <- result
}

# Combine all results into a single data frame
all_results <- do.call(rbind, results)

# Print the combined results
print(all_results)

write.csv(all_results, "Results/fungi.csv", row.names = FALSE)




