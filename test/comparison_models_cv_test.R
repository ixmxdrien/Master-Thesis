################################################################################
# 1. LIBRARY SETUP AND PACKAGE INSTALLATION
################################################################################

# Required libraries for model comparison and visualization
libraries <- c("forecast", "dplyr", "tidyr", "ggplot2", "Metrics", "modeltime", "knitr", "kableExtra")

# Install and load required packages
for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
    library(lib, character.only = TRUE)
  }
}

################################################################################
# 2. LOAD SAVED RESULTS
################################################################################

# Load accuracy results for both models
global_accuracy <- readRDS("data/rds/cv_accuracy_by_ticker.rds")
local_accuracy <- readRDS("data/rds/local_accuracy_results.rds")

# Load forecast errors
global_forecast_errors <- readRDS("data/rds/cv_forecast_errors.rds")
local_forecast_errors <- readRDS("data/rds/local_forecast_errors.rds")

# Debug prints to check data structure
print("Global accuracy structure:")
print(str(global_accuracy))
print("\nLocal accuracy structure:")
print(str(local_accuracy))

################################################################################
# 3. RMSE COMPARISON
################################################################################

# Create comparison table for each stock and fold
comparison_df <- data.frame(
  Stock = character(),
  Fold = integer(),
  Global_RMSE = numeric(),
  Local_RMSE = numeric(),
  stringsAsFactors = FALSE
)

# Process each stock
stocks <- unique(local_accuracy$ticker)

# Process each stock
for (stock in stocks) {
  # Get local RMSE for this stock
  local_stock_data <- local_accuracy[local_accuracy$ticker == stock, ]
  
  # Get global RMSE for this stock
  global_stock_data <- global_accuracy[global_accuracy$ticker == stock, ]
  
  # Add rows for each fold
  for (fold in 1:2) {
    local_rmse <- local_stock_data$rmse[fold]
    global_rmse <- global_stock_data$rmse[1] # Global model has same RMSE for both folds
    
    comparison_df <- rbind(comparison_df, data.frame(
      Stock = stock,
      Fold = fold,
      Global_RMSE = global_rmse,
      Local_RMSE = local_rmse
    ))
  }
}

# Calculate which model performed better for each stock and fold
comparison_df$better_model <- ifelse(
  comparison_df$Global_RMSE < comparison_df$Local_RMSE,
  "Global",
  "Local"
)

# Create formatted table using kable
comparison_table <- kable(
  comparison_df,
  format = "html",
  caption = "RMSE Comparison by Stock and Fold",
  digits = 6
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) %>%
  add_header_above(c(" " = 2, "RMSE Values" = 2, " " = 1))

# Display the table
print(comparison_table)

# Save the comparison table
saveRDS(comparison_df, "data/rds/rmse_comparison_table.rds")

################################################################################
# 4. DIEBOLD-MARIANO TEST
################################################################################

# Function to perform Diebold-Mariano test
perform_dm_test <- function(global_errors, local_errors, global_dates, local_dates, stock, fold) {
  # Create data frames with dates and errors
  global_df <- data.frame(date = global_dates, error = global_errors)
  local_df <- data.frame(date = local_dates, error = local_errors)
  
  # Print date ranges for debugging
  cat(sprintf("\nDate ranges for %s (Fold %d):\n", stock, fold))
  cat(sprintf("Global model: %s to %s\n", 
              min(global_dates), max(global_dates)))
  cat(sprintf("Local model: %s to %s\n", 
              min(local_dates), max(local_dates)))
  
  # Merge on dates to ensure we compare the same periods
  merged_df <- inner_join(global_df, local_df, by = "date", suffix = c("_global", "_local"))
  
  # Remove any NA values
  merged_df <- na.omit(merged_df)
  
  # Print information about the merge
  cat(sprintf("Number of observations after merge: %d\n", nrow(merged_df)))
  
  # Check if we have enough observations (minimum 30 for statistical significance)
  if (nrow(merged_df) < 30) {
    return(data.frame(
      DM_statistic = NA,
      p_value = NA,
      conclusion = "Insufficient observations (minimum 30 required)",
      n_observations = nrow(merged_df),
      date_range_start = min(merged_df$date),
      date_range_end = max(merged_df$date)
    ))
  }
  
  # Calculate squared errors
  global_squared_errors <- merged_df$error_global^2
  local_squared_errors <- merged_df$error_local^2
  
  # Set h to 1 for one-step ahead forecasts
  h <- 1
  
  # Perform DM test
  dm_test <- dm.test(global_squared_errors, local_squared_errors, 
                     alternative = "two.sided",
                     h = h)
  
  return(data.frame(
    DM_statistic = dm_test$statistic,
    p_value = dm_test$p.value,
    conclusion = ifelse(dm_test$p.value < 0.05, 
                       ifelse(dm_test$statistic > 0, "Local model significantly better", 
                              "Global model significantly better"),
                       "No significant difference"),
    n_observations = nrow(merged_df),
    date_range_start = min(merged_df$date),
    date_range_end = max(merged_df$date)
  ))
}

# Create results dataframe for DM tests
dm_results <- data.frame(
  Stock = character(),
  Fold = integer(),
  DM_statistic = numeric(),
  p_value = numeric(),
  conclusion = character(),
  n_observations = integer(),
  date_range_start = as.Date(character()),
  date_range_end = as.Date(character()),
  stringsAsFactors = FALSE
)

# Process each stock
stocks <- unique(local_forecast_errors$ticker)

# Perform DM test for each stock and fold
for (stock in stocks) {
  for (fold in 1:2) {
    # Get forecast errors and dates for this stock and fold
    global_data <- global_forecast_errors %>%
      filter(ticker == stock & fold$id == "Slice1") %>%
      select(date, forecast_error)
    
    local_data <- local_forecast_errors %>%
      filter(ticker == stock & fold == !!fold) %>%
      select(date, forecast_error)
    
    # Print information about the comparison
    cat(sprintf("\nComparing %s (Fold %d):\n", stock, fold))
    cat(sprintf("Number of global errors: %d\n", nrow(global_data)))
    cat(sprintf("Number of local errors: %d\n", nrow(local_data)))
    
    # Perform DM test
    dm_result <- perform_dm_test(
      global_errors = global_data$forecast_error,
      local_errors = local_data$forecast_error,
      global_dates = global_data$date,
      local_dates = local_data$date,
      stock = stock,
      fold = fold
    )
    
    # Add results to dataframe
    dm_results <- rbind(dm_results, data.frame(
      Stock = stock,
      Fold = fold,
      DM_statistic = dm_result$DM_statistic,
      p_value = dm_result$p_value,
      conclusion = dm_result$conclusion,
      n_observations = dm_result$n_observations,
      date_range_start = dm_result$date_range_start,
      date_range_end = dm_result$date_range_end
    ))
  }
}

# Create formatted table for DM test results
dm_table <- kable(
  dm_results,
  format = "html",
  caption = "Diebold-Mariano Test Results by Stock and Fold",
  digits = 4
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

# Display DM test results
print(dm_table)

# Save DM test results
saveRDS(dm_results, "data/rds/dm_test_results.rds")

################################################################################
# 5. VISUALIZATION OF RESULTS
################################################################################

# Create boxplot for RMSE comparison
rmse_boxplot <- ggplot(
  data = bind_rows(
    data.frame(value = rep(global_accuracy$rmse[1], nrow(local_accuracy)), model = "Global"),
    data.frame(value = local_accuracy$rmse, model = "Local")
  ),
  aes(x = model, y = value, fill = model)
) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "RMSE Distribution Comparison",
    x = "Model",
    y = "RMSE"
  ) +
  theme(legend.position = "none")

# Save plot
ggsave("data/plots/rmse_boxplot.png", rmse_boxplot, width = 8, height = 6)

################################################################################
# 6. FINAL SUMMARY
################################################################################

# Create final summary
final_summary <- list(
  comparison_table = comparison_table,
  dm_test_results = dm_table,
  overall_better_model = names(which.max(table(comparison_df$better_model)))
)

# Save final summary
saveRDS(final_summary, "data/rds/final_rmse_summary.rds")

# Print overall conclusion
cat("\nOverall Conclusion:\n")
cat("Based on RMSE comparison, the", final_summary$overall_better_model, "model performed better overall.\n")
cat("See the detailed results in the comparison table and DM test results above.\n") 