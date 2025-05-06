################################################################################
# 1. LIBRARY SETUP AND PACKAGE INSTALLATION
################################################################################

# Required libraries
libraries <- c("forecast", "dplyr", "tidyr", "ggplot2", "Metrics", "modeltime", "knitr", "kableExtra")

# Install and load required packages
for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
    library(lib, character.only = TRUE)
  }
}

################################################################################
# 2. LOAD SAVED MODELS AND RESULTS
################################################################################

# Load RMSE results
global_rmse <- readRDS("global_model_rmse.rds")
local_rmse <- readRDS("local_performance_summary.rds")

# Load forecast results for error calculations
global_forecasts <- readRDS("global_model_forecasts.rds")
local_forecasts <- readRDS("local_forecasts.rds")

################################################################################
# 3. PREPARE DATA FOR COMPARISON
################################################################################

# Create comparison dataframe
comparison_df <- inner_join(
  global_rmse,
  local_rmse %>% select(ticker, local_rmse = local_rmse),
  by = "ticker"
)

# Add difference and percentage difference columns
comparison_df <- comparison_df %>%
  mutate(
    rmse_difference = local_rmse - global_rmse,
    percent_difference = (rmse_difference / global_rmse) * 100,
    better_model = case_when(
      local_rmse < global_rmse ~ "Local",
      local_rmse > global_rmse ~ "Global",
      TRUE ~ "Equal"
    )
  )

# Print detailed comparison table
cat("\nDetailed RMSE Comparison by Ticker:\n")
comparison_table <- comparison_df %>%
  arrange(percent_difference) %>%
  select(
    Ticker = ticker,
    `Global RMSE` = global_rmse,
    `Local RMSE` = local_rmse,
    `RMSE Difference` = rmse_difference,
    `% Difference` = percent_difference,
    `Better Model` = better_model
  ) %>%
  mutate(
    `Global RMSE` = round(`Global RMSE`, 4),
    `Local RMSE` = round(`Local RMSE`, 4),
    `RMSE Difference` = round(`RMSE Difference`, 4),
    `% Difference` = round(`% Difference`, 2)
  )

# Print the table with kable
print(kable(comparison_table, format = "html", caption = "RMSE Comparison by Ticker") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")))

# Save the comparison table to HTML
save_kable(comparison_table, "rmse_comparison_table.html")

################################################################################
# 4. PERFORM DIEBOLD-MARIANO TEST
################################################################################

# Function to perform Diebold-Mariano test
perform_dm_test <- function(global_errors, local_errors) {
  # Calculate squared errors
  global_squared_errors <- global_errors^2
  local_squared_errors <- local_errors^2
  
  # Calculate loss differential
  loss_diff <- global_squared_errors - local_squared_errors
  
  # Perform Diebold-Mariano test
  dm_test <- dm.test(loss_diff, alternative = "two.sided")
  
  return(dm_test)
}

# Perform test for each ticker
dm_results <- data.frame(
  ticker = character(),
  dm_statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (ticker in comparison_df$ticker) {
  tryCatch({
    # Get forecast errors for both models
    global_data <- global_forecasts %>% filter(ticker == !!ticker)
    
    # Extract local forecast data
    local_data <- local_forecasts[[ticker]]
    
    # For global model
    global_errors <- global_data$actual - global_data$forecast
    
    # For local model - handle different data structure
    if (is.list(local_data)) {
      # If local_data is a list with actual and forecast components
      local_errors <- local_data$actual - local_data$forecast
    } else if (is.numeric(local_data)) {
      # If local_data is just a vector of forecasts
      # We need to get the actual values from the test data
      test_data <- testing(splits_list[[which(unique(df_combined$ticker) == ticker)]])
      local_errors <- test_data$returns - local_data
    } else {
      stop("Unexpected format for local forecasts")
    }
    
    # Ensure same length for comparison
    min_length <- min(length(global_errors), length(local_errors))
    global_errors <- global_errors[1:min_length]
    local_errors <- local_errors[1:min_length]
    
    # Remove any NA values
    valid_indices <- !is.na(global_errors) & !is.na(local_errors)
    global_errors <- global_errors[valid_indices]
    local_errors <- local_errors[valid_indices]
    
    # Only proceed if we have enough data points
    if (length(global_errors) > 1 && length(local_errors) > 1) {
      # Perform DM test
      dm_test_result <- perform_dm_test(global_errors, local_errors)
      
      # Add results to dataframe
      dm_results <- rbind(dm_results, data.frame(
        ticker = ticker,
        dm_statistic = dm_test_result$statistic,
        p_value = dm_test_result$p.value
      ))
    } else {
      warning(sprintf("Not enough valid data points for ticker %s", ticker))
    }
  }, error = function(e) {
    warning(sprintf("Error processing ticker %s: %s", ticker, e$message))
  })
}

################################################################################
# 5. VISUALIZE RESULTS
################################################################################

# Create comparison plot
comparison_plot <- ggplot(comparison_df, aes(x = ticker)) +
  geom_bar(aes(y = global_rmse, fill = "Global Model"), stat = "identity", position = "dodge", width = 0.4) +
  geom_bar(aes(y = local_rmse, fill = "Local Model"), stat = "identity", position = "dodge", width = 0.4) +
  labs(title = "RMSE Comparison: Global vs Local Models",
       x = "Ticker",
       y = "RMSE",
       fill = "Model Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save plot
ggsave("model_comparison_plot.pdf", comparison_plot, width = 12, height = 8)

# Create percentage difference plot
percent_plot <- ggplot(comparison_df, aes(x = reorder(ticker, percent_difference), y = percent_difference)) +
  geom_bar(stat = "identity", aes(fill = percent_difference > 0)) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "green"),
                   labels = c("TRUE" = "Local Worse", "FALSE" = "Local Better")) +
  labs(title = "Percentage Difference in RMSE (Local vs Global)",
       x = "Ticker",
       y = "Percentage Difference (%)",
       fill = "Performance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save percentage plot
ggsave("rmse_percentage_difference.pdf", percent_plot, width = 12, height = 8)

################################################################################
# 6. SAVE RESULTS
################################################################################

# Save comparison results
write.csv(comparison_df, "rmse_comparison.csv", row.names = FALSE)
write.csv(dm_results, "diebold_mariano_results.csv", row.names = FALSE)

# Print summary statistics
cat("\nSummary Statistics:\n")
print(summary(comparison_df))

cat("\nDiebold-Mariano Test Results:\n")
print(dm_results)

# Calculate percentage of cases where local model outperforms global model
local_better <- mean(comparison_df$local_rmse < comparison_df$global_rmse) * 100
cat(sprintf("\nLocal model outperforms global model in %.2f%% of cases\n", local_better))

# Print significant results
cat("\nSignificant Results (p < 0.05):\n")
print(dm_results %>% filter(p_value < 0.05))

# Print detailed summary
cat("\nDetailed Summary:\n")
summary_stats <- comparison_df %>%
  group_by(better_model) %>%
  summarise(
    count = n(),
    mean_rmse_diff = mean(rmse_difference),
    mean_percent_diff = mean(percent_difference)
  )

print(summary_stats) 