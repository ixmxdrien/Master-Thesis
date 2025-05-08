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
# 2. LOAD SAVED MODELS AND RESULTS
################################################################################

# Load RMSE results for both models
global_rmse <- readRDS("data/rds/global_model_rmse.rds")
local_rmse <- readRDS("data/rds/local_performance_summary.rds")

# Load forecast results for error calculations
global_forecasts <- readRDS("data/rds/global_model_forecasts.rds")
local_forecasts <- readRDS("data/rds/local_forecasts.rds")

# Load forecast errors for both models
global_errors <- readRDS("data/rds/forecast_errors.rds")
local_errors <- readRDS("data/rds/local_forecast_errors.rds")



################################################################################
# 3. RMSE COMPARISON
################################################################################

# Create comparison dataframe by joining global and local RMSE results
comparison_df <- inner_join(
  global_rmse,
  local_rmse %>% select(ticker, local_rmse = local_rmse),
  by = "ticker"
)

# Calculate differences and determine better model
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

# Create formatted comparison table
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

# Display and save comparison table
print(kable(comparison_table, format = "html", caption = "RMSE Comparison by Ticker") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")))

save_kable(comparison_table, "data/rmse_comparison_table.html")



################################################################################
# 4. DIEBOLD-MARIANO TEST IMPLEMENTATION
################################################################################

# Function to perform Diebold-Mariano test for each ticker
perform_dm_test <- function(global_errors, local_errors, ticker) {
  # Filter errors for specific ticker
  global_ticker <- global_errors %>% filter(ticker == !!ticker)
  local_ticker <- local_errors %>% filter(ticker == !!ticker)
  
  # Convert dates to character strings for comparison
  global_ticker <- global_ticker %>%
    mutate(date_str = as.character(.index))
  
  local_ticker <- local_ticker %>%
    mutate(date_str = as.character(date))
  
  # Find common dates between both models
  common_dates <- intersect(global_ticker$date_str, local_ticker$date_str)
  
  if (length(common_dates) < 2) {
    return(data.frame(
      ticker = ticker,
      dm_statistic = NA,
      p_value = NA,
      conclusion = "Insufficient data",
      n_observations = length(common_dates)
    ))
  }
  
  # Align errors on common dates
  global_aligned <- global_ticker %>% 
    filter(date_str %in% common_dates) %>% 
    arrange(date_str) %>% 
    select(date_str, e1_t)
  
  local_aligned <- local_ticker %>% 
    filter(date_str %in% common_dates) %>% 
    arrange(date_str) %>% 
    select(date_str, e1_t)
  
  # Verify date alignment
  if (!all(global_aligned$date_str == local_aligned$date_str)) {
    return(data.frame(
      ticker = ticker,
      dm_statistic = NA,
      p_value = NA,
      conclusion = "Date alignment issue",
      n_observations = nrow(global_aligned)
    ))
  }
  
  # Extract errors excluding NA values
  global_errors <- global_aligned$e1_t[!is.na(global_aligned$e1_t)]
  local_errors <- local_aligned$e1_t[!is.na(local_aligned$e1_t)]
  
  # Calculate h parameter based on data size
  n_obs <- length(global_errors)
  h <- max(1, min(1, floor(n_obs / 2)))
  
  if (n_obs < 2) {
    return(data.frame(
      ticker = ticker,
      dm_statistic = NA,
      p_value = NA,
      conclusion = "Insufficient data",
      n_observations = n_obs
    ))
  }
  
  # Perform Diebold-Mariano test
  dm_result <- dm.test(global_errors, local_errors, alternative = "two.sided", h = h)
  
  # Interpret results
  conclusion <- case_when(
    dm_result$p.value < 0.05 & dm_result$statistic > 0 ~ "Local model significantly better",
    dm_result$p.value < 0.05 & dm_result$statistic < 0 ~ "Global model significantly better",
    TRUE ~ "No significant difference"
  )
  
  return(data.frame(
    ticker = ticker,
    dm_statistic = dm_result$statistic,
    p_value = dm_result$p.value,
    conclusion = conclusion,
    n_observations = n_obs
  ))
}

# Perform DM test for each ticker
dm_results <- data.frame()
for (ticker in unique(global_errors$ticker)) {
  result <- perform_dm_test(global_errors, local_errors, ticker)
  dm_results <- bind_rows(dm_results, result)
}

# Save DM test results
saveRDS(dm_results, "data/rds/dm_test_results.rds")



################################################################################
# 6. FINAL SUMMARY
################################################################################

# Create final summary combining RMSE and DM test results
final_summary <- comparison_df %>%
  left_join(dm_results, by = "ticker") %>%
  select(
    Ticker = ticker,
    `Global RMSE` = global_rmse,
    `Local RMSE` = local_rmse,
    `RMSE Difference` = rmse_difference,
    `% Difference` = percent_difference,
    `DM Statistic` = dm_statistic,
    `DM p-value` = p_value,
    `DM Conclusion` = conclusion
  ) %>%
  mutate(
    `Global RMSE` = round(`Global RMSE`, 4),
    `Local RMSE` = round(`Local RMSE`, 4),
    `RMSE Difference` = round(`RMSE Difference`, 4),
    `% Difference` = round(`% Difference`, 2),
    `DM Statistic` = round(`DM Statistic`, 4),
    `DM p-value` = round(`DM p-value`, 4)
  )

# Display and save final summary
print(kable(final_summary, format = "html", caption = "Final Model Comparison Summary") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")))

saveRDS(final_summary, "data/rds/final_comparison_summary.rds")
#save_kable(final_summary, "data/html/final_comparison_summary.html")
