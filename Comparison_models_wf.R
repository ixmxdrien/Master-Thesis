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

# Print structure of RMSE data
cat("\nStructure of global_rmse:\n")
str(global_rmse)
print(head(global_rmse))

cat("\nStructure of local_rmse:\n")
str(local_rmse)
print(head(local_rmse))

# Load forecast errors for both models
global_errors <- readRDS("data/rds/forecast_errors.rds")
local_errors <- readRDS("data/rds/local_forecast_errors.rds")

# Print structure of error data
cat("\nStructure of global_errors:\n")
str(global_errors)
print(head(global_errors))

cat("\nStructure of local_errors:\n")
str(local_errors)
print(head(local_errors))

################################################################################
# 3. RMSE COMPARISON
################################################################################

# Create comparison dataframe by joining global and local RMSE results
comparison_df <- inner_join(
  global_rmse,
  local_rmse,
  by = "ticker"
)

# Create a more intuitive comparison table
comparison_table <- comparison_df %>%
  mutate(
    # Calculate differences for each fold
    fold1_diff = local_rmse_fold1 - global_rmse_fold1,
    fold2_diff = local_rmse_fold2 - global_rmse_fold2,
    # Determine better model for each fold
    fold1_better = case_when(
      fold1_diff < 0 ~ "Local",
      fold1_diff > 0 ~ "Global",
      TRUE ~ "Equal"
    ),
    fold2_better = case_when(
      fold2_diff < 0 ~ "Local",
      fold2_diff > 0 ~ "Global",
      TRUE ~ "Equal"
    ),
    # Calculate percentage improvements
    fold1_improvement = (fold1_diff / global_rmse_fold1) * 100,
    fold2_improvement = (fold2_diff / global_rmse_fold2) * 100
  ) %>%
  select(
    Ticker = ticker,
    # Fold 1 results
    `Global RMSE (Fold 1)` = global_rmse_fold1,
    `Local RMSE (Fold 1)` = local_rmse_fold1,
    `Fold 1 Diff` = fold1_diff,
    `Fold 1 % Imp` = fold1_improvement,
    `Fold 1 Better` = fold1_better,
    # Fold 2 results
    `Global RMSE (Fold 2)` = global_rmse_fold2,
    `Local RMSE (Fold 2)` = local_rmse_fold2,
    `Fold 2 Diff` = fold2_diff,
    `Fold 2 % Imp` = fold2_improvement,
    `Fold 2 Better` = fold2_better
  ) %>%
  mutate(
    # Round numeric columns
    across(where(is.numeric), ~round(., 4)),
    # Format percentage columns
    `Fold 1 % Imp` = round(`Fold 1 % Imp`, 2),
    `Fold 2 % Imp` = round(`Fold 2 % Imp`, 2)
  )

# Display comparison table with conditional formatting
print(kable(comparison_table, format = "html", caption = "Model Comparison by Fold") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
      row_spec(which(comparison_table$`Fold 1 Better` == "Local"), background = "#e6ffe6") %>%
      row_spec(which(comparison_table$`Fold 1 Better` == "Global"), background = "#ffe6e6") %>%
      row_spec(which(comparison_table$`Fold 2 Better` == "Local"), background = "#e6ffe6") %>%
      row_spec(which(comparison_table$`Fold 2 Better` == "Global"), background = "#ffe6e6"))

# Save comparison table
save_kable(comparison_table, "data/rmse_comparison_table_wf.html")

# Print summary statistics
cat("\nSummary of Model Performance:\n")
cat("\nFold 1 Summary:\n")
fold1_summary <- comparison_df %>%
  summarise(
    local_better = sum(local_rmse_fold1 < global_rmse_fold1),
    global_better = sum(local_rmse_fold1 > global_rmse_fold1),
    equal = sum(local_rmse_fold1 == global_rmse_fold1),
    avg_improvement = mean((local_rmse_fold1 - global_rmse_fold1) / global_rmse_fold1 * 100)
  )
print(fold1_summary)

cat("\nFold 2 Summary:\n")
fold2_summary <- comparison_df %>%
  summarise(
    local_better = sum(local_rmse_fold2 < global_rmse_fold2),
    global_better = sum(local_rmse_fold2 > global_rmse_fold2),
    equal = sum(local_rmse_fold2 == global_rmse_fold2),
    avg_improvement = mean((local_rmse_fold2 - global_rmse_fold2) / global_rmse_fold2 * 100)
  )
print(fold2_summary)

################################################################################
# 4. DIEBOLD-MARIANO TEST IMPLEMENTATION
################################################################################

# Function to perform Diebold-Mariano test for each ticker and fold
perform_dm_test <- function(global_errors, local_errors, ticker, fold) {
  # Filter errors for specific ticker and fold
  global_ticker <- global_errors %>% filter(ticker == !!ticker)
  local_ticker <- local_errors %>% filter(ticker == !!ticker, fold == !!fold)
  
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
      fold = fold,
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
      fold = fold,
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
      fold = fold,
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
    fold = fold,
    dm_statistic = dm_result$statistic,
    p_value = dm_result$p.value,
    conclusion = conclusion,
    n_observations = n_obs
  ))
}

# Perform DM test for each ticker and fold
dm_results <- data.frame()
for (ticker in unique(global_errors$ticker)) {
  for (fold in c("Fold 1", "Fold 2")) {
    result <- perform_dm_test(global_errors, local_errors, ticker, fold)
    dm_results <- bind_rows(dm_results, result)
  }
}

# Format DM results for display
dm_table <- dm_results %>%
  mutate(
    # Round numeric values
    dm_statistic = round(dm_statistic, 4),
    p_value = round(p_value, 4),
    # Format conclusion with color indicators
    conclusion = case_when(
      conclusion == "Local model significantly better" ~ "Local ✓",
      conclusion == "Global model significantly better" ~ "Global ✓",
      conclusion == "No significant difference" ~ "No difference",
      TRUE ~ conclusion
    )
  ) %>%
  select(
    Ticker = ticker,
    Fold = fold,
    `DM Statistic` = dm_statistic,
    `P-value` = p_value,
    `Conclusion` = conclusion,
    `N Observations` = n_observations
  )

# Display DM results table with conditional formatting
print(kable(dm_table, format = "html", caption = "Diebold-Mariano Test Results by Fold") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
      row_spec(which(dm_table$Conclusion == "Local ✓"), background = "#e6ffe6") %>%
      row_spec(which(dm_table$Conclusion == "Global ✓"), background = "#ffe6e6"))

# Save DM test results
saveRDS(dm_results, "data/rds/dm_test_results_wf.rds")
save_kable(dm_table, "data/html/dm_test_results_wf.html")

# Print summary of DM test results
cat("\nSummary of Diebold-Mariano Test Results:\n")
dm_summary <- dm_results %>%
  group_by(fold) %>%
  summarise(
    local_better = sum(conclusion == "Local model significantly better", na.rm = TRUE),
    global_better = sum(conclusion == "Global model significantly better", na.rm = TRUE),
    no_difference = sum(conclusion == "No significant difference", na.rm = TRUE),
    insufficient_data = sum(conclusion %in% c("Insufficient data", "Date alignment issue"), na.rm = TRUE)
  )
print(kable(dm_summary, format = "html", caption = "Summary of DM Test Results by Fold") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")))

################################################################################
# 5. FINAL SUMMARY
################################################################################

# Create final summary combining RMSE and DM test results
final_summary <- comparison_df %>%
  left_join(
    dm_results %>%
      group_by(ticker) %>%
      summarise(
        dm_statistic_avg = mean(dm_statistic, na.rm = TRUE),
        dm_p_value_avg = mean(p_value, na.rm = TRUE),
        conclusion = case_when(
          all(conclusion == "Local model significantly better") ~ "Local model significantly better",
          all(conclusion == "Global model significantly better") ~ "Global model significantly better",
          TRUE ~ "Mixed results"
        )
      ),
    by = "ticker"
  ) %>%
  select(
    Ticker = ticker,
    `Global RMSE` = global_rmse,
    `Local RMSE Fold 1` = local_rmse_fold1,
    `Local RMSE Fold 2` = local_rmse_fold2,
    `Local RMSE Avg` = local_rmse_avg,
    `RMSE Difference` = rmse_difference,
    `% Difference` = percent_difference,
    `DM Statistic Avg` = dm_statistic_avg,
    `DM p-value Avg` = dm_p_value_avg,
    `DM Conclusion` = conclusion
  ) %>%
  mutate(
    `Global RMSE` = round(`Global RMSE`, 4),
    `Local RMSE Fold 1` = round(`Local RMSE Fold 1`, 4),
    `Local RMSE Fold 2` = round(`Local RMSE Fold 2`, 4),
    `Local RMSE Avg` = round(`Local RMSE Avg`, 4),
    `RMSE Difference` = round(`RMSE Difference`, 4),
    `% Difference` = round(`% Difference`, 2),
    `DM Statistic Avg` = round(`DM Statistic Avg`, 4),
    `DM p-value Avg` = round(`DM p-value Avg`, 4)
  )

# Display and save final summary
print(kable(final_summary, format = "html", caption = "Final Model Comparison Summary (Walk-Forward)") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")))

saveRDS(final_summary, "data/rds/final_comparison_summary_wf.rds")
save_kable(final_summary, "data/html/final_comparison_summary_wf.html") 