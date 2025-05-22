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
xgb_rmse <- readRDS("data/rds/global_model_rmse.rds")
lm_rmse <- readRDS("data/rds/global_model_rmse_rs.rds")

# Print structure of RMSE data
cat("\nStructure of XGBoost RMSE:\n")
str(xgb_rmse)
print(head(xgb_rmse))

cat("\nStructure of Linear Regression RMSE:\n")
str(lm_rmse)
print(head(lm_rmse))

# Load forecast errors for both models
xgb_errors <- readRDS("data/rds/forecast_errors.rds")
lm_errors <- readRDS("data/rds/forecast_errors_rs.rds")

# Print structure of error data
cat("\nStructure of XGBoost errors:\n")
str(xgb_errors)
print(head(xgb_errors))

cat("\nStructure of Linear Regression errors:\n")
str(lm_errors)
print(head(lm_errors))

################################################################################
# 3. RMSE COMPARISON
################################################################################

# Create comparison dataframe by joining XGBoost and Linear Regression RMSE results
comparison_df <- inner_join(
  xgb_rmse,
  lm_rmse,
  by = "ticker",
  suffix = c("_xgb", "_lm")
)

# Create a more intuitive comparison table
comparison_table <- comparison_df %>%
  mutate(
    # Calculate differences for each fold
    fold1_diff = global_rmse_fold1_lm - global_rmse_fold1_xgb,
    fold2_diff = global_rmse_fold2_lm - global_rmse_fold2_xgb,
    # Determine better model for each fold
    fold1_better = case_when(
      fold1_diff < 0 ~ "Linear Regression",
      fold1_diff > 0 ~ "XGBoost",
      TRUE ~ "Equal"
    ),
    fold2_better = case_when(
      fold2_diff < 0 ~ "Linear Regression",
      fold2_diff > 0 ~ "XGBoost",
      TRUE ~ "Equal"
    ),
    # Calculate percentage improvements
    fold1_improvement = (fold1_diff / global_rmse_fold1_xgb) * 100,
    fold2_improvement = (fold2_diff / global_rmse_fold2_xgb) * 100
  ) %>%
  select(
    Ticker = ticker,
    # Fold 1 results
    `XGBoost RMSE (Fold 1)` = global_rmse_fold1_xgb,
    `Linear Regression RMSE (Fold 1)` = global_rmse_fold1_lm,
    `Fold 1 Diff` = fold1_diff,
    `Fold 1 % Imp` = fold1_improvement,
    `Fold 1 Better` = fold1_better,
    # Fold 2 results
    `XGBoost RMSE (Fold 2)` = global_rmse_fold2_xgb,
    `Linear Regression RMSE (Fold 2)` = global_rmse_fold2_lm,
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
print(kable(comparison_table, format = "html", caption = "Global Models Comparison by Fold") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
      row_spec(which(comparison_table$`Fold 1 Better` == "Linear Regression"), background = "#e6ffe6") %>%
      row_spec(which(comparison_table$`Fold 1 Better` == "XGBoost"), background = "#ffe6e6") %>%
      row_spec(which(comparison_table$`Fold 2 Better` == "Linear Regression"), background = "#e6ffe6") %>%
      row_spec(which(comparison_table$`Fold 2 Better` == "XGBoost"), background = "#ffe6e6"))

# Save comparison table
save_kable(comparison_table, "data/global_models_comparison_table.html")

# Print summary statistics
cat("\nSummary of Model Performance:\n")
cat("\nFold 1 Summary:\n")
fold1_summary <- comparison_df %>%
  summarise(
    linear_better = sum(global_rmse_fold1_lm < global_rmse_fold1_xgb),
    xgb_better = sum(global_rmse_fold1_lm > global_rmse_fold1_xgb),
    equal = sum(global_rmse_fold1_lm == global_rmse_fold1_xgb),
    avg_improvement = mean((global_rmse_fold1_lm - global_rmse_fold1_xgb) / global_rmse_fold1_xgb * 100)
  )
print(fold1_summary)

cat("\nFold 2 Summary:\n")
fold2_summary <- comparison_df %>%
  summarise(
    linear_better = sum(global_rmse_fold2_lm < global_rmse_fold2_xgb),
    xgb_better = sum(global_rmse_fold2_lm > global_rmse_fold2_xgb),
    equal = sum(global_rmse_fold2_lm == global_rmse_fold2_xgb),
    avg_improvement = mean((global_rmse_fold2_lm - global_rmse_fold2_xgb) / global_rmse_fold2_xgb * 100)
  )
print(fold2_summary)

################################################################################
# 4. DIEBOLD-MARIANO TEST IMPLEMENTATION
################################################################################

# Function to perform Diebold-Mariano test for each ticker and fold
perform_dm_test <- function(xgb_errors, lm_errors, ticker, fold) {
  # Filter errors for specific ticker and fold
  xgb_ticker <- xgb_errors %>% filter(ticker == !!ticker)
  lm_ticker <- lm_errors %>% filter(ticker == !!ticker, fold == !!fold)
  
  # Convert dates to character strings for comparison
  xgb_ticker <- xgb_ticker %>%
    mutate(date_str = as.character(.index))
  
  lm_ticker <- lm_ticker %>%
    mutate(date_str = as.character(.index))
  
  # Find common dates between both models
  common_dates <- intersect(xgb_ticker$date_str, lm_ticker$date_str)
  
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
  xgb_aligned <- xgb_ticker %>% 
    filter(date_str %in% common_dates) %>% 
    arrange(date_str) %>% 
    select(date_str, e1_t)
  
  lm_aligned <- lm_ticker %>% 
    filter(date_str %in% common_dates) %>% 
    arrange(date_str) %>% 
    select(date_str, e1_t)
  
  # Verify date alignment
  if (!all(xgb_aligned$date_str == lm_aligned$date_str)) {
    return(data.frame(
      ticker = ticker,
      fold = fold,
      dm_statistic = NA,
      p_value = NA,
      conclusion = "Date alignment issue",
      n_observations = nrow(xgb_aligned)
    ))
  }
  
  # Extract errors excluding NA values
  xgb_errors <- xgb_aligned$e1_t[!is.na(xgb_aligned$e1_t)]
  lm_errors <- lm_aligned$e1_t[!is.na(lm_aligned$e1_t)]
  
  # Calculate h parameter based on data size
  n_obs <- length(xgb_errors)
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
  dm_result <- dm.test(xgb_errors, lm_errors, alternative = "two.sided", h = h)
  
  # Interpret results
  conclusion <- case_when(
    dm_result$p.value < 0.05 & dm_result$statistic > 0 ~ "Linear Regression significantly better",
    dm_result$p.value < 0.05 & dm_result$statistic < 0 ~ "XGBoost significantly better",
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
for (ticker in unique(xgb_errors$ticker)) {
  for (fold in c("Fold 1", "Fold 2")) {
    result <- perform_dm_test(xgb_errors, lm_errors, ticker, fold)
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
      conclusion == "Linear Regression significantly better" ~ "Linear Regression ✓",
      conclusion == "XGBoost significantly better" ~ "XGBoost ✓",
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
      row_spec(which(dm_table$Conclusion == "Linear Regression ✓"), background = "#e6ffe6") %>%
      row_spec(which(dm_table$Conclusion == "XGBoost ✓"), background = "#ffe6e6"))

# Save DM test results
saveRDS(dm_results, "data/rds/global_models_dm_test_results.rds")
save_kable(dm_table, "data/html/global_models_dm_test_results.html")

# Print summary of DM test results
cat("\nSummary of Diebold-Mariano Test Results:\n")
dm_summary <- dm_results %>%
  group_by(fold) %>%
  summarise(
    linear_better = sum(conclusion == "Linear Regression significantly better", na.rm = TRUE),
    xgb_better = sum(conclusion == "XGBoost significantly better", na.rm = TRUE),
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
          all(conclusion == "Linear Regression significantly better") ~ "Linear Regression significantly better",
          all(conclusion == "XGBoost significantly better") ~ "XGBoost significantly better",
          TRUE ~ "Mixed results"
        )
      ),
    by = "ticker"
  ) %>%
  select(
    Ticker = ticker,
    `XGBoost RMSE Fold 1` = global_rmse_fold1_xgb,
    `Linear Regression RMSE Fold 1` = global_rmse_fold1_lm,
    `XGBoost RMSE Fold 2` = global_rmse_fold2_xgb,
    `Linear Regression RMSE Fold 2` = global_rmse_fold2_lm,
    `RMSE Difference Fold 1` = fold1_diff,
    `RMSE Difference Fold 2` = fold2_diff,
    `% Difference Fold 1` = fold1_improvement,
    `% Difference Fold 2` = fold2_improvement,
    `DM Statistic Avg` = dm_statistic_avg,
    `DM p-value Avg` = dm_p_value_avg,
    `DM Conclusion` = conclusion
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 4)),
    `% Difference Fold 1` = round(`% Difference Fold 1`, 2),
    `% Difference Fold 2` = round(`% Difference Fold 2`, 2)
  )

# Display and save final summary
print(kable(final_summary, format = "html", caption = "Final Global Models Comparison Summary") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")))

saveRDS(final_summary, "data/rds/global_models_final_comparison_summary.rds")
save_kable(final_summary, "data/html/global_models_final_comparison_summary.html") 