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
arima_rmse <- readRDS("data/rds/local_performance_summary.rds")
xgb_rmse <- readRDS("data/rds/local_model_accuracy_metrics.rds")

# Print structure of RMSE data
cat("\nStructure of ARIMA/ARIMAX RMSE:\n")
str(arima_rmse)
print(head(arima_rmse))

cat("\nStructure of XGBoost RMSE:\n")
str(xgb_rmse)
print(head(xgb_rmse))

# Load forecast errors for both models
arima_errors <- readRDS("data/rds/local_forecast_errors.rds")
xgb_errors <- readRDS("data/rds/xgboost_forecast_errors.rds")

cat("\nStructure of ARIMA/ARIMAX errors:\n")
str(arima_errors)
print(head(arima_errors))

cat("\nStructure of XGBoost errors:\n")
str(xgb_errors)
print(head(xgb_errors))

################################################################################
# 3. RMSE COMPARISON
################################################################################

# Prepare XGBoost RMSE table (one row per ticker, per fold)
xgb_rmse_folds <- xgb_rmse %>%
  select(ticker, rmse, fold) %>%
  filter(ticker != "QQQ") %>%
  pivot_wider(names_from = fold, values_from = rmse, names_prefix = "xgb_rmse_")

# Prepare ARIMA RMSE table (already one row per ticker)
arima_rmse <- arima_rmse %>%
  filter(ticker != "QQQ") %>%
  rename(arima_rmse_Fold1 = local_rmse_fold1, arima_rmse_Fold2 = local_rmse_fold2)

# Merge for comparison
comparison_df <- inner_join(arima_rmse, xgb_rmse_folds, by = "ticker")

# Create a more intuitive comparison table
comparison_table <- comparison_df %>%
  mutate(
    # Calculate differences for each fold
    fold1_diff = xgb_rmse_Fold_1 - arima_rmse_Fold1,
    fold2_diff = xgb_rmse_Fold_2 - arima_rmse_Fold2,
    # Determine better model for each fold
    fold1_better = case_when(
      fold1_diff < 0 ~ "XGBoost",
      fold1_diff > 0 ~ "ARIMA/ARIMAX",
      TRUE ~ "Equal"
    ),
    fold2_better = case_when(
      fold2_diff < 0 ~ "XGBoost",
      fold2_diff > 0 ~ "ARIMA/ARIMAX",
      TRUE ~ "Equal"
    ),
    # Calculate percentage improvements
    fold1_improvement = (fold1_diff / arima_rmse_Fold1) * 100,
    fold2_improvement = (fold2_diff / arima_rmse_Fold2) * 100
  ) %>%
  select(
    Ticker = ticker,
    `ARIMA/ARIMAX RMSE (Fold 1)` = arima_rmse_Fold1,
    `XGBoost RMSE (Fold 1)` = xgb_rmse_Fold_1,
    `Fold 1 Diff` = fold1_diff,
    `Fold 1 % Imp` = fold1_improvement,
    `Fold 1 Better` = fold1_better,
    `ARIMA/ARIMAX RMSE (Fold 2)` = arima_rmse_Fold2,
    `XGBoost RMSE (Fold 2)` = xgb_rmse_Fold_2,
    `Fold 2 Diff` = fold2_diff,
    `Fold 2 % Imp` = fold2_improvement,
    `Fold 2 Better` = fold2_better
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 4)),
    `Fold 1 % Imp` = round(`Fold 1 % Imp`, 2),
    `Fold 2 % Imp` = round(`Fold 2 % Imp`, 2)
  )

# Display comparison table with conditional formatting
print(kable(comparison_table, format = "html", caption = "Local Models Comparison by Fold") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
      row_spec(which(comparison_table$`Fold 1 Better` == "XGBoost"), background = "#e6ffe6") %>%
      row_spec(which(comparison_table$`Fold 1 Better` == "ARIMA/ARIMAX"), background = "#ffe6e6") %>%
      row_spec(which(comparison_table$`Fold 2 Better` == "XGBoost"), background = "#e6ffe6") %>%
      row_spec(which(comparison_table$`Fold 2 Better` == "ARIMA/ARIMAX"), background = "#ffe6e6"))

# Save comparison table
save_kable(comparison_table, "data/local_models_comparison_table.html")

# Print summary statistics
cat("\nSummary of Model Performance:\n")
cat("\nFold 1 Summary:\n")
fold1_summary <- comparison_df %>%
  summarise(
    xgb_better = sum(xgb_rmse_Fold_1 < arima_rmse_Fold1),
    arima_better = sum(xgb_rmse_Fold_1 > arima_rmse_Fold1),
    equal = sum(xgb_rmse_Fold_1 == arima_rmse_Fold1),
    avg_improvement = mean((xgb_rmse_Fold_1 - arima_rmse_Fold1) / arima_rmse_Fold1 * 100)
  )
print(fold1_summary)

cat("\nFold 2 Summary:\n")
fold2_summary <- comparison_df %>%
  summarise(
    xgb_better = sum(xgb_rmse_Fold_2 < arima_rmse_Fold2),
    arima_better = sum(xgb_rmse_Fold_2 > arima_rmse_Fold2),
    equal = sum(xgb_rmse_Fold_2 == arima_rmse_Fold2),
    avg_improvement = mean((xgb_rmse_Fold_2 - arima_rmse_Fold2) / arima_rmse_Fold2 * 100)
  )
print(fold2_summary)

################################################################################
# 4. DIEBOLD-MARIANO TEST IMPLEMENTATION
################################################################################

# Function to perform Diebold-Mariano test for each ticker and fold
perform_dm_test <- function(arima_errors, xgb_errors, ticker, fold) {
  # Filter errors for specific ticker and fold
  arima_ticker <- arima_errors %>% filter(ticker == !!ticker, fold == !!fold)
  xgb_ticker <- xgb_errors %>% filter(ticker == !!ticker, fold == !!fold)
  
  # Convert dates to character strings for comparison
  arima_ticker <- arima_ticker %>% mutate(date_str = as.character(date))
  xgb_ticker <- xgb_ticker %>% mutate(date_str = as.character(date))
  
  # Find common dates between both models
  common_dates <- intersect(arima_ticker$date_str, xgb_ticker$date_str)
  
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
  arima_aligned <- arima_ticker %>% 
    filter(date_str %in% common_dates) %>% 
    arrange(date_str) %>% 
    select(date_str, e1_t)
  xgb_aligned <- xgb_ticker %>% 
    filter(date_str %in% common_dates) %>% 
    arrange(date_str) %>% 
    select(date_str, e1_t)
  
  # Verify date alignment
  if (!all(arima_aligned$date_str == xgb_aligned$date_str)) {
    return(data.frame(
      ticker = ticker,
      fold = fold,
      dm_statistic = NA,
      p_value = NA,
      conclusion = "Date alignment issue",
      n_observations = nrow(arima_aligned)
    ))
  }
  
  # Extract errors excluding NA values
  arima_e <- arima_aligned$e1_t[!is.na(arima_aligned$e1_t)]
  xgb_e <- xgb_aligned$e1_t[!is.na(xgb_aligned$e1_t)]
  
  # Calculate h parameter based on data size
  n_obs <- length(arima_e)
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
  dm_result <- dm.test(arima_e, xgb_e, alternative = "two.sided", h = h)
  
  # Interpret results
  conclusion <- case_when(
    dm_result$p.value < 0.05 & dm_result$statistic > 0 ~ "XGBoost significantly better",
    dm_result$p.value < 0.05 & dm_result$statistic < 0 ~ "ARIMA/ARIMAX significantly better",
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
for (ticker in unique(comparison_df$ticker)) {
  for (fold in c("Fold 1", "Fold 2")) {
    result <- perform_dm_test(arima_errors, xgb_errors, ticker, fold)
    dm_results <- bind_rows(dm_results, result)
  }
}

# Format DM results for display
dm_table <- dm_results %>%
  mutate(
    dm_statistic = round(dm_statistic, 4),
    p_value = round(p_value, 4),
    conclusion = case_when(
      conclusion == "XGBoost significantly better" ~ "XGBoost ✓",
      conclusion == "ARIMA/ARIMAX significantly better" ~ "ARIMA/ARIMAX ✓",
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

print(kable(dm_table, format = "html", caption = "Diebold-Mariano Test Results by Fold") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
      row_spec(which(dm_table$Conclusion == "XGBoost ✓"), background = "#e6ffe6") %>%
      row_spec(which(dm_table$Conclusion == "ARIMA/ARIMAX ✓"), background = "#ffe6e6"))

# Save DM test results
saveRDS(dm_results, "data/rds/local_models_dm_test_results.rds")
save_kable(dm_table, "data/html/local_models_dm_test_results.html")

cat("\nSummary of Diebold-Mariano Test Results:\n")
dm_summary <- dm_results %>%
  group_by(fold) %>%
  summarise(
    xgb_better = sum(conclusion == "XGBoost significantly better", na.rm = TRUE),
    arima_better = sum(conclusion == "ARIMA/ARIMAX significantly better", na.rm = TRUE),
    no_difference = sum(conclusion == "No significant difference", na.rm = TRUE),
    insufficient_data = sum(conclusion %in% c("Insufficient data", "Date alignment issue"), na.rm = TRUE)
  )
print(kable(dm_summary, format = "html", caption = "Summary of DM Test Results by Fold") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")))

################################################################################
# 5. FINAL SUMMARY
################################################################################

final_summary <- comparison_df %>%
  left_join(
    dm_results %>%
      group_by(ticker) %>%
      summarise(
        dm_statistic_avg = mean(dm_statistic, na.rm = TRUE),
        dm_p_value_avg = mean(p_value, na.rm = TRUE),
        conclusion = case_when(
          all(conclusion == "XGBoost significantly better") ~ "XGBoost significantly better",
          all(conclusion == "ARIMA/ARIMAX significantly better") ~ "ARIMA/ARIMAX significantly better",
          TRUE ~ "Mixed results"
        )
      ),
    by = "ticker"
  ) %>%
  select(
    Ticker = ticker,
    `ARIMA/ARIMAX RMSE Fold 1` = arima_rmse_Fold1,
    `XGBoost RMSE Fold 1` = xgb_rmse_Fold1,
    `ARIMA/ARIMAX RMSE Fold 2` = arima_rmse_Fold2,
    `XGBoost RMSE Fold 2` = xgb_rmse_Fold2,
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

print(kable(final_summary, format = "html", caption = "Final Local Models Comparison Summary") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")))

saveRDS(final_summary, "data/rds/local_models_final_comparison_summary.rds")
save_kable(final_summary, "data/html/local_models_final_comparison_summary.html") 