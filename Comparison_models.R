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
global_rmse <- readRDS("data/global_model_rmse.rds")
local_rmse <- readRDS("data/local_performance_summary.rds")

# Load forecast results for error calculations
global_forecasts <- readRDS("data/global_model_forecasts.rds")
local_forecasts <- readRDS("data/local_forecasts.rds")

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
save_kable(comparison_table, "data/rmse_comparison_table.html")
