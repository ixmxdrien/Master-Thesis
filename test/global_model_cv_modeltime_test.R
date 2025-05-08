################################################################################
# 1. LIBRARY SETUP AND PACKAGE INSTALLATION
################################################################################

# Required libraries for time series analysis and modeling
libraries <- c("tidymodels", "modeltime", "timetk", "tidyverse", "lubridate", "tseries",
               "ggplot2", "caret", "dplyr", "plm", "tidyr", "zoo", "forecast", "KFAS", "reshape2",
               "lmtest", "purrr", "urca", "Metrics", "knitr", "kableExtra", "TSrepr")

# Install and load required packages
for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
    library(lib, character.only = TRUE)
  }
}

################################################################################
# 2. DATA LOADING AND INITIAL PROCESSING
################################################################################

# Load and process quarterly datasets for each company
# Tesla quarterly data
df_q1_tesla <- read_csv("data/stocks_and_pred/tesla_prod/kalshi-chart-data-tesla-24-q1.csv") %>% mutate(quarter = "Q1")
df_q2_tesla <- read_csv("data/stocks_and_pred/tesla_prod/kalshi-chart-data-tesla-24-q2.csv") %>% mutate(quarter = "Q2")
df_q3_tesla <- read_csv("data/stocks_and_pred/tesla_prod/kalshi-chart-data-tesla-24-q3.csv") %>% mutate(quarter = "Q3")
df_q4_tesla <- read_csv("data/stocks_and_pred/tesla_prod/kalshi-chart-data-tesla-24-q4.csv") %>% mutate(quarter = "Q4")

# Netflix quarterly data
df_q1_netflix <- read_csv("data/stocks_and_pred/netflix_sub/kalshi-chart-data-netflixsubs-24-q1.csv") %>% mutate(quarter = "Q1")
df_q2_netflix <- read_csv("data/stocks_and_pred/netflix_sub/kalshi-chart-data-netflixsubs-24-q2.csv") %>% mutate(quarter = "Q2")
df_q3_netflix <- read_csv("data/stocks_and_pred/netflix_sub/kalshi-chart-data-netflixsubs-24-q3.csv") %>% mutate(quarter = "Q3")
df_q4_netflix <- read_csv("data/stocks_and_pred/netflix_sub/kalshi-chart-data-netflixsubs-24-q4.csv") %>% mutate(quarter = "Q4")

# Meta quarterly data
df_q1_meta <- read_csv("data/stocks_and_pred/meta_users/kalshi-chart-data-metadap-24-q1.csv") %>% mutate(quarter = "Q1")
df_q2_meta <- read_csv("data/stocks_and_pred/meta_users/kalshi-chart-data-metadap-24-q2.csv") %>% mutate(quarter = "Q2")
df_q3_meta <- read_csv("data/stocks_and_pred/meta_users/kalshi-chart-data-metadap-24-q3.csv") %>% mutate(quarter = "Q3")

# GDP quarterly data
df_q1_gdp <- read_csv("data/stocks_and_pred/GDP_US/kalshi-chart-data-gdp-24apr25.csv") %>% mutate(quarter = "Q1")
df_q2_gdp <- read_csv("data/stocks_and_pred/GDP_US/kalshi-chart-data-gdp-24jul25.csv") %>% mutate(quarter = "Q2")
df_q3_gdp <- read_csv("data/stocks_and_pred/GDP_US/kalshi-chart-data-gdp-24oct30.csv") %>% mutate(quarter = "Q3")
df_q4_gdp <- read_csv("data/stocks_and_pred/GDP_US/kalshi-chart-data-kxgdp-25jan31.csv") %>% mutate(quarter = "Q4")

################################################################################
# 3. DATA COMBINATION AND TRANSFORMATION
################################################################################

# Combine quarterly datasets for each company
df_Tesla <- bind_rows(df_q1_tesla, df_q2_tesla, df_q3_tesla, df_q4_tesla) %>% mutate(ticker = "Tesla")
df_Netflix <- bind_rows(df_q1_netflix, df_q2_netflix, df_q3_netflix, df_q4_netflix) %>% mutate(ticker = "Netflix")
df_Meta <- bind_rows(df_q1_meta, df_q2_meta, df_q3_meta) %>% mutate(ticker = "Meta")
df_GDP <- bind_rows(df_q1_gdp, df_q2_gdp, df_q3_gdp, df_q4_gdp) %>% mutate(ticker = "GDP")

# Load additional market datasets
df_SpaceX <- read_csv("data/stocks_and_pred/spaceX/kalshi-chart-data-spacexcount-24.csv") %>% mutate(ticker = "SpaceX")
df_gas_us <- read_csv("data/stocks_and_pred/price_gas_usa/kalshi-chart-data-aaagasmaxtx-24dec31.csv") %>% mutate(ticker = "Gas US")
df_wti_oil <- read_csv("data/stocks_and_pred/wti_oil/kalshi-chart-data-wtimin-24dec31.csv") %>% mutate(ticker = "WTI Oil")
df_google_sp <- read_csv("data/stocks_and_pred/Sundar_Pichai_google/kalshi-chart-data-googleceochange.csv") %>% mutate(ticker = "Google")
df_fed_rate <- read_csv("data/stocks_and_pred/fed_rate_us/kalshi-chart-data-fedratemin-24dec31.csv") %>% mutate(ticker = "Fed Rate")
df_btc <- read_csv("data/stocks_and_pred/btc/kalshi-chart-data-btcmaxy-24dec31.csv") %>% mutate(ticker = "BTC")
df_us_sc <- read_csv("data/stocks_and_pred/us_semi_conductor/kalshi-chart-data-semiprodh-24.csv") %>% mutate(ticker = "US Semi Conductor")
df_infla <- read_csv("data/stocks_and_pred/inflation/kalshi-chart-data-acpicore-2024.csv") %>% mutate(ticker = "Inflation")
df_layoffs <- read_csv("data/stocks_and_pred/big_tech_layoffs/kalshi-chart-data-bigtechlayoff-24dec31.csv") %>% mutate(ticker = "Layoffs")
df_huricane <- read_csv("data/stocks_and_pred/number_of_huricane/kalshi-chart-data-hurctot-24dec01.csv") %>% mutate(ticker = "Hurricanes")
df_eth <- read_csv("data/stocks_and_pred/eth/kalshi-chart-data-ethmaxy-24dec31.csv") %>% mutate(ticker = "ETH")
df_measles <- read_csv("data/stocks_and_pred/Measles_cases/kalshi-chart-data-measles-24.csv") %>% mutate(ticker = "Measles")
df_apple <- read_csv("data/stocks_and_pred/Apple/kalshi-chart-data-applecar-24dec31.csv") %>% mutate(ticker = "Apple")

################################################################################
# 4. DAILY DATA PROCESSING
################################################################################

# Process daily data for each dataset
process_df_daily <- function(df, ticker) {
  df %>%
    mutate(date = as.Date(Timestamp)) %>% 
    mutate(id = ticker) %>%
    group_by(date, id) %>%
    summarise(pred_daily = mean(Value, na.rm = TRUE))
}

# Process regular datasets
df_pred_daily_TESLA <- process_df_daily(df_Tesla, "TESLA")
df_pred_daily_NETFLIX <- process_df_daily(df_Netflix, "NETFLIX")
df_pred_daily_META <- process_df_daily(df_Meta, "META")
df_pred_daily_GDP <- process_df_daily(df_GDP, "GDP")
df_pred_daily_SpaceX <- process_df_daily(df_SpaceX, "SpaceX")
df_pred_daily_gas_us <- process_df_daily(df_gas_us, "gas_us")
df_pred_daily_wti_oil <- process_df_daily(df_wti_oil, "wti_oil")
df_pred_daily_btc <- process_df_daily(df_btc, "btc")
df_pred_daily_us_sc <- process_df_daily(df_us_sc, "us_sc")
df_pred_daily_infla <- process_df_daily(df_infla, "infla")
df_pred_daily_huricane <- process_df_daily(df_huricane, "huricane")
df_pred_daily_eth <- process_df_daily(df_eth, "eth")
df_pred_daily_measles <- process_df_daily(df_measles, "measles")

# Process forecast data with percentage values
process_df_daily_forecast <- function(df, ticker) {
  df %>%
    mutate(
      date = as.Date(Timestamp),
      Forecast = as.numeric(gsub("%", "", Forecast)),
      id = ticker
    ) %>%
    group_by(date, id) %>%
    summarise(pred_daily = mean(Forecast, na.rm = TRUE)) %>%
    ungroup()
}

# Process forecast datasets
df_pred_daily_google <- process_df_daily_forecast(df_google_sp, "Google")
df_pred_daily_fed <- process_df_daily_forecast(df_fed_rate, "Fed Rate")
df_pred_daily_layoffs <- process_df_daily_forecast(df_layoffs, "Layoffs")
df_pred_daily_apple <- process_df_daily_forecast(df_apple, "Apple")

################################################################################
# 5. DATE SEQUENCE AND DATA COMPLETION
################################################################################

# Create complete date sequences for each dataset
complete_dates_TESLA <- tibble(date = seq(min(df_pred_daily_TESLA$date, na.rm = TRUE),
                                        max(df_pred_daily_TESLA$date, na.rm = TRUE), by = "day"))
complete_dates_NETFLIX <- tibble(date = seq(min(df_pred_daily_NETFLIX$date, na.rm = TRUE),
                                          max(df_pred_daily_NETFLIX$date, na.rm = TRUE), by = "day"))
complete_dates_META <- tibble(date = seq(min(df_pred_daily_META$date, na.rm = TRUE),
                                       max(df_pred_daily_META$date, na.rm = TRUE), by = "day"))
complete_dates_GDP <- tibble(date = seq(min(df_pred_daily_GDP$date, na.rm = TRUE),
                                      max(df_pred_daily_GDP$date, na.rm = TRUE), by = "day"))

# Join with original datasets to ensure complete date coverage
df_pred_daily_TESLA <- complete_dates_TESLA %>% left_join(df_pred_daily_TESLA, by = "date")
df_pred_daily_NETFLIX <- complete_dates_NETFLIX %>% left_join(df_pred_daily_NETFLIX, by = "date")
df_pred_daily_META <- complete_dates_META %>% left_join(df_pred_daily_META, by = "date")
df_pred_daily_GDP <- complete_dates_GDP %>% left_join(df_pred_daily_GDP, by = "date")

################################################################################
# 6. KALMAN FILTER IMPLEMENTATION
################################################################################

# Apply Kalman filter to fill missing values
apply_kalman_filter <- function(df) {
  ts_data <- ts(df$pred_daily, frequency = 365, start = c(2024, 1))
  model <- SSModel(ts_data ~ -1 + SSMtrend(degree = 1, Q = 1))
  kalman_fit <- KFS(model, simplify = TRUE)
  kalman_values <- kalman_fit$a
  if (length(kalman_values) != nrow(df)) {
    kalman_values <- kalman_values[1:nrow(df)]
  }
  df$pred_daily <- ifelse(is.na(df$pred_daily), kalman_values, df$pred_daily)
  df$id[is.na(df$id)] <- unique(df$id[!is.na(df$id)])[1]
  return(df)
}

# Apply Kalman filter to main datasets
df_pred_daily_TESLA <- apply_kalman_filter(df_pred_daily_TESLA)
df_pred_daily_NETFLIX <- apply_kalman_filter(df_pred_daily_NETFLIX)
df_pred_daily_META <- apply_kalman_filter(df_pred_daily_META)
df_pred_daily_GDP <- apply_kalman_filter(df_pred_daily_GDP)

################################################################################
# 7. MARKET DATA INTEGRATION
################################################################################

# Load and process ETF data
df_etf <- read_csv("ETF/combined_returns_2024.csv") %>% drop_na()

# Load and process stock data
df_stock <- read_csv("data/stocks_and_pred/tilt_stocks_2024.csv") %>%
  select(-`...1`) %>%
  filter(ticker %in% c("TSLA", "NFLX", "META", "GOOG", "COIN", "INTC", "JPM", "XOM", "AAPL", 
                       "BAC", "TOT", "PFE", "JNJ", "MSFT", "AMZN", "WMT", "NVDA")) %>%
  mutate(date = as.Date(date))

# Combine stock and ETF data
df_stock <- bind_rows(df_stock, df_etf)

################################################################################
# 8. FINAL DATA PREPARATION
################################################################################

# Combine all prediction datasets
df_pred_all <- bind_rows(
  df_pred_daily_TESLA, df_pred_daily_NETFLIX, df_pred_daily_META, df_pred_daily_GDP,
  df_pred_daily_SpaceX, df_pred_daily_gas_us, df_pred_daily_wti_oil, df_pred_daily_btc,
  df_pred_daily_us_sc, df_pred_daily_infla, df_pred_daily_huricane, df_pred_daily_eth,
  df_pred_daily_measles, df_pred_daily_apple
)

# Combine predictions with stock data
df_combined <- df_pred_all %>% left_join(df_stock, by = c("date"))
df_combined <- df_combined %>% filter(!is.na(returns))

# Create final data table for modeling and ensure proper ordering
data_tbl <- df_combined %>% 
  select(date, ticker, value = returns, id, pred_daily) %>%
  arrange(date, ticker)  # Ensure data is ordered by date and ticker

################################################################################
# 9. TIME SERIES CROSS-VALIDATION WITH MODELTIME
################################################################################

# Calculate the total number of days in the dataset
total_days <- as.numeric(max(data_tbl$date) - min(data_tbl$date))
half_days <- floor(total_days / 2)

# Create time series cross-validation splits with exactly 2 folds
cv_splits <- data_tbl %>%
  time_series_cv(
    date_var = date,
    initial = paste(half_days, "days"),  # First fold will be half of the data
    assess = paste(half_days, "days"),   # Second fold will be the other half
    skip = "1 day",                      # Minimum required skip
    cumulative = TRUE,
    slice_limit = 2                      # Limit to exactly 2 folds
  )

# Create XGBoost model specification with default values
model_spec <- boost_tree(
  mode = "regression",
  trees = 1000,           # Default number of trees
  min_n = 20,            # Default minimum number of observations
  tree_depth = 6,        # Default tree depth
  learn_rate = 0.01,     # Default learning rate
  loss_reduction = 0,    # Default loss reduction
  sample_size = 1,       # Default sample size
  mtry = 1              # Use all predictors
) %>%
  set_engine("xgboost", counts = FALSE)  # Disable counts option

# Create preprocessing recipe
rec_obj <- recipe(value ~ ., data = data_tbl) %>%
  update_role(date, ticker, new_role = "id") %>%
  step_novel(id) %>%
  step_dummy(id) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_timeseries_signature(date) %>%
  step_rm(date) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# Create workflow
wflw <- workflow() %>%
  add_model(model_spec) %>%
  add_recipe(rec_obj)

# Function to train and evaluate model for each fold
train_and_evaluate_fold <- function(split) {
  # Extract training and testing data
  train_data <- training(split)
  test_data <- testing(split)
  
  # Train model
  model_fit <- wflw %>%
    fit(train_data)
  
  # Make predictions
  predictions <- model_fit %>%
    predict(test_data) %>%
    bind_cols(test_data %>% select(date, ticker, value))
  
  # Create modeltime table
  model_tbl <- modeltime_table(model_fit)
  
  # Calibrate model
  calib_tbl <- model_tbl %>%
    modeltime_calibrate(test_data)
  
  # Calculate accuracy metrics using extended metrics for intermittent series
  accuracy_tbl <- suppressMessages(
    calib_tbl %>%
      modeltime_accuracy(metric_set = extended_forecast_accuracy_metric_set())
  )
  
  # Calculate accuracy by ticker using extended metrics
  accuracy_by_ticker <- predictions %>%
    group_by(ticker) %>%
    summarise(
      rmse = sqrt(mean((value - .pred)^2)),
      mae = mean(abs(value - .pred)),
      mape = mean(abs((value - .pred)/value)) * 100,
      mase = mean(abs(value - .pred)) / mean(abs(diff(value))),
      maape = mean(atan(abs((value - .pred)/value))) * 180/pi,  # MAAPE metric for intermittent series
      .groups = "drop"
    )
  
  # Generate forecast errors
  forecast_errors <- predictions %>%
    mutate(
      forecast_error = value - .pred,
      fold = split$id
    ) %>%
    select(date, ticker, forecast_error, fold) %>%
    distinct(date, ticker, .keep_all = TRUE)  # Keep only unique combinations of date and ticker
  
  return(list(
    model = model_fit,
    accuracy = accuracy_tbl,
    accuracy_by_ticker = accuracy_by_ticker,
    forecast_errors = forecast_errors
  ))
}

# Apply cross-validation
cv_results <- suppressMessages(map(cv_splits$splits, train_and_evaluate_fold))

# Extract and combine accuracy results
accuracy_results <- map_dfr(cv_results, ~.x$accuracy, .id = "fold")
accuracy_by_ticker <- map_dfr(cv_results, ~.x$accuracy_by_ticker, .id = "fold")

# Combine forecast errors and remove duplicates
forecast_errors <- map_dfr(cv_results, ~.x$forecast_errors) %>%
  distinct(date, ticker, fold, .keep_all = TRUE)  # Ensure no duplicates across folds

# Print accuracy results with extended metrics
cat("\nOverall Accuracy Metrics for each fold:\n")
print(
  kable(accuracy_results, 
        caption = "Overall Accuracy Metrics by Fold (Including MAAPE)",
        digits = 4) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
)

cat("\nAccuracy Metrics by Ticker for each fold:\n")
print(
  kable(accuracy_by_ticker, 
        caption = "Accuracy Metrics by Ticker and Fold (Including MAAPE)",
        digits = 4) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
)

# Save results
saveRDS(accuracy_results, "data/rds/cv_accuracy_results.rds")
saveRDS(accuracy_by_ticker, "data/rds/cv_accuracy_by_ticker.rds")
saveRDS(forecast_errors, "data/rds/cv_forecast_errors.rds")

################################################################################
# 10. FORECASTING WITH MODELTIME
################################################################################

# Train final model on full dataset
final_model <- wflw %>%
  fit(data_tbl)

# Create modeltime table
model_tbl <- modeltime_table(final_model)

# Prepare data for future predictions
data_tbl <- data_tbl %>%
  distinct(ticker, date, .keep_all = TRUE)

# Create future time frame for predictions
future_tbl <- data_tbl %>%
  group_by(ticker) %>%
  future_frame(
    .length_out = 60, 
    .date_var = date,
    .bind_data = FALSE
  ) %>% 
  mutate(
    id = NA,
    pred_daily = NA
  )

# Generate forecasts with confidence intervals
forecast_results <- model_tbl %>%
  modeltime_forecast(
    new_data = future_tbl,
    actual_data = data_tbl,
    conf_by_id = TRUE
  )

# Visualize forecasts with historical data
forecast_results %>%
  group_by(ticker) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .facet_ncol = 2,
    .title = "60-Day Forecast",
    .y_lab = "Value",
    .x_lab = "Date"
  )

# Save forecast results
saveRDS(forecast_results, "data/rds/forecast_results.rds")
