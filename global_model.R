################################################################################
# 1. LIBRARY SETUP AND PACKAGE INSTALLATION
################################################################################

# Required libraries for time series analysis and modeling
libraries <- c("tidymodels", "modeltime", "timetk", "tidyverse", "lubridate", "tseries",
               "ggplot2", "caret", "dplyr", "plm", "tidyr", "zoo", "forecast", "KFAS", "reshape2",
               "lmtest", "purrr", "urca", "Metrics")

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

# Load and process datasets
# Tesla quarterly data
df_q1_tesla <- read_csv("analyzing the stock market/tesla_prod/kalshi-chart-data-tesla-24-q1.csv") %>% mutate(quarter = "Q1")
df_q2_tesla <- read_csv("analyzing the stock market/tesla_prod/kalshi-chart-data-tesla-24-q2.csv") %>% mutate(quarter = "Q2")
df_q3_tesla <- read_csv("analyzing the stock market/tesla_prod/kalshi-chart-data-tesla-24-q3.csv") %>% mutate(quarter = "Q3")
df_q4_tesla <- read_csv("analyzing the stock market/tesla_prod/kalshi-chart-data-tesla-24-q4.csv") %>% mutate(quarter = "Q4")

# Netflix quarterly data
df_q1_netflix <- read_csv("analyzing the stock market/netflix_sub/kalshi-chart-data-netflixsubs-24-q1.csv") %>% mutate(quarter = "Q1")
df_q2_netflix <- read_csv("analyzing the stock market/netflix_sub/kalshi-chart-data-netflixsubs-24-q2.csv") %>% mutate(quarter = "Q2")
df_q3_netflix <- read_csv("analyzing the stock market/netflix_sub/kalshi-chart-data-netflixsubs-24-q3.csv") %>% mutate(quarter = "Q3")
df_q4_netflix <- read_csv("analyzing the stock market/netflix_sub/kalshi-chart-data-netflixsubs-24-q4.csv") %>% mutate(quarter = "Q4")

# Meta quarterly data
df_q1_meta <- read_csv("analyzing the stock market/meta_users/kalshi-chart-data-metadap-24-q1.csv") %>% mutate(quarter = "Q1")
df_q2_meta <- read_csv("analyzing the stock market/meta_users/kalshi-chart-data-metadap-24-q2.csv") %>% mutate(quarter = "Q2")
df_q3_meta <- read_csv("analyzing the stock market/meta_users/kalshi-chart-data-metadap-24-q3.csv") %>% mutate(quarter = "Q3")

# GDP quarterly data
df_q1_gdp <- read_csv("analyzing the stock market/GDP_US/kalshi-chart-data-gdp-24apr25.csv") %>% mutate(quarter = "Q1")
df_q2_gdp <- read_csv("analyzing the stock market/GDP_US/kalshi-chart-data-gdp-24jul25.csv") %>% mutate(quarter = "Q2")
df_q3_gdp <- read_csv("analyzing the stock market/GDP_US/kalshi-chart-data-gdp-24oct30.csv") %>% mutate(quarter = "Q3")
df_q4_gdp <- read_csv("analyzing the stock market/GDP_US/kalshi-chart-data-kxgdp-25jan31.csv") %>% mutate(quarter = "Q4")



################################################################################
# 3. DATA COMBINATION AND TRANSFORMATION
################################################################################

# Combine quarterly datasets
df_Tesla <- bind_rows(df_q1_tesla, df_q2_tesla, df_q3_tesla, df_q4_tesla) %>% mutate(ticker = "Tesla")
df_Netflix <- bind_rows(df_q1_netflix, df_q2_netflix, df_q3_netflix, df_q4_netflix) %>% mutate(ticker = "Netflix")
df_Meta <- bind_rows(df_q1_meta, df_q2_meta, df_q3_meta) %>% mutate(ticker = "Meta")
df_GDP <- bind_rows(df_q1_gdp, df_q2_gdp, df_q3_gdp, df_q4_gdp) %>% mutate(ticker = "GDP")

# Load additional market datasets
df_SpaceX <- read_csv("analyzing the stock market/spaceX/kalshi-chart-data-spacexcount-24.csv") %>% mutate(ticker = "SpaceX")
df_gas_us <- read_csv("analyzing the stock market/price_gas_usa/kalshi-chart-data-aaagasmaxtx-24dec31.csv") %>% mutate(ticker = "Gas US")
df_wti_oil <- read_csv("analyzing the stock market/wti_oil/kalshi-chart-data-wtimin-24dec31.csv") %>% mutate(ticker = "WTI Oil")
df_google_sp <- read_csv("analyzing the stock market/Sundar_Pichai_google/kalshi-chart-data-googleceochange.csv") %>% mutate(ticker = "Google")
df_fed_rate <- read_csv("analyzing the stock market/fed_rate_us/kalshi-chart-data-fedratemin-24dec31.csv") %>% mutate(ticker = "Fed Rate")
df_btc <- read_csv("analyzing the stock market/btc/kalshi-chart-data-btcmaxy-24dec31.csv") %>% mutate(ticker = "BTC")
df_us_sc <- read_csv("analyzing the stock market/us_semi_conductor/kalshi-chart-data-semiprodh-24.csv") %>% mutate(ticker = "US Semi Conductor")
df_infla <- read_csv("analyzing the stock market/inflation/kalshi-chart-data-acpicore-2024.csv") %>% mutate(ticker = "Inflation")
df_layoffs <- read_csv("analyzing the stock market/big_tech_layoffs/kalshi-chart-data-bigtechlayoff-24dec31.csv") %>% mutate(ticker = "Layoffs")
df_huricane <- read_csv("analyzing the stock market/number_of_huricane/kalshi-chart-data-hurctot-24dec01.csv") %>% mutate(ticker = "Hurricanes")
df_eth <- read_csv("analyzing the stock market/eth/kalshi-chart-data-ethmaxy-24dec31.csv") %>% mutate(ticker = "ETH")
df_measles <- read_csv("analyzing the stock market/Measles_cases/kalshi-chart-data-measles-24.csv") %>% mutate(ticker = "Measles")
df_apple <- read_csv("analyzing the stock market/Apple/kalshi-chart-data-applecar-24dec31.csv") %>% mutate(ticker = "Apple")



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

# Process forecast data
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

# Join with original datasets
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
df_stock <- read_csv("analyzing the stock market/tilt_stocks_2024.csv") %>%
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

# Create final data table for modeling
data_tbl <- df_combined %>% select(date, ticker, value = returns, id, pred_daily)



################################################################################
# 9. MODEL CREATION AND TRAINING
################################################################################

# Create time series split for modeling
data_tbl %>%
  group_by(ticker) %>%
  plot_time_series(
    date, value, .interactive = FALSE, .facet_ncol = 3
  )


splits <- data_tbl %>% 
  time_series_split(
    assess     = "3 months", 
    cumulative = TRUE
  )

# Create XGBoost preprocessing recipe
rec_obj_xgb <- recipe(value ~ ., training(splits)) %>%
  step_dummy(id) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_timeseries_signature(date) %>%
  step_rm(date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# Create and fit XGBoost workflow
wflw_xgb <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(rec_obj_xgb) %>%
  fit(training(splits))



################################################################################
# 10. MODEL EVALUATION
################################################################################

# Create model table and calibrate
model_tbl <- modeltime_table(wflw_xgb)
calib_tbl <- model_tbl %>% 
  modeltime_calibrate(
    new_data = testing(splits), 
    id = "ticker"
  )

# Evaluate model accuracy
# Overall accuracy metrics
calib_tbl %>% 
  modeltime_accuracy(acc_by_id = FALSE) %>% 
  table_modeltime_accuracy(.interactive = FALSE)

# Accuracy metrics by ticker
calib_tbl %>% 
  modeltime_accuracy(acc_by_id = TRUE) %>% 
  table_modeltime_accuracy(.interactive = FALSE)

# Visualize model performance
calib_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = data_tbl,
    conf_by_id = TRUE
  ) %>%
  group_by(ticker) %>%
  plot_modeltime_forecast(
    .facet_ncol = 3,
    .interactive = FALSE
  )

################################################################################
# 11. FORECASTING
################################################################################

# Refit model on full dataset
refit_tbl <- calib_tbl %>%
  modeltime_refit(data = data_tbl)

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

# Generate and visualize forecasts
forecast_results <- refit_tbl %>%
  modeltime_forecast(
    new_data = future_tbl,
    actual_data = data_tbl, 
    conf_by_id = TRUE
  )


# Calculate RMSE for each ticker (local accuracy)
global_rmse_by_ticker <- calib_tbl %>% 
  modeltime_accuracy(acc_by_id = TRUE) %>%
  select(ticker, global_rmse = rmse)

# Calculate overall RMSE (global accuracy)
global_rmse_overall <- calib_tbl %>% 
  modeltime_accuracy(acc_by_id = FALSE) %>%
  select(global_rmse = rmse) %>%
  mutate(ticker = "OVERALL")

# Combine both RMSE measurements
global_rmse_results <- bind_rows(global_rmse_by_ticker, global_rmse_overall)

# Save RMSE results
saveRDS(global_rmse_results, "global_model_rmse.rds")

# Save forecast results
saveRDS(forecast_results, "global_model_forecasts.rds")

# Save forecast results
saveRDS(forecast_results, "global_model_forecasts.rds")

# Visualize forecasts
forecast_results %>%
  group_by(ticker) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .facet_ncol = 2
  )
