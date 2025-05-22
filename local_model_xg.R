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
df_etf <- read_csv("ETF/combined_returns_2024.csv") %>% 
  drop_na() %>%
  filter(ticker != "CAC.PA")

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
  df_pred_daily_infla, df_pred_daily_huricane, df_pred_daily_eth,
  df_pred_daily_measles, df_pred_daily_apple
)

# Combine predictions with stock data
df_combined <- df_pred_all %>% left_join(df_stock, by = c("date"))
df_combined <- df_combined %>% filter(!is.na(returns))

# Create final data table for modeling
data_tbl <- df_combined %>% select(date, ticker, value = returns, id, pred_daily)

################################################################################
# 9. MODEL CREATION AND TRAINING - PAR TICKER
################################################################################

# Liste des tickers (actions) à modéliser
tickers_to_model <- unique(data_tbl$ticker)

# Fonction pour créer modèle XGBoost pour chaque ticker
create_xgboost_model_per_ticker <- function(ticker_name) {
  data_ticker <- data_tbl %>% filter(ticker == ticker_name)
  
  if (nrow(data_ticker) < 100) {
    message(paste("Pas assez de données pour", ticker_name))
    return(NULL)
  }
  
  splits <- time_series_cv(
    data_ticker,
    date_var   = date,
    initial    = "150 days",
    assess     = "75 days",
    skip       = "75 days",
    cumulative = TRUE,
    slice_limit = 2
  )
  
  accuracy_list <- list()
  forecast_list <- list()
  
  for (i in seq_along(splits$splits)) {
    split <- splits$splits[[i]]
    train_data <- analysis(split)
    test_data  <- assessment(split)
    
    rec <- recipe(value ~ ., data = train_data) %>%
      step_rm(ticker) %>%
      step_normalize(all_numeric(), -all_outcomes()) %>%
      step_timeseries_signature(date) %>%
      step_rm(date) %>%
      step_zv(all_predictors()) %>%
      step_dummy(all_nominal_predictors(), one_hot = TRUE)
    
    wf <- workflow() %>%
      add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
      add_recipe(rec) %>%
      fit(train_data)
    
    model_tbl <- modeltime_table(wf)
    calib_tbl <- model_tbl %>%
      modeltime_calibrate(new_data = test_data)
    
    forecast_tbl <- calib_tbl %>%
      modeltime_forecast(new_data = test_data, actual_data = data_ticker)
    
    accuracy_tbl <- calib_tbl %>%
      modeltime_accuracy() %>%
      mutate(fold = paste0("Fold_", i))
    
    accuracy_list[[i]] <- accuracy_tbl
    forecast_list[[i]] <- forecast_tbl
  }
  
  list(
    ticker = ticker_name,
    accuracy = bind_rows(accuracy_list),
    forecasts = forecast_list
  )
}


# Appliquer la fonction à chaque ticker
xgboost_models_by_ticker <- tickers_to_model %>%
  set_names() %>%
  map(create_xgboost_model_per_ticker)

# Nettoyer les NULL (tickers sans données suffisantes)
xgboost_models_by_ticker <- compact(xgboost_models_by_ticker)

# Résumé des performances : on conserve le détail par split
accuracy_summary <- map_dfr(
  xgboost_models_by_ticker,
  ~ .x$accuracy %>% mutate(ticker = .x$ticker, split = row_number())
)

# Save accuracy summary to RDS file
saveRDS(accuracy_summary, "data/rds/local_model_accuracy_metrics.rds")

# Affichage des accuracies pour chaque split (fold)
cat("===== Résultats par fold (split) =====\n")
accuracy_summary %>%
  select(ticker, split, mae, rmse, rsq) %>%
  arrange(ticker, split) %>%
  print(n = Inf)




################################################################################
# 10. ERROR ANALYSIS BY TIME POINT
################################################################################

# Create forecast error data frames for both folds
xgboost_forecast_errors_fold1 <- data.frame()
xgboost_forecast_errors_fold2 <- data.frame()

# Calculate forecast errors for each ticker in both folds
for (ticker in names(xgboost_models_by_ticker)) {
  # Get forecasts for both folds
  forecasts_fold1 <- xgboost_models_by_ticker[[ticker]]$forecasts[[1]]
  forecasts_fold2 <- xgboost_models_by_ticker[[ticker]]$forecasts[[2]]
  
  # Process fold 1 errors
  if (!is.null(forecasts_fold1)) {
    # Extract the relevant data from the forecast object
    forecast_data_fold1 <- forecasts_fold1 %>%
      filter(.model_desc != "ACTUAL") %>%
      select(.index, .value) %>%
      rename(date = .index, y_hat_t = .value)
    
    actual_data_fold1 <- forecasts_fold1 %>%
      filter(.model_desc == "ACTUAL") %>%
      select(.index, .value) %>%
      rename(date = .index, y_t = .value)
    
    # Join forecast and actual data
    ticker_errors_fold1 <- forecast_data_fold1 %>%
      left_join(actual_data_fold1, by = "date") %>%
      mutate(
        ticker = ticker,
        fold = "Fold 1"
      )
    
    # Calculate forecast errors
    ticker_errors_fold1 <- ticker_errors_fold1 %>%
      mutate(e1_t = y_t - y_hat_t) %>%
      filter(!is.na(y_t) & !is.na(y_hat_t))
    
    # Add to main data frame
    xgboost_forecast_errors_fold1 <- bind_rows(xgboost_forecast_errors_fold1, ticker_errors_fold1)
  }
  
  # Process fold 2 errors
  if (!is.null(forecasts_fold2)) {
    # Extract the relevant data from the forecast object
    forecast_data_fold2 <- forecasts_fold2 %>%
      filter(.model_desc != "ACTUAL") %>%
      select(.index, .value) %>%
      rename(date = .index, y_hat_t = .value)
    
    actual_data_fold2 <- forecasts_fold2 %>%
      filter(.model_desc == "ACTUAL") %>%
      select(.index, .value) %>%
      rename(date = .index, y_t = .value)
    
    # Join forecast and actual data
    ticker_errors_fold2 <- forecast_data_fold2 %>%
      left_join(actual_data_fold2, by = "date") %>%
      mutate(
        ticker = ticker,
        fold = "Fold 2"
      )
    
    # Calculate forecast errors
    ticker_errors_fold2 <- ticker_errors_fold2 %>%
      mutate(e1_t = y_t - y_hat_t) %>%
      filter(!is.na(y_t) & !is.na(y_hat_t))
    
    # Add to main data frame
    xgboost_forecast_errors_fold2 <- bind_rows(xgboost_forecast_errors_fold2, ticker_errors_fold2)
  }
}

# Combine errors from both folds
xgboost_forecast_errors <- bind_rows(xgboost_forecast_errors_fold1, xgboost_forecast_errors_fold2)

# Remove any remaining duplicates
xgboost_forecast_errors <- xgboost_forecast_errors %>%
  distinct(date, ticker, fold, .keep_all = TRUE)

# Save results
saveRDS(xgboost_forecast_errors, "data/rds/xgboost_forecast_errors.rds")

# Display summary statistics of forecast errors
cat("\nSummary of XGBoost Forecast Errors:\n")
print(summary(xgboost_forecast_errors))

