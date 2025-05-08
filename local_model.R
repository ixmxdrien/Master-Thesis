################################################################################
# 1. LIBRARY SETUP AND PACKAGE INSTALLATION
################################################################################

# Required libraries for time series analysis and modeling
libraries <- c("tidymodels", "modeltime", "timetk", "tidyverse", "lubridate", "tseries",
               "ggplot2", "caret", "dplyr", "plm", "tidyr", "zoo", "forecast", "KFAS", "reshape2",
               "lmtest", "purrr", "urca", "Metrics", "strucchange", "moments")

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
df_pred_daily_SpaceX <- apply_kalman_filter(df_pred_daily_SpaceX)
df_pred_daily_gas_us <- apply_kalman_filter(df_pred_daily_gas_us)
df_pred_daily_wti_oil <- apply_kalman_filter(df_pred_daily_wti_oil)
df_pred_daily_btc <- apply_kalman_filter(df_pred_daily_btc)
df_pred_daily_us_sc <- apply_kalman_filter(df_pred_daily_us_sc)
df_pred_daily_infla <- apply_kalman_filter(df_pred_daily_infla)
df_pred_daily_huricane <- apply_kalman_filter(df_pred_daily_huricane)
df_pred_daily_eth <- apply_kalman_filter(df_pred_daily_eth)
df_pred_daily_measles <- apply_kalman_filter(df_pred_daily_measles)

################################################################################
# 7. STATIONARITY CHECK PREDICTION MARKET
################################################################################

# Function to check and handle stationarity
check_stationarity <- function(df, ticker) {
  df_clean <- df[!is.na(df$pred_daily), ]
  
  # Perform ADF test
  adf_result <- adf.test(df_clean$pred_daily, alternative = "stationary")
  cat(paste("ADF test for", ticker, "p-value:", adf_result$p.value, "\n"))
  
  # Apply differencing if needed
  if (adf_result$p.value > 0.05) {
    cat(paste(ticker, "is not stationary. Applying first differencing...\n"))
    df$pred_daily <- c(NA, diff(df$pred_daily, differences = 1))
    
    # Check stationarity after first differencing
    df_clean <- df[!is.na(df$pred_daily), ]
    adf_result_diff1 <- adf.test(df_clean$pred_daily, alternative = "stationary")
    cat(paste("ADF test after first differencing for", ticker, "p-value:", adf_result_diff1$p.value, "\n"))
    
    # Apply second differencing if needed
    if (adf_result_diff1$p.value > 0.05) {
      cat(paste(ticker, "is still not stationary. Applying second differencing...\n"))
      df$pred_daily <- c(NA, diff(df$pred_daily, differences = 1))
      
      df_clean <- df[!is.na(df$pred_daily), ]
      adf_result_diff2 <- adf.test(df_clean$pred_daily, alternative = "stationary")
      cat(paste("ADF test after second differencing for", ticker, "p-value:", adf_result_diff2$p.value, "\n"))
    } else {
      cat(paste(ticker, "is now stationary after first differencing.\n"))
    }
  } else {
    cat(paste(ticker, "is stationary.\n"))
  }
  
  return(df)
}

# Apply stationarity check to all datasets
df_pred_daily_TESLA <- check_stationarity(df_pred_daily_TESLA, "TESLA") %>% na.omit()
df_pred_daily_NETFLIX <- check_stationarity(df_pred_daily_NETFLIX, "NETFLIX") %>% na.omit()
df_pred_daily_META <- check_stationarity(df_pred_daily_META, "META") %>% na.omit()
df_pred_daily_GDP <- check_stationarity(df_pred_daily_GDP, "GDP") %>% na.omit()
df_pred_daily_SpaceX <- check_stationarity(df_pred_daily_SpaceX, "SpaceX") %>% na.omit()
df_pred_daily_gas_us <- check_stationarity(df_pred_daily_gas_us, "Gas US") %>% na.omit()
df_pred_daily_wti_oil <- check_stationarity(df_pred_daily_wti_oil, "WTI Oil") %>% na.omit()
df_pred_daily_btc <- check_stationarity(df_pred_daily_btc, "BTC") %>% na.omit()
df_pred_daily_us_sc <- check_stationarity(df_pred_daily_us_sc, "US Semi Conductor") %>% na.omit()
df_pred_daily_infla <- check_stationarity(df_pred_daily_infla, "Inflation") %>% na.omit()
df_pred_daily_huricane <- check_stationarity(df_pred_daily_huricane, "Hurricanes") %>% na.omit()
df_pred_daily_eth <- check_stationarity(df_pred_daily_eth, "ETH") %>% na.omit()
df_pred_daily_measles <- check_stationarity(df_pred_daily_measles, "Measles") %>% na.omit()
df_pred_daily_apple <- check_stationarity(df_pred_daily_apple, "Apple") %>% na.omit()

# Combine all prediction datasets
df_pred_all <- bind_rows(
  df_pred_daily_TESLA, df_pred_daily_NETFLIX, df_pred_daily_META, df_pred_daily_GDP,
  df_pred_daily_SpaceX, df_pred_daily_gas_us, df_pred_daily_wti_oil, df_pred_daily_btc,
  df_pred_daily_us_sc, df_pred_daily_infla, df_pred_daily_huricane, df_pred_daily_eth,
  df_pred_daily_measles, df_pred_daily_apple
)

################################################################################
# 8. MARKET DATA INTEGRATION
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
# 9. STATIONARITY CHECK STOCKS
################################################################################

# Function to check and handle stock data stationarity
check_stationarity <- function(df, ticker) {
  df_clean <- df[!is.na(df$returns), ]
  
  # Perform ADF test
  adf_result <- adf.test(df_clean$returns, alternative = "stationary")
  cat(paste("ADF test for", ticker, "p-value:", adf_result$p.value, "\n"))
  
  # Apply differencing if needed
  if (adf_result$p.value > 0.05) {
    cat(paste(ticker, "is not stationary. Applying first differencing...\n"))
    df$returns <- c(NA, diff(df$returns, differences = 1))
    
    # Check stationarity after first differencing
    df_clean <- df[!is.na(df$returns), ]
    adf_result_diff1 <- adf.test(df_clean$returns, alternative = "stationary")
    cat(paste("ADF test after first differencing for", ticker, "p-value:", adf_result_diff1$p.value, "\n"))
    
    # Apply second differencing if needed
    if (adf_result_diff1$p.value > 0.05) {
      cat(paste(ticker, "is still not stationary. Applying second differencing...\n"))
      df$returns <- c(NA, diff(df$returns, differences = 1))
      
      df_clean <- df[!is.na(df$returns), ]
      adf_result_diff2 <- adf.test(df_clean$returns, alternative = "stationary")
      cat(paste("ADF test after second differencing for", ticker, "p-value:", adf_result_diff2$p.value, "\n"))
    } else {
      cat(paste(ticker, "is now stationary after first differencing.\n"))
    }
  } else {
    cat(paste(ticker, "is stationary.\n"))
  }
  
  return(df)
}

# Apply stationarity check to each ticker
for (ticker in unique(df_stock$ticker)) {
  df_stock <- check_stationarity(df_stock, ticker)
}

################################################################################
# 10. CROSS-CORRELATION ANALYSIS
################################################################################

# Function to compute cross-correlation between two time series
compute_cross_correlation <- function(stock_series, pred_series, max_lag = 20) {
  # Merge series on date
  df_combined <- inner_join(stock_series, pred_series, by = "date", suffix = c("_stock", "_pred"))
  df_clean <- df_combined %>% drop_na(pred_daily_stock, pred_daily_pred)
  
  # Check data sufficiency
  if (nrow(df_clean) < (max_lag + 1)) {
    return(data.frame(
      lag = NA,
      correlation = NA,
      stock = unique(stock_series$ticker)[1],
      prediction_market = unique(pred_series$id)[1]
    ))
  }
  
  # Compute cross-correlation
  ccf_result <- ccf(df_clean$pred_daily_stock, df_clean$pred_daily_pred, 
                   lag.max = max_lag, plot = FALSE)
  
  # Create result dataframe
  result_df <- data.frame(
    lag = ccf_result$lag,
    correlation = ccf_result$acf,
    stock = unique(stock_series$ticker)[1],
    prediction_market = unique(pred_series$id)[1]
  )
  
  return(result_df)
}

# Prepare data for cross-correlation analysis
stock_series_list <- lapply(unique(df_stock$ticker), function(tk) {
  df_stock %>% filter(ticker == tk) %>%
    select(date, ticker, pred_daily = returns)
})
names(stock_series_list) <- unique(df_stock$ticker)

prediction_series_list <- lapply(unique(df_pred_all$id), function(id) {
  df_pred_all %>% filter(id == !!id) %>%
    select(date, id, pred_daily)
})
names(prediction_series_list) <- unique(df_pred_all$id)

# Compute cross-correlations for all combinations
cross_corr_results <- list()
for (stock_name in names(stock_series_list)) {
  for (pred_name in names(prediction_series_list)) {
    result <- compute_cross_correlation(
      stock_series = stock_series_list[[stock_name]],
      pred_series = prediction_series_list[[pred_name]]
    )
    cross_corr_results[[length(cross_corr_results) + 1]] <- result
  }
}

# Combine all results
df_cross_corr <- bind_rows(cross_corr_results)

# Find significant correlations (using 95% confidence interval)
df_cross_corr_significant <- df_cross_corr %>%
  filter(abs(correlation) > 1.96/sqrt(nrow(df_stock))) %>%
  arrange(desc(abs(correlation)))

# Display top correlations
cat("\nTop 10 strongest cross-correlations:\n")
print(head(df_cross_corr_significant, 10))

# Save results
write_csv(df_cross_corr_significant, "data/csv/cross_correlation_results.csv")

################################################################################
# 11. DIAGNOSTIC TESTS
################################################################################

# Function to perform diagnostic tests on a time series
perform_diagnostic_tests <- function(series, series_name) {
  # Create a list to store results
  results <- list()
  
  # 1. Test for Normality (Shapiro-Wilk test)
  shapiro_test <- shapiro.test(series)
  results$normality <- data.frame(
    test = "Shapiro-Wilk",
    p_value = shapiro_test$p.value,
    is_normal = shapiro_test$p.value > 0.05
  )
  
  # 2. Test for Heteroskedasticity (Breusch-Pagan test)
  # Create a simple linear model for the test
  lm_model <- lm(series ~ seq_along(series))
  bp_test <- bptest(lm_model)
  results$heteroskedasticity <- data.frame(
    test = "Breusch-Pagan",
    p_value = bp_test$p.value,
    is_homoskedastic = bp_test$p.value > 0.05
  )
  
  # 3. Test for Structural Breaks (Chow test)
  # Split the series in half
  n <- length(series)
  mid_point <- floor(n/2)
  chow_test <- sctest(series ~ seq_along(series), type = "Chow", point = mid_point)
  results$structural_break <- data.frame(
    test = "Chow",
    p_value = chow_test$p.value,
    has_break = chow_test$p.value < 0.05
  )
  
  # 4. Outlier Detection (using IQR method)
  q1 <- quantile(series, 0.25)
  q3 <- quantile(series, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers <- sum(series < lower_bound | series > upper_bound)
  
  results$outliers <- data.frame(
    test = "IQR",
    n_outliers = outliers,
    percentage = (outliers / length(series)) * 100
  )
  
  # Add series name to all results
  for (i in seq_along(results)) {
    results[[i]]$series_name <- series_name
  }
  
  return(results)
}

# Perform diagnostic tests on stock returns
stock_diagnostics <- list()
for (ticker in unique(df_stock$ticker)) {
  stock_data <- df_stock %>% 
    filter(ticker == !!ticker) %>%
    pull(returns)
  
  stock_diagnostics[[ticker]] <- perform_diagnostic_tests(stock_data, ticker)
}

# Perform diagnostic tests on prediction market data
pred_diagnostics <- list()
for (pred_market in unique(df_pred_all$id)) {
  pred_data <- df_pred_all %>% 
    filter(id == !!pred_market) %>%
    pull(pred_daily)
  
  pred_diagnostics[[pred_market]] <- perform_diagnostic_tests(pred_data, pred_market)
}

# Combine results
combine_diagnostic_results <- function(diagnostics_list) {
  all_results <- list()
  for (i in seq_along(diagnostics_list)) {
    for (j in seq_along(diagnostics_list[[i]])) {
      all_results[[length(all_results) + 1]] <- diagnostics_list[[i]][[j]]
    }
  }
  return(bind_rows(all_results))
}

# Combine and save results
stock_diagnostic_results <- combine_diagnostic_results(stock_diagnostics)
pred_diagnostic_results <- combine_diagnostic_results(pred_diagnostics)

# Save results
write_csv(stock_diagnostic_results, "data/csv/stock_diagnostic_results.csv")
write_csv(pred_diagnostic_results, "data/csv/prediction_market_diagnostic_results.csv")

# Display summary of results
cat("\nSummary of Diagnostic Tests for Stocks:\n")
print(summary(stock_diagnostic_results))

cat("\nSummary of Diagnostic Tests for Prediction Markets:\n")
print(summary(pred_diagnostic_results))

################################################################################
# 12. GRANGER CAUSALITY (PRED TO STOCKS)
################################################################################

# Function to perform Granger causality test
granger_causality_test <- function(pred_series, stock_series, max_lag = 5, pred_name = "Prediction Market", stock_name = "Stock") {
  # Merge series on date
  df_combined <- inner_join(pred_series, stock_series, by = "date", suffix = c("_pred", "_stock"))
  df_clean <- df_combined %>% drop_na(pred_daily_pred, pred_daily_stock)
  
  # Check data sufficiency
  if (nrow(df_clean) < (max_lag + 1)) {
    return(data.frame(
      Prediction_Market = pred_name,
      Stock = stock_name,
      p_value = NA,
      result = "Not enough data"
    ))
  }
  
  # Create time series
  ts_pred <- df_clean$pred_daily_pred
  ts_stock <- df_clean$pred_daily_stock
  
  # Perform Granger test
  test_result <- tryCatch({
    grangertest(ts_stock ~ ts_pred, order = max_lag, data = df_clean)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(test_result)) {
    return(data.frame(
      Prediction_Market = pred_name,
      Stock = stock_name,
      p_value = NA,
      result = "Test failed"
    ))
  }
  
  # Extract and interpret results
  p_value <- test_result$`Pr(>F)`[2]
  result <- ifelse(p_value < 0.05, "Granger-causes", "Does not Granger-cause")
  
  return(data.frame(
    Prediction_Market = pred_name,
    Stock = stock_name,
    p_value = p_value,
    result = result
  ))
}

# Prepare data for Granger causality tests
prediction_markets <- list(
  df_pred_daily_TESLA, df_pred_daily_NETFLIX, df_pred_daily_META, df_pred_daily_GDP,
  df_pred_daily_SpaceX, df_pred_daily_gas_us, df_pred_daily_wti_oil, df_pred_daily_btc,
  df_pred_daily_us_sc, df_pred_daily_infla, df_pred_daily_huricane, df_pred_daily_eth,
  df_pred_daily_measles, df_pred_daily_apple
)

prediction_names <- sapply(prediction_markets, function(df) unique(df$id)[1])
stock_tickers <- unique(df_stock$ticker)

# Create stock series list
stock_series_list <- lapply(stock_tickers, function(tk) {
  df_stock %>% filter(ticker == tk) %>%
    select(date, pred_daily = returns)
})
names(stock_series_list) <- stock_tickers

# Perform Granger causality tests
results <- list()
for (i in seq_along(prediction_markets)) {
  for (j in seq_along(stock_series_list)) {
    result <- granger_causality_test(
      pred_series = prediction_markets[[i]],
      stock_series = stock_series_list[[j]],
      max_lag = 5,
      pred_name = prediction_names[i],
      stock_name = names(stock_series_list)[j]
    )
    results[[length(results) + 1]] <- result
  }
}

# Combine and sort results
df_results <- bind_rows(results)
df_results_sorted <- df_results %>% arrange(p_value)

# Display significant results
print(df_results_sorted %>% filter(!is.na(p_value) & p_value < 0.15))

# Save results
write_csv(df_results_sorted, "data/csv/granger_results_all_combinations.csv")

# Combine predictions with stock data
df_combined <- df_pred_all %>%
  left_join(df_stock, by = c("date")) %>%
  filter(!is.na(returns))

################################################################################
# 13. ARIMA(X) PANEL DATA MODELING
################################################################################

# Create walk-forward time series split
splits <- df_combined %>%
  arrange(date) %>%  # Ensure data is ordered by date
  time_series_cv(
    date_var   = date,
    initial    = "150 days",     # ~5 months
    assess     = "75 days",      # ~2.5 months
    skip       = "75 days",      # Step of 2.5 months
    cumulative = TRUE,          # Keep all previous data in training
    slice_limit = 2             # Limit to 2 folds
  )

# Print splits to verify
print(splits)

# Get both splits for training and testing
first_split <- splits$splits[[1]]
second_split <- splits$splits[[2]]

# Get training and testing data for both splits
train_data_1 <- analysis(first_split)
test_data_1 <- assessment(first_split)
train_data_2 <- analysis(second_split)
test_data_2 <- assessment(second_split)

# Function to evaluate model performance
evaluate_model <- function(model, test_data, ticker) {
  # Validate test data
  if (nrow(test_data) == 0) {
    stop("No test data available")
  }
  
  # Generate forecasts
  if (inherits(model, "ARIMA")) {
    # For ARIMA models, generate forecast for the entire test period
    forecast_values <- forecast(model, h = nrow(test_data))
    forecast_means <- forecast_values$mean
  } else {
    # For ARIMAX models, we need to handle exogenous variables
    exog_test <- test_data %>%
      select(matches("pred_daily")) %>%
      as.matrix()
    
    # Print dimensions for debugging
    cat(sprintf("Test data rows: %d, Exog matrix rows: %d\n", 
                nrow(test_data), nrow(exog_test)))
    
    # Ensure we have the right number of forecasts
    if (nrow(exog_test) != nrow(test_data)) {
      warning(sprintf("Dimension mismatch: test_data=%d, exog_matrix=%d", 
                     nrow(test_data), nrow(exog_test)))
      h <- min(nrow(exog_test), nrow(test_data))
      forecast_values <- forecast(model, xreg = exog_test[1:h,], h = h)
    } else {
      forecast_values <- forecast(model, xreg = exog_test, h = nrow(test_data))
    }
    forecast_means <- forecast_values$mean
  }
  
  # Print forecast information for debugging
  cat(sprintf("Number of forecasts generated: %d\n", length(forecast_means)))
  cat(sprintf("First few forecast values: %s\n", 
              paste(head(forecast_means, 3), collapse = ", ")))
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data$returns - forecast_means)^2))
  
  return(list(
    ticker = ticker,
    rmse = rmse,
    forecast = forecast_means
  ))
}

# Function to fit local model for a specific fold
fit_local_model_fold <- function(ticker_data, granger_results, prediction_markets, fold_number) {
  # Validate input data
  if (nrow(ticker_data) == 0) {
    stop("No data available for this ticker")
  }
  
  current_ticker <- unique(ticker_data$ticker)
  
  # Display Granger test results
  cat(sprintf("\nGranger test results for %s (Fold %d):\n", current_ticker, fold_number))
  granger_for_ticker <- granger_results %>%
    filter(Stock == current_ticker)
  print(granger_for_ticker)
  
  # Extract significant exogenous variables
  exog_vars <- granger_results %>%
    filter(Stock == current_ticker,
           result == "Granger-causes",
           p_value < 0.05) %>%
    pull(Prediction_Market)
  
  cat(sprintf("Significant exogenous variables for %s (Fold %d): %s\n", 
              current_ticker, 
              fold_number,
              if(length(exog_vars) > 0) paste(exog_vars, collapse = ", ") else "none"))
  
  # Prepare time series data
  ts_data <- ts(ticker_data$returns, frequency = 365)
  
  # Fit appropriate model based on exogenous variables
  if (length(exog_vars) > 0) {
    # Prepare exogenous matrix
    exog_matrix <- prediction_markets %>%
      filter(id %in% exog_vars) %>%
      select(date, id, pred_daily) %>%
      group_by(id) %>%
      arrange(date) %>%
      fill(pred_daily, .direction = "down") %>%
      ungroup() %>%
      pivot_wider(names_from = id, values_from = pred_daily) %>%
      arrange(date)
    
    # Print matrix dimensions for debugging
    cat(sprintf("Exogenous matrix dimensions: %d x %d\n", 
                nrow(exog_matrix), ncol(exog_matrix)))
    
    # Align dates
    exog_matrix <- exog_matrix %>%
      filter(date %in% ticker_data$date) %>%
      arrange(date)
    
    # Print aligned dimensions for debugging
    cat(sprintf("Aligned matrix dimensions: %d x %d\n", 
                nrow(exog_matrix), ncol(exog_matrix)))
    
    # Validate exogenous matrix
    if (nrow(exog_matrix) == 0 || ncol(exog_matrix) <= 1) {
      cat(sprintf("No valid exogenous data for %s (Fold %d), using simple ARIMA\n", 
                  current_ticker, fold_number))
      model <- auto.arima(ts_data)
    } else {
      # Check date alignment
      if (!all(exog_matrix$date == ticker_data$date)) {
        warning("Dates not perfectly aligned, adjusting...")
        exog_matrix <- exog_matrix %>%
          right_join(ticker_data %>% select(date), by = "date") %>%
          arrange(date) %>%
          fill(-date, .direction = "down")
      }
      
      # Prepare matrix for modeling
      exog_matrix <- exog_matrix %>%
        select(-date) %>%
        as.matrix()
      
      # Check for NA values
      if (any(is.na(exog_matrix))) {
        warning("NA values detected in exogenous variables, using simple ARIMA")
        model <- auto.arima(ts_data)
      } else {
        # Fit ARIMAX model
        tryCatch({
          model <- auto.arima(ts_data, xreg = exog_matrix)
          # Print model summary for debugging
          cat("Model summary:\n")
          print(summary(model))
        }, error = function(e) {
          warning(paste("Error fitting ARIMAX:", e$message, "\nUsing simple ARIMA"))
          model <- auto.arima(ts_data)
        })
      }
    }
  } else {
    # Use simple ARIMA if no significant exogenous variables
    cat(sprintf("No significant exogenous variables for %s (Fold %d), using simple ARIMA\n", 
                current_ticker, fold_number))
    model <- auto.arima(ts_data)
  }
  
  return(model)
}

# Function to analyze model residuals
analyze_residuals <- function(model, ticker) {
  # Extract residuals
  residuals <- residuals(model)
  
  # Create a list to store results
  results <- list()
  
  # 1. Test for Normality (Shapiro-Wilk)
  shapiro_test <- shapiro.test(residuals)
  results$normality <- data.frame(
    test = "Shapiro-Wilk",
    p_value = shapiro_test$p.value,
    is_normal = shapiro_test$p.value > 0.05
  )
  
  # 2. Test for Autocorrelation (Ljung-Box)
  lb_test <- Box.test(residuals, type = "Ljung-Box")
  results$autocorrelation <- data.frame(
    test = "Ljung-Box",
    p_value = lb_test$p.value,
    is_white_noise = lb_test$p.value > 0.05
  )
  
  # 3. Test for Heteroskedasticity (Breusch-Pagan)
  lm_model <- lm(residuals ~ seq_along(residuals))
  bp_test <- bptest(lm_model)
  results$heteroskedasticity <- data.frame(
    test = "Breusch-Pagan",
    p_value = bp_test$p.value,
    is_homoskedastic = bp_test$p.value > 0.05
  )
  
  # 4. Basic statistics
  results$statistics <- data.frame(
    mean = mean(residuals),
    sd = sd(residuals),
    skewness = skewness(residuals),
    kurtosis = kurtosis(residuals)
  )
  
  # Add ticker to all results
  for (i in seq_along(results)) {
    results[[i]]$ticker <- ticker
  }
  
  return(results)
}

# Function to combine residual analysis results
combine_residual_results <- function(analysis_list) {
  all_results <- list()
  for (i in seq_along(analysis_list)) {
    for (j in seq_along(analysis_list[[i]])) {
      all_results[[length(all_results) + 1]] <- analysis_list[[i]][[j]]
    }
  }
  return(bind_rows(all_results))
}

# Initialize result storage for both folds
local_models_fold1 <- list()
local_models_fold2 <- list()
local_forecasts_fold1 <- list()
local_forecasts_fold2 <- list()
local_rmse_fold1 <- list()
local_rmse_fold2 <- list()
train_forecasts_fold1 <- list()
train_forecasts_fold2 <- list()

# Fit models for each ticker in both folds
for (ticker in unique(df_combined$ticker)) {
  cat(sprintf("\nProcessing %s...\n", ticker))
  
  # Extract ticker data for both folds
  ticker_data_fold1 <- train_data_1 %>% 
    filter(ticker == !!ticker) %>%
    arrange(date)
  
  ticker_data_fold2 <- train_data_2 %>% 
    filter(ticker == !!ticker) %>%
    arrange(date)
  
  # Check data sufficiency
  if (nrow(ticker_data_fold1) < 50 || nrow(ticker_data_fold2) < 50) {
    cat(sprintf("Insufficient data for %s, skipping...\n", ticker))
    next
  }
  
  tryCatch({
    # Fit local models for both folds
    local_model_fold1 <- fit_local_model_fold(
      ticker_data = ticker_data_fold1,
      granger_results = df_results_sorted,
      prediction_markets = df_pred_all,
      fold_number = 1
    )
    
    local_model_fold2 <- fit_local_model_fold(
      ticker_data = ticker_data_fold2,
      granger_results = df_results_sorted,
      prediction_markets = df_pred_all,
      fold_number = 2
    )
    
    # Evaluate models
    evaluation_fold1 <- evaluate_model(
      model = local_model_fold1,
      test_data = test_data_1 %>% filter(ticker == !!ticker),
      ticker = ticker
    )
    
    evaluation_fold2 <- evaluate_model(
      model = local_model_fold2,
      test_data = test_data_2 %>% filter(ticker == !!ticker),
      ticker = ticker
    )
    
    # Generate train set forecasts
    train_forecast_fold1 <- fitted(local_model_fold1)
    train_forecast_fold2 <- fitted(local_model_fold2)
    
    # Store results
    local_models_fold1[[ticker]] <- local_model_fold1
    local_models_fold2[[ticker]] <- local_model_fold2
    local_forecasts_fold1[[ticker]] <- evaluation_fold1$forecast
    local_forecasts_fold2[[ticker]] <- evaluation_fold2$forecast
    local_rmse_fold1[[ticker]] <- evaluation_fold1$rmse
    local_rmse_fold2[[ticker]] <- evaluation_fold2$rmse
    train_forecasts_fold1[[ticker]] <- train_forecast_fold1
    train_forecasts_fold2[[ticker]] <- train_forecast_fold2
    
    cat(sprintf("Models successfully fitted for %s\n", ticker))
  }, error = function(e) {
    cat(sprintf("Error processing %s: %s\n", ticker, e$message))
  })
}

# Create performance summary for both folds
performance_summary <- data.frame(
  ticker = names(local_rmse_fold1),
  local_rmse_fold1 = unlist(local_rmse_fold1),
  local_rmse_fold2 = unlist(local_rmse_fold2)
)

################################################################################
# 14. RESIDUAL ANALYSIS
################################################################################

# Analyze residuals for both folds
residual_analysis_fold1 <- list()
residual_analysis_fold2 <- list()

for (ticker in names(local_models_fold1)) {
  cat(sprintf("\nAnalyzing residuals for %s (Fold 1)...\n", ticker))
  residual_analysis_fold1[[ticker]] <- analyze_residuals(local_models_fold1[[ticker]], ticker)
  
  cat(sprintf("\nAnalyzing residuals for %s (Fold 2)...\n", ticker))
  residual_analysis_fold2[[ticker]] <- analyze_residuals(local_models_fold2[[ticker]], ticker)
}

# Combine results for both folds
residual_results_fold1 <- combine_residual_results(residual_analysis_fold1)
residual_results_fold2 <- combine_residual_results(residual_analysis_fold2)

# Save results
write_csv(residual_results_fold1, "data/csv/residual_analysis_results_fold1.csv")
write_csv(residual_results_fold2, "data/csv/residual_analysis_results_fold2.csv")

################################################################################
# 15. ERROR ANALYSIS BY TIME POINT
################################################################################

# Create forecast error data frames for both folds
local_forecast_errors_fold1 <- data.frame()
local_forecast_errors_fold2 <- data.frame()

# Calculate forecast errors for each ticker in both folds
for (ticker in names(local_forecasts_fold1)) {
  # Get test data for both folds
  test_data_fold1 <- test_data_1 %>% filter(ticker == !!ticker)
  test_data_fold2 <- test_data_2 %>% filter(ticker == !!ticker)
  
  # Process fold 1 errors
  forecast_values_fold1 <- local_forecasts_fold1[[ticker]]
  if (length(forecast_values_fold1) > nrow(test_data_fold1)) {
    forecast_values_fold1 <- forecast_values_fold1[1:nrow(test_data_fold1)]
  }
  
  # Create error data frame for fold 1
  ticker_errors_fold1 <- data.frame(
    date = test_data_fold1$date[1:length(forecast_values_fold1)],
    ticker = ticker,
    y_t = test_data_fold1$returns[1:length(forecast_values_fold1)],
    y_hat_t = forecast_values_fold1,
    fold = "Fold 1"
  )
  
  # Process fold 2 errors
  forecast_values_fold2 <- local_forecasts_fold2[[ticker]]
  if (length(forecast_values_fold2) > nrow(test_data_fold2)) {
    forecast_values_fold2 <- forecast_values_fold2[1:nrow(test_data_fold2)]
  }
  
  # Create error data frame for fold 2
  ticker_errors_fold2 <- data.frame(
    date = test_data_fold2$date[1:length(forecast_values_fold2)],
    ticker = ticker,
    y_t = test_data_fold2$returns[1:length(forecast_values_fold2)],
    y_hat_t = forecast_values_fold2,
    fold = "Fold 2"
  )
  
  # Calculate forecast errors and remove duplicates
  ticker_errors_fold1 <- ticker_errors_fold1 %>%
    mutate(e1_t = y_t - y_hat_t) %>%
    filter(!is.na(y_t) & !is.na(y_hat_t))
  
  ticker_errors_fold2 <- ticker_errors_fold2 %>%
    mutate(e1_t = y_t - y_hat_t) %>%
    filter(!is.na(y_t) & !is.na(y_hat_t))
  
  # Add to main data frames
  local_forecast_errors_fold1 <- bind_rows(local_forecast_errors_fold1, ticker_errors_fold1)
  local_forecast_errors_fold2 <- bind_rows(local_forecast_errors_fold2, ticker_errors_fold2)
}

# Combine errors from both folds
local_forecast_errors <- bind_rows(local_forecast_errors_fold1, local_forecast_errors_fold2)

# Remove any remaining duplicates
local_forecast_errors <- local_forecast_errors %>%
  distinct(date, ticker, fold, .keep_all = TRUE)

# Save results
saveRDS(local_forecast_errors, "data/rds/local_forecast_errors.rds")
saveRDS(list(fold1 = local_models_fold1, fold2 = local_models_fold2), "data/rds/local_models.rds")
saveRDS(list(fold1 = local_forecasts_fold1, fold2 = local_forecasts_fold2), "data/rds/local_forecasts.rds")
saveRDS(performance_summary, "data/rds/local_performance_summary.rds")

################################################################################
# 16. VISUALIZATION FUNCTIONS
################################################################################

# Function to plot training results for a specific fold
plot_train_results_fold <- function(ticker, fold_number) {
  if (fold_number == 1) {
    train_data <- train_data_1 %>% filter(ticker == !!ticker)
    test_data <- test_data_1 %>% filter(ticker == !!ticker)
    train_forecast <- train_forecasts_fold1[[ticker]]
  } else {
    train_data <- train_data_2 %>% filter(ticker == !!ticker)
    test_data <- test_data_2 %>% filter(ticker == !!ticker)
    train_forecast <- train_forecasts_fold2[[ticker]]
  }
  
  # Create training forecast data frame
  train_forecast_df <- data.frame(
    date = train_data$date[1:length(train_forecast)],
    forecast = train_forecast
  )
  
  # Calculate y-axis limits
  y_min <- min(c(train_data$returns, test_data$returns, train_forecast_df$forecast), na.rm = TRUE)
  y_max <- max(c(train_data$returns, test_data$returns, train_forecast_df$forecast), na.rm = TRUE)
  
  # Create plot
  p <- ggplot() +
    geom_line(data = train_data, aes(x = date, y = returns, color = "Actual (train)"), linewidth = 0.8) +
    geom_line(data = train_forecast_df, 
              aes(x = date, y = forecast, color = "Forecast (train)"), linewidth = 0.8) +
    geom_line(data = test_data, aes(x = date, y = returns, color = "Actual (test)"), linewidth = 0.8) +
    labs(title = paste("Model Results for", ticker, "- Fold", fold_number),
         x = "Date",
         y = "Returns",
         color = "Legend") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    ylim(y_min, y_max)
  
  return(p)
}

# Function to plot future forecasts for a specific fold
plot_future_forecasts_fold <- function(ticker, fold_number) {
  if (fold_number == 1) {
    train_data <- train_data_1 %>% filter(ticker == !!ticker)
    test_data <- test_data_1 %>% filter(ticker == !!ticker)
    model <- local_models_fold1[[ticker]]
  } else {
    train_data <- train_data_2 %>% filter(ticker == !!ticker)
    test_data <- test_data_2 %>% filter(ticker == !!ticker)
    model <- local_models_fold2[[ticker]]
  }
  
  # Generate future dates and forecasts
  future_dates <- seq(max(test_data$date), by = "day", length.out = 60)
  if (inherits(model, "ARIMA")) {
    future_forecast <- forecast(model, h = 60)
  } else {
    last_exog <- test_data %>%
      select(matches("pred_daily")) %>%
      tail(1) %>%
      as.matrix()
    future_forecast <- forecast(model, 
                              xreg = matrix(rep(last_exog, 60), ncol = ncol(last_exog), byrow = TRUE), 
                              h = 60)
  }
  
  # Calculate y-axis limits
  y_min <- min(c(train_data$returns, test_data$returns, future_forecast$mean), na.rm = TRUE)
  y_max <- max(c(train_data$returns, test_data$returns, future_forecast$mean), na.rm = TRUE)
  
  # Create plot
  p <- ggplot() +
    geom_line(data = train_data, aes(x = date, y = returns, color = "Actual (train)"), linewidth = 0.8) +
    geom_line(data = test_data, aes(x = date, y = returns, color = "Actual (test)"), linewidth = 0.8) +
    geom_line(data = data.frame(date = future_dates, forecast = future_forecast$mean), 
              aes(x = date, y = forecast, color = "Future Forecast"), linewidth = 0.8) +
    geom_ribbon(data = data.frame(date = future_dates, 
                                 lower = future_forecast$lower[,2], 
                                 upper = future_forecast$upper[,2]),
                aes(x = date, ymin = lower, ymax = upper), alpha = 0.2) +
    labs(title = paste("Future Forecasts for", ticker, "- Fold", fold_number),
         x = "Date",
         y = "Returns",
         color = "Legend") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    ylim(y_min, y_max)
  
  return(p)
}


