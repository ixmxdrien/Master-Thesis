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
df_pred_daily_apple <- apply_kalman_filter(df_pred_daily_apple)

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
# 13. ARIMA(X) PANEL DATA MODELING WITH CROSS-VALIDATION
################################################################################

# Calculate the total number of days in the dataset
total_days <- as.numeric(max(df_combined$date) - min(df_combined$date))
half_days <- floor(total_days / 2)

# Function to create time series cross-validation splits
create_cv_splits <- function(data, ticker) {
  # Filter data for current ticker
  ticker_data <- data %>% 
    filter(ticker == !!ticker) %>%
    arrange(date)  # Ensure data is ordered by date
  
  # Check if we have enough data
  if (nrow(ticker_data) < 456) {
    warning(sprintf("Insufficient data for %s (n=%d). Minimum required is 456 rows.", 
                   ticker, nrow(ticker_data)))
    return(NULL)
  }
  
  # Calculate split sizes
  total_days <- nrow(ticker_data)
  half_days <- floor(total_days / 2)
  
  # Create exactly 2 splits
  splits <- list()
  
  # First split: first half for training, second half for testing
  split1 <- make_splits(
    list(analysis = 1:half_days, assessment = (half_days + 1):total_days),
    data = ticker_data
  )
  
  # Second split: second half for training, first half for testing
  split2 <- make_splits(
    list(analysis = (half_days + 1):total_days, assessment = 1:half_days),
    data = ticker_data
  )
  
  splits <- list(split1, split2)
  
  # Create rset object with simplified fold names
  splits <- new_rset(
    splits = splits,
    ids = c("1", "2")
  )
  
  return(splits)
}

# Function to fit ARIMA(X) model for a given split
fit_arima_model <- function(split, granger_results, prediction_markets) {
  # Get training and testing data
  train_data <- analysis(split)
  test_data <- assessment(split)
  
  # Check for duplicate dates in test data
  duplicate_dates <- test_data %>%
    group_by(date) %>%
    filter(n() > 1) %>%
    arrange(date)
  
  if (nrow(duplicate_dates) > 0) {
    warning(sprintf("Found %d duplicate dates in test data", nrow(duplicate_dates)))
    print(duplicate_dates)
    # Keep only the first occurrence of each date
    test_data <- test_data %>%
      distinct(date, .keep_all = TRUE)
  }
  
  # Get current ticker
  current_ticker <- unique(train_data$ticker)
  
  # Display Granger test results
  cat(sprintf("\nGranger test results for %s:\n", current_ticker))
  granger_for_ticker <- granger_results %>%
    filter(Stock == current_ticker)
  print(granger_for_ticker)
  
  # Extract significant exogenous variables from Granger results
  exog_vars <- granger_results %>%
    filter(Stock == current_ticker,
           result == "Granger-causes",
           p_value < 0.05) %>%
    pull(Prediction_Market)
  
  cat(sprintf("Significant exogenous variables for %s: %s\n", 
              current_ticker, 
              if(length(exog_vars) > 0) paste(exog_vars, collapse = ", ") else "none"))
  
  # Prepare time series data
  ts_data <- ts(train_data$returns, frequency = 365)
  
  # Initialize model and exog_matrix variables
  model <- NULL
  exog_matrix <- NULL
  
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
    
    # Check for duplicate dates in exogenous matrix
    duplicate_exog_dates <- exog_matrix %>%
      group_by(date) %>%
      filter(n() > 1) %>%
      arrange(date)
    
    if (nrow(duplicate_exog_dates) > 0) {
      warning(sprintf("Found %d duplicate dates in exogenous matrix", nrow(duplicate_exog_dates)))
      print(duplicate_exog_dates)
      # Keep only the first occurrence of each date
      exog_matrix <- exog_matrix %>%
        distinct(date, .keep_all = TRUE)
    }
    
    # Display matrix dimensions
    cat(sprintf("Exogenous matrix dimensions for %s: %d x %d\n", 
                current_ticker, nrow(exog_matrix), ncol(exog_matrix)))
    
    # Align dates
    exog_matrix <- exog_matrix %>%
      filter(date %in% train_data$date) %>%
      arrange(date)
    
    cat(sprintf("Dimensions after alignment for %s: %d x %d\n", 
                current_ticker, nrow(exog_matrix), ncol(exog_matrix)))
    
    # Validate exogenous matrix
    if (nrow(exog_matrix) == 0 || ncol(exog_matrix) <= 1) {
      cat(sprintf("No valid exogenous data for %s, using simple ARIMA\n", current_ticker))
      model <- auto.arima(ts_data)
    } else {
      # Check date alignment
      if (!all(exog_matrix$date == train_data$date)) {
        warning("Dates not perfectly aligned, adjusting...")
        exog_matrix <- exog_matrix %>%
          right_join(train_data %>% select(date), by = "date") %>%
          arrange(date) %>%
          fill(-date, .direction = "down")
      }
      
      # Prepare matrix for modeling
      exog_matrix <- exog_matrix %>%
        select(-date) %>%
        as.matrix()
      
      # Adjust dimensions if needed
      if (nrow(exog_matrix) != length(ts_data)) {
        warning(sprintf("Dimension mismatch: ts_data=%d, exog_matrix=%d", 
                       length(ts_data), nrow(exog_matrix)))
        ts_data <- ts(train_data$returns[1:nrow(exog_matrix)], frequency = 365)
      }
      
      # Check for NA values
      if (any(is.na(exog_matrix))) {
        warning("NA values detected in exogenous variables, using simple ARIMA")
        model <- auto.arima(ts_data)
      } else {
        # Check for informative variables
        if (ncol(exog_matrix) == 0 || all(apply(exog_matrix, 2, function(x) length(unique(x)) == 1))) {
          warning("Non-informative exogenous variables, using simple ARIMA")
          model <- auto.arima(ts_data)
        } else {
          # Fit ARIMAX model
          tryCatch({
            model <- auto.arima(ts_data, xreg = exog_matrix)
            cat(sprintf("Successfully fitted ARIMAX model for %s\n", current_ticker))
          }, error = function(e) {
            warning(paste("Error fitting ARIMAX:", e$message, "\nUsing simple ARIMA"))
            model <- auto.arima(ts_data)
          })
        }
      }
    }
  } else {
    # Use simple ARIMA if no significant exogenous variables
    cat(sprintf("No significant exogenous variables for %s, using simple ARIMA\n", current_ticker))
    model <- auto.arima(ts_data)
  }
  
  # Generate predictions
  if (is.null(model)) {
    stop("Model fitting failed")
  }
  
  # Check if model is ARIMAX
  is_arimax <- !is.null(model$xreg)
  
  if (!is_arimax) {
    predictions <- forecast(model, h = nrow(test_data))
  } else {
    # Prepare exogenous matrix for test data
    exog_test <- prediction_markets %>%
      filter(id %in% exog_vars) %>%
      select(date, id, pred_daily) %>%
      group_by(id) %>%
      arrange(date) %>%
      fill(pred_daily, .direction = "down") %>%
      ungroup() %>%
      pivot_wider(names_from = id, values_from = pred_daily) %>%
      arrange(date) %>%
      filter(date %in% test_data$date) %>%
      select(-date) %>%
      as.matrix()
    
    # Check for duplicate dates in test exogenous matrix
    if (nrow(exog_test) != nrow(test_data)) {
      warning(sprintf("Dimension mismatch in test exogenous matrix: exog_test=%d, test_data=%d", 
                     nrow(exog_test), nrow(test_data)))
    }
    
    # Check if we have valid exogenous data for prediction
    if (nrow(exog_test) == 0 || ncol(exog_test) == 0) {
      warning("No valid exogenous data for prediction, using simple ARIMA forecast")
      predictions <- forecast(model, h = nrow(test_data))
    } else {
      # Ensure dimensions match
      if (nrow(exog_test) != nrow(test_data)) {
        warning("Dimension mismatch in test data, adjusting...")
        h <- min(nrow(exog_test), nrow(test_data))
        predictions <- forecast(model, xreg = exog_test[1:h,], h = h)
      } else {
        predictions <- forecast(model, xreg = exog_test, h = nrow(test_data))
      }
    }
  }
  
  # Ensure predictions and test data have the same length
  min_length <- min(length(predictions$mean), nrow(test_data))
  predictions$mean <- predictions$mean[1:min_length]
  test_data <- test_data[1:min_length,]
  
  # Calculate accuracy metrics
  accuracy_metrics <- data.frame(
    ticker = current_ticker,
    fold = as.numeric(split$id),  # Convert fold ID to numeric
    rmse = sqrt(mean((test_data$returns - predictions$mean)^2)),
    mae = mean(abs(test_data$returns - predictions$mean)),
    mape = mean(abs((test_data$returns - predictions$mean)/test_data$returns)) * 100,
    mase = mean(abs(test_data$returns - predictions$mean)) / mean(abs(diff(test_data$returns))),
    maape = mean(atan(abs((test_data$returns - predictions$mean)/test_data$returns))) * 180/pi
  )
  
  # Generate forecast errors
  forecast_errors <- data.frame(
    date = test_data$date,
    ticker = current_ticker,
    forecast_error = test_data$returns - predictions$mean,
    fold = as.numeric(split$id)  # Convert fold ID to numeric
  )
  
  # Log the number of unique combinations
  cat(sprintf("\nNumber of unique date/ticker/fold combinations: %d\n", 
              nrow(forecast_errors %>% distinct(date, ticker, fold))))
  
  return(list(
    model = model,
    accuracy = accuracy_metrics,
    forecast_errors = forecast_errors
  ))
}

# Initialize storage for results
cv_results <- list()
accuracy_results <- list()
forecast_errors <- list()

# Perform cross-validation for each ticker
for (ticker in unique(df_combined$ticker)) {
  cat(sprintf("\nProcessing %s...\n", ticker))
  
  # Create CV splits for current ticker
  cv_splits <- create_cv_splits(df_combined, ticker)
  
  # Skip if insufficient data
  if (is.null(cv_splits)) {
    cat(sprintf("Skipping %s due to insufficient data\n", ticker))
    next
  }
  
  # Fit models for each split
  ticker_results <- map(cv_splits$splits, ~fit_arima_model(
    split = .x,
    granger_results = df_results_sorted,
    prediction_markets = df_pred_all
  ))
  
  # Store results
  cv_results[[ticker]] <- ticker_results
  accuracy_results[[ticker]] <- map_dfr(ticker_results, ~.x$accuracy)
  forecast_errors[[ticker]] <- map_dfr(ticker_results, ~.x$forecast_errors)
}

# Combine all results
all_accuracy_results <- bind_rows(accuracy_results)
all_forecast_errors <- bind_rows(forecast_errors)

# Print accuracy results
if (nrow(all_accuracy_results) > 0) {
  cat("\nOverall Accuracy Metrics by Ticker and Fold:\n")
  print(
    kable(all_accuracy_results, 
          caption = "Accuracy Metrics by Ticker and Fold",
          digits = 4) %>%
      kable_styling(bootstrap_options = c("striped", "hover"))
  )
} else {
  cat("\nNo results to display - all tickers were skipped due to insufficient data.\n")
}

# Save results
saveRDS(cv_results, "data/rds/local_cv_results.rds")
saveRDS(all_accuracy_results, "data/rds/local_accuracy_results.rds")
saveRDS(all_forecast_errors, "data/rds/local_forecast_errors.rds")

################################################################################
# 14. RESIDUAL ANALYSIS
################################################################################

# Function to analyze model residuals
analyze_residuals <- function(model, ticker, fold) {
  # Extract residuals
  residuals <- residuals(model)
  
  # Create a list to store results
  results <- list()
  
  # 1. Test for Normality (Shapiro-Wilk)
  shapiro_test <- shapiro.test(residuals)
  results$normality <- data.frame(
    test = "Shapiro-Wilk",
    p_value = shapiro_test$p.value,
    is_normal = shapiro_test$p.value > 0.05,
    ticker = ticker,
    fold = fold
  )
  
  # 2. Test for Autocorrelation (Ljung-Box)
  lb_test <- Box.test(residuals, type = "Ljung-Box")
  results$autocorrelation <- data.frame(
    test = "Ljung-Box",
    p_value = lb_test$p.value,
    is_white_noise = lb_test$p.value > 0.05,
    ticker = ticker,
    fold = fold
  )
  
  # 3. Test for Heteroskedasticity (Breusch-Pagan)
  lm_model <- lm(residuals ~ seq_along(residuals))
  bp_test <- bptest(lm_model)
  results$heteroskedasticity <- data.frame(
    test = "Breusch-Pagan",
    p_value = bp_test$p.value,
    is_homoskedastic = bp_test$p.value > 0.05,
    ticker = ticker,
    fold = fold
  )
  
  # 4. Basic statistics
  results$statistics <- data.frame(
    mean = mean(residuals),
    sd = sd(residuals),
    skewness = skewness(residuals),
    kurtosis = kurtosis(residuals),
    ticker = ticker,
    fold = fold
  )
  
  return(results)
}

# Analyze residuals for all models in cv_results
residual_analysis <- list()
for (ticker in names(cv_results)) {
  cat(sprintf("\nAnalyzing residuals for %s...\n", ticker))
  
  # Get results for both folds
  for (fold_idx in seq_along(cv_results[[ticker]])) {
    fold_results <- cv_results[[ticker]][[fold_idx]]
    if (!is.null(fold_results$model)) {
      residual_analysis[[paste(ticker, "fold", fold_idx, sep = "_")]] <- 
        analyze_residuals(fold_results$model, ticker, fold_idx)
    }
  }
}

# Combine results
combine_residual_results <- function(analysis_list) {
  all_results <- list()
  for (i in seq_along(analysis_list)) {
    for (j in seq_along(analysis_list[[i]])) {
      all_results[[length(all_results) + 1]] <- analysis_list[[i]][[j]]
    }
  }
  return(bind_rows(all_results))
}

# Combine and save results
residual_results <- combine_residual_results(residual_analysis)

# Save results
write_csv(residual_results, "data/csv/residual_analysis_results.csv")

# Display summary of results by ticker and fold
cat("\nSummary of Residual Analysis by Ticker and Fold:\n")
print(
  residual_results %>%
    group_by(ticker, fold) %>%
    summarise(
      normality_p_value = mean(p_value[test == "Shapiro-Wilk"]),
      is_normal = all(is_normal[test == "Shapiro-Wilk"]),
      autocorrelation_p_value = mean(p_value[test == "Ljung-Box"]),
      is_white_noise = all(is_white_noise[test == "Ljung-Box"]),
      heteroskedasticity_p_value = mean(p_value[test == "Breusch-Pagan"]),
      is_homoskedastic = all(is_homoskedastic[test == "Breusch-Pagan"]),
      mean_residual = mean(mean),
      sd_residual = mean(sd),
      mean_skewness = mean(skewness),
      mean_kurtosis = mean(kurtosis)
    ) %>%
    arrange(ticker, fold)
)

# Create residual plots for each model and fold
for (ticker in names(cv_results)) {
  for (fold_idx in seq_along(cv_results[[ticker]])) {
    fold_results <- cv_results[[ticker]][[fold_idx]]
    if (!is.null(fold_results$model)) {
      # Create plot
      p <- ggplot(data.frame(residuals = residuals(fold_results$model))) +
        geom_histogram(aes(x = residuals), bins = 30) +
        geom_density(aes(x = residuals), color = "red") +
        labs(title = paste("Residual Distribution for", ticker, "- Fold", fold_idx),
             x = "Residuals",
             y = "Count") +
        theme_minimal()
      
      # Save plot
      ggsave(paste0("plots/residuals_", ticker, "_fold", fold_idx, ".png"), p, width = 10, height = 6)
      
      # Create ACF plot
      acf_plot <- ggAcf(residuals(fold_results$model)) +
        labs(title = paste("ACF of Residuals for", ticker, "- Fold", fold_idx)) +
        theme_minimal()
      
      # Save ACF plot
      ggsave(paste0("plots/acf_residuals_", ticker, "_fold", fold_idx, ".png"), acf_plot, width = 10, height = 6)
    }
  }
}

################################################################################
# 15. ERROR ANALYSIS BY TIME POINT
################################################################################


################################################################################
# 16. VISUALIZATION FUNCTIONS
################################################################################

################################################################################
# 16. FORECASTING WITH ARIMA(X) MODELS
################################################################################

# Function to generate forecasts for a given model and ticker
generate_forecasts <- function(model, ticker, test_data, prediction_markets, exog_vars) {
  # Generate future dates
  future_dates <- seq(max(test_data$date), by = "day", length.out = 60)
  
  # Check if model is ARIMAX
  is_arimax <- !is.null(model$xreg)
  
  if (!is_arimax) {
    # Simple ARIMA forecast
    future_forecast <- forecast(model, h = 60)
  } else {
    # Get the original xreg names from the model
    original_xreg_names <- colnames(model$xreg)
    
    # Prepare exogenous matrix for future predictions
    exog_future <- prediction_markets %>%
      filter(id %in% exog_vars) %>%
      select(date, id, pred_daily) %>%
      group_by(id) %>%
      arrange(date) %>%
      fill(pred_daily, .direction = "down") %>%
      ungroup() %>%
      pivot_wider(names_from = id, values_from = pred_daily) %>%
      arrange(date) %>%
      filter(date %in% future_dates)
    
    # If no valid exogenous data, use simple ARIMA forecast
    if (nrow(exog_future) == 0 || ncol(exog_future) <= 1) {
      warning(sprintf("No valid exogenous data for future predictions of %s, using simple ARIMA forecast", ticker))
      future_forecast <- forecast(model, h = 60)
    } else {
      # Get the last known values for each exogenous variable
      last_exog_values <- test_data %>%
        select(all_of(original_xreg_names)) %>%
        tail(1)
      
      # Create future exogenous matrix with the same structure as training data
      exog_future_matrix <- matrix(NA, nrow = 60, ncol = length(original_xreg_names))
      colnames(exog_future_matrix) <- original_xreg_names
      
      # Fill with last known values
      for (i in 1:length(original_xreg_names)) {
        exog_future_matrix[, i] <- last_exog_values[[original_xreg_names[i]]]
      }
      
      # Generate forecast with exogenous variables
      tryCatch({
        future_forecast <- forecast(model, xreg = exog_future_matrix, h = 60)
      }, error = function(e) {
        warning(sprintf("Error in ARIMAX forecast for %s: %s. Falling back to simple ARIMA.", ticker, e$message))
        future_forecast <- forecast(model, h = 60)
      })
    }
  }
  
  # Create forecast data frame
  forecast_df <- data.frame(
    date = future_dates,
    ticker = ticker,
    forecast = future_forecast$mean,
    lower_80 = future_forecast$lower[,1],
    upper_80 = future_forecast$upper[,1],
    lower_95 = future_forecast$lower[,2],
    upper_95 = future_forecast$upper[,2]
  )
  
  return(forecast_df)
}

# Generate forecasts for each ticker
future_forecasts <- list()
for (ticker in names(cv_results)) {
  cat(sprintf("\nGenerating forecasts for %s...\n", ticker))
  
  # Get the last fold's model (most recent)
  last_fold_model <- cv_results[[ticker]][[length(cv_results[[ticker]])]]$model
  
  # Get the last fold's test data from the combined dataset
  last_fold_test <- df_combined %>%
    filter(ticker == !!ticker) %>%
    arrange(date) %>%
    tail(n = 60)  # Use last 60 days as test data
  
  # Get significant exogenous variables for this ticker
  exog_vars <- df_results_sorted %>%
    filter(Stock == ticker,
           result == "Granger-causes",
           p_value < 0.05) %>%
    pull(Prediction_Market)
  
  # Generate forecasts
  future_forecasts[[ticker]] <- generate_forecasts(
    model = last_fold_model,
    ticker = ticker,
    test_data = last_fold_test,
    prediction_markets = df_pred_all,
    exog_vars = exog_vars
  )
}

# Combine all forecasts
all_future_forecasts <- bind_rows(future_forecasts)

# Create visualization function
plot_future_forecasts <- function(forecast_data, ticker) {
  # Get historical data
  historical_data <- df_combined %>%
    filter(ticker == !!ticker) %>%
    select(date, returns) %>%
    rename(value = returns)
  
  # Get forecast data for this ticker
  ticker_forecast <- forecast_data %>%
    filter(ticker == !!ticker)
  
  # Create plot
  p <- ggplot() +
    # Historical data
    geom_line(data = historical_data, 
              aes(x = date, y = value, color = "Historical"), 
              linewidth = 0.8) +
    # Forecast
    geom_line(data = ticker_forecast, 
              aes(x = date, y = forecast, color = "Forecast"), 
              linewidth = 0.8) +
    # Confidence intervals
    geom_ribbon(data = ticker_forecast,
                aes(x = date, ymin = lower_95, ymax = upper_95),
                fill = "blue", alpha = 0.1) +
    geom_ribbon(data = ticker_forecast,
                aes(x = date, ymin = lower_80, ymax = upper_80),
                fill = "blue", alpha = 0.2) +
    # Labels and theme
    labs(title = paste("60-Day Forecast for", ticker),
         x = "Date",
         y = "Returns",
         color = "Legend") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

# Create and save plots for each ticker
for (ticker in names(future_forecasts)) {
  p <- plot_future_forecasts(all_future_forecasts, ticker)
  ggsave(paste0("plots/future_forecast_", ticker, ".png"), p, width = 12, height = 6)
}

# Save forecast results
saveRDS(all_future_forecasts, "data/rds/local_future_forecasts.rds")

# Print summary of forecasts
cat("\nSummary of 60-day forecasts:\n")
print(
  all_future_forecasts %>%
    group_by(ticker) %>%
    summarise(
      mean_forecast = mean(forecast),
      min_forecast = min(forecast),
      max_forecast = max(forecast),
      forecast_range = max_forecast - min_forecast
    ) %>%
    arrange(ticker)
)
