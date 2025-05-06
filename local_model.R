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
# 6. STATIONARITY CHECK PREDICTION MARKET
################################################################################

check_stationarity <- function(df, ticker) {

  df_clean <- df[!is.na(df$pred_daily), ]
  
  adf_result <- adf.test(df_clean$pred_daily, alternative = "stationary")
  
  cat(paste("ADF test for", ticker, "p-value:", adf_result$p.value, "\n"))
  
  if (adf_result$p.value > 0.05) {
    cat(paste(ticker, "is not stationary. Applying first differencing...\n"))
    df$pred_daily <- c(NA, diff(df$pred_daily, differences = 1))  
    
    df_clean <- df[!is.na(df$pred_daily), ]
    
    adf_result_diff1 <- adf.test(df_clean$pred_daily, alternative = "stationary")
    cat(paste("ADF test after first differencing for", ticker, "p-value:", adf_result_diff1$p.value, "\n"))
    
    # Si la série est toujours non stationnaire, appliquer une seconde différenciation
    if (adf_result_diff1$p.value > 0.05) {
      cat(paste(ticker, "is still not stationary. Applying second differencing...\n"))
      df$pred_daily <- c(NA, diff(df$pred_daily, differences = 1))  # Différence supplémentaire, ajout de NA
      
      # Vérifier s'il y a des NA et les supprimer avant le test ADF
      df_clean <- df[!is.na(df$pred_daily), ]
      
      # Refaites le test après la seconde différenciation
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


# Appliquer la fonction à chaque série
df_pred_daily_TESLA <- check_stationarity(df_pred_daily_TESLA, "TESLA") %>% na.omit() # DIFF = 1
df_pred_daily_NETFLIX <- check_stationarity(df_pred_daily_NETFLIX, "NETFLIX") %>% na.omit() # DIFF = 1
df_pred_daily_META <- check_stationarity(df_pred_daily_META, "META") %>% na.omit() # DIFF = 0
df_pred_daily_GDP <- check_stationarity(df_pred_daily_GDP, "GDP") %>% na.omit() # DIFF = 1
df_pred_daily_SpaceX <- check_stationarity(df_pred_daily_SpaceX, "SpaceX") %>% na.omit() # DIFF = 1
df_pred_daily_gas_us <- check_stationarity(df_pred_daily_gas_us, "Gas US") %>% na.omit() # DIFF = 1
df_pred_daily_wti_oil <- check_stationarity(df_pred_daily_wti_oil, "WTI Oil") %>% na.omit() # DIFF = 0
df_pred_daily_btc <- check_stationarity(df_pred_daily_btc, "BTC") %>% na.omit() # DIFF = 1
df_pred_daily_us_sc <- check_stationarity(df_pred_daily_us_sc, "US Semi Conductor") %>% na.omit() # DIFF = 1
df_pred_daily_infla <- check_stationarity(df_pred_daily_infla, "Inflation") %>% na.omit() # DIFF = 1
df_pred_daily_huricane <- check_stationarity(df_pred_daily_huricane, "Hurricanes") %>% na.omit() # DIFF = 1
df_pred_daily_eth <- check_stationarity(df_pred_daily_eth, "ETH") %>% na.omit() # DIFF = 1
df_pred_daily_measles <- check_stationarity(df_pred_daily_measles, "Measles") %>% na.omit() # DIFF = 1
df_pred_daily_apple <- check_stationarity(df_pred_daily_apple, "Apple") %>% na.omit() # DIFF = 0


# Fusionner tous les DataFrames en un seul si besoin
df_pred_all <- bind_rows(
  df_pred_daily_TESLA,
  df_pred_daily_NETFLIX,
  df_pred_daily_META,
  df_pred_daily_GDP,
  df_pred_daily_SpaceX,
  df_pred_daily_gas_us,
  df_pred_daily_wti_oil,
  df_pred_daily_btc,
  df_pred_daily_us_sc,
  df_pred_daily_infla,   
  df_pred_daily_huricane,
  df_pred_daily_eth,
  df_pred_daily_measles,
  df_pred_daily_apple
)

head(df_pred_all)


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
# 8. STATIONARITY CHECK STOCKS
################################################################################


check_stationarity <- function(df, ticker) {
  df_clean <- df[!is.na(df$returns), ]
  
  adf_result <- adf.test(df_clean$returns, alternative = "stationary")
  
  cat(paste("ADF test for", ticker, "p-value:", adf_result$p.value, "\n"))
  
  if (adf_result$p.value > 0.05) {
    cat(paste(ticker, "is not stationary. Applying first differencing...\n"))
    df$returns <- c(NA, diff(df$returns, differences = 1))  # Applique la première différenciation
    
    # Vérifier s'il y a des NA et les supprimer avant le test ADF
    df_clean <- df[!is.na(df$returns), ]
    
    # Refaire le test après la première différenciation
    adf_result_diff1 <- adf.test(df_clean$returns, alternative = "stationary")
    cat(paste("ADF test after first differencing for", ticker, "p-value:", adf_result_diff1$p.value, "\n"))
    
    # Si la série est toujours non stationnaire, appliquer une seconde différenciation
    if (adf_result_diff1$p.value > 0.05) {
      cat(paste(ticker, "is still not stationary. Applying second differencing...\n"))
      df$returns <- c(NA, diff(df$returns, differences = 1))  # Différence supplémentaire
      
      # Vérifier s'il y a des NA et les supprimer avant le test ADF
      df_clean <- df[!is.na(df$returns), ]
      
      # Refaites le test après la seconde différenciation
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

# Tester la stationnarité pour chaque ticker et afficher les résultats
results <- list()

for (ticker in unique(df_stock$ticker)) {
  # Filtrer les données pour chaque ticker
  stock_data <- df_stock %>% filter(ticker == ticker) %>% select(returns) %>% unlist()
  
  # Tester la stationnarité et appliquer les différenciations si nécessaire
  df_stock <- check_stationarity(df_stock, ticker)
}

################################################################################
# 9. GRANGER CAUSALITY (PRED TO STOCKS)
################################################################################


granger_causality_test <- function(pred_series, stock_series, max_lag = 5, pred_name = "Prediction Market", stock_name = "Stock") {
  library(lmtest)
  
  # Fusionner les deux séries sur la date
  df_combined <- inner_join(pred_series, stock_series, by = "date", suffix = c("_pred", "_stock"))
  
  # Supprimer les NA restants
  df_clean <- df_combined %>% drop_na(pred_daily_pred, pred_daily_stock)
  
  if (nrow(df_clean) < (max_lag + 1)) {
    return(data.frame(
      Prediction_Market = pred_name,
      Stock = stock_name,
      p_value = NA,
      result = "Not enough data"
    ))
  }
  
  # Créer les deux séries temporelles
  ts_pred <- df_clean$pred_daily_pred
  ts_stock <- df_clean$pred_daily_stock
  
  # Appliquer le test de Granger
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
  
  # Extraire la p-value
  p_value <- test_result$`Pr(>F)`[2]
  result <- ifelse(p_value < 0.05, "Granger-causes", "Does not Granger-cause")
  
  return(data.frame(
    Prediction_Market = pred_name,
    Stock = stock_name,
    p_value = p_value,
    result = result
  ))
}


# Liste des Prediction Markets
prediction_markets <- list(
  df_pred_daily_TESLA,
  df_pred_daily_NETFLIX,
  df_pred_daily_META,
  df_pred_daily_GDP,
  df_pred_daily_SpaceX,
  df_pred_daily_gas_us,
  df_pred_daily_wti_oil,
  df_pred_daily_btc,
  df_pred_daily_us_sc,
  df_pred_daily_infla,
  df_pred_daily_huricane,
  df_pred_daily_eth,
  df_pred_daily_measles,
  df_pred_daily_apple
)

# Récupérer les noms pour les associer
prediction_names <- sapply(prediction_markets, function(df) unique(df$id)[1])

# Créer une liste de séries stock par ticker
stock_tickers <- unique(df_stock$ticker)
stock_series_list <- lapply(stock_tickers, function(tk) {
  df_stock %>% filter(ticker == tk) %>%
    select(date, pred_daily = returns)  # on suppose que la colonne 'close' contient le prix
})
names(stock_series_list) <- stock_tickers

# Lancer les tests
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

# Combiner tous les résultats en un seul tableau
df_results <- bind_rows(results)

# Trier par p-value croissante
df_results_sorted <- df_results %>% arrange(p_value)

# Afficher les résultats significatifs
print(df_results_sorted %>% filter(!is.na(p_value) & p_value < 0.15))



write_csv(df_results_sorted, "granger_results_all_combinations.csv")


# Effectuer un left_join entre les prédictions et les actions
df_combined <- df_pred_all %>%
  left_join(df_stock, by = c("date"))

# Vérifier les premières lignes du DataFrame combiné
head(df_combined)

# Supprimer les lignes avec NA returns
df_combined <- df_combined %>%
  filter(!is.na(returns))



#############################################################################################
################################# ARIMA(X) Panel Data Modeling ##############################
#############################################################################################

# Fonction pour ajuster le modèle local
fit_local_model <- function(ticker_data, granger_results, prediction_markets) {
  # Vérifier si les données sont valides
  if (nrow(ticker_data) == 0) {
    stop("Aucune donnée disponible pour ce ticker")
  }
  
  current_ticker <- unique(ticker_data$ticker)
  
  # Afficher les résultats du test de Granger pour ce ticker
  cat(sprintf("\nRésultats du test de Granger pour %s:\n", current_ticker))
  granger_for_ticker <- granger_results %>%
    filter(Stock == current_ticker)
  print(granger_for_ticker)
  
  # Extraire les variables exogènes significatives pour ce ticker
  exog_vars <- granger_results %>%
    filter(Stock == current_ticker,
           result == "Granger-causes",
           p_value < 0.05) %>%
    pull(Prediction_Market)
  
  cat(sprintf("Variables exogènes significatives trouvées pour %s: %s\n", 
              current_ticker, 
              if(length(exog_vars) > 0) paste(exog_vars, collapse = ", ") else "aucune"))
  
  # Préparer les données pour le modèle
  ts_data <- ts(ticker_data$returns, frequency = 365)
  
  # Si des variables exogènes sont significatives, utiliser ARIMAX
  if (length(exog_vars) > 0) {
    # Préparer la matrice des variables exogènes
    exog_matrix <- prediction_markets %>%
      filter(id %in% exog_vars) %>%
      select(date, id, pred_daily) %>%
      # Remplir les valeurs manquantes avec la dernière valeur connue
      group_by(id) %>%
      arrange(date) %>%
      fill(pred_daily, .direction = "down") %>%
      ungroup() %>%
      pivot_wider(names_from = id, values_from = pred_daily) %>%
      arrange(date)
    
    # Afficher les dimensions de la matrice exogène
    cat(sprintf("Dimensions de la matrice exogène pour %s: %d x %d\n", 
                current_ticker, nrow(exog_matrix), ncol(exog_matrix)))
    
    # Aligner les dates avec les données principales
    exog_matrix <- exog_matrix %>%
      filter(date %in% ticker_data$date) %>%
      arrange(date)
    
    # Afficher les dimensions après alignement
    cat(sprintf("Dimensions après alignement pour %s: %d x %d\n", 
                current_ticker, nrow(exog_matrix), ncol(exog_matrix)))
    
    # Vérifier si la matrice exogène est valide
    if (nrow(exog_matrix) == 0 || ncol(exog_matrix) <= 1) {
      cat(sprintf("Pas de données exogènes valides pour %s, utilisation d'ARIMA simple\n", current_ticker))
      model <- auto.arima(ts_data)
    } else {
      # Vérifier l'alignement des dates
      if (!all(exog_matrix$date == ticker_data$date)) {
        warning("Les dates ne sont pas parfaitement alignées, ajustement nécessaire")
        # Réindexer les données pour assurer l'alignement
        exog_matrix <- exog_matrix %>%
          right_join(ticker_data %>% select(date), by = "date") %>%
          arrange(date) %>%
          # Remplir les valeurs manquantes après l'alignement
          fill(-date, .direction = "down")
      }
      
      # Supprimer la colonne date et convertir en matrice
      exog_matrix <- exog_matrix %>%
        select(-date) %>%
        as.matrix()
      
      # Vérifier les dimensions
      if (nrow(exog_matrix) != length(ts_data)) {
        warning(sprintf("Dimensions non correspondantes: ts_data=%d, exog_matrix=%d", 
                       length(ts_data), nrow(exog_matrix)))
        # Ajuster la longueur de ts_data pour correspondre à exog_matrix
        ts_data <- ts(ticker_data$returns[1:nrow(exog_matrix)], frequency = 365)
      }
      
      # Vérifier si la matrice exogène contient des NA
      if (any(is.na(exog_matrix))) {
        warning("NA détectés dans les variables exogènes, utilisation d'ARIMA simple")
        model <- auto.arima(ts_data)
      } else {
        # Vérifier si la matrice exogène est vide ou ne contient que des colonnes constantes
        if (ncol(exog_matrix) == 0 || all(apply(exog_matrix, 2, function(x) length(unique(x)) == 1))) {
          warning("Variables exogènes non informatives, utilisation d'ARIMA simple")
          model <- auto.arima(ts_data)
        } else {
          # Ajuster le modèle ARIMAX
          tryCatch({
            model <- auto.arima(ts_data, xreg = exog_matrix)
          }, error = function(e) {
            warning(paste("Erreur lors de l'ajustement ARIMAX:", e$message, "\nUtilisation d'ARIMA simple"))
            model <- auto.arima(ts_data)
          })
        }
      }
    }
  } else {
    # Sinon, utiliser ARIMA simple
    cat(sprintf("Aucune variable exogène significative pour %s, utilisation d'ARIMA simple\n", current_ticker))
    model <- auto.arima(ts_data)
  }
  
  return(model)
}

# Fonction pour évaluer les performances du modèle
evaluate_model <- function(model, test_data, ticker) {
  # Vérifier si les données de test sont valides
  if (nrow(test_data) == 0) {
    stop("Aucune donnée de test disponible")
  }
  
  # Générer les prévisions
  if (inherits(model, "ARIMA")) {
    # Pour ARIMA simple
    forecast_values <- forecast(model, h = nrow(test_data))
  } else {
    # Pour ARIMAX
    exog_test <- test_data %>%
      select(matches("pred_daily")) %>%
      as.matrix()
    
    # Vérifier les dimensions
    if (nrow(exog_test) != nrow(test_data)) {
      warning("Dimensions non correspondantes dans les données de test")
      # Ajuster la longueur des prévisions
      h <- min(nrow(exog_test), nrow(test_data))
      forecast_values <- forecast(model, xreg = exog_test[1:h,], h = h)
    } else {
      forecast_values <- forecast(model, xreg = exog_test, h = nrow(test_data))
    }
  }
  
  # Calculer le RMSE
  rmse <- sqrt(mean((test_data$returns - forecast_values$mean)^2))
  
  return(list(
    ticker = ticker,
    rmse = rmse,
    forecast = forecast_values$mean
  ))
}

# Préparation des données pour la modélisation
# Créer les splits train/test pour chaque ticker
splits_list <- df_combined %>%
  group_by(ticker) %>%
  group_map(~ {
    # S'assurer que les données sont triées par date
    .x <- .x %>% arrange(date)
    time_series_split(
      .x,
      assess = "3 months",
      cumulative = TRUE
    )
  })

# Initialiser les listes pour stocker les résultats
local_models <- list()
local_forecasts <- list()
local_rmse <- list()
train_forecasts <- list()  # Nouvelle liste pour stocker les prévisions sur le train set

# Boucle sur chaque ticker pour ajuster les modèles locaux
for (i in seq_along(unique(df_combined$ticker))) {
  ticker <- unique(df_combined$ticker)[i]
  cat(sprintf("\nProcessing %s...\n", ticker))
  
  # Extraire les données pour ce ticker
  ticker_data <- df_combined %>% 
    filter(ticker == !!ticker) %>%
    arrange(date)  # S'assurer que les données sont triées par date
  
  # Vérifier si nous avons assez de données
  if (nrow(ticker_data) < 50) {
    cat(sprintf("Pas assez de données pour %s, skipping...\n", ticker))
    next
  }
  
  tryCatch({
    # Ajuster le modèle local
    local_model <- fit_local_model(
      ticker_data = ticker_data,
      granger_results = df_results_sorted,
      prediction_markets = df_pred_all
    )
    
    # Évaluer le modèle
    evaluation <- evaluate_model(
      model = local_model,
      test_data = testing(splits_list[[i]]),
      ticker = ticker
    )
    
    # Générer les prévisions sur le train set
    train_data <- training(splits_list[[i]])
    if (inherits(local_model, "ARIMA")) {
      train_forecast <- fitted(local_model)
    } else {
      exog_train <- train_data %>%
        select(matches("pred_daily")) %>%
        as.matrix()
      train_forecast <- fitted(local_model)
    }
    
    # Stocker les résultats
    local_models[[ticker]] <- local_model
    local_forecasts[[ticker]] <- evaluation$forecast
    local_rmse[[ticker]] <- evaluation$rmse
    train_forecasts[[ticker]] <- train_forecast
    
    cat(sprintf("Modèle ajusté avec succès pour %s\n", ticker))
  }, error = function(e) {
    cat(sprintf("Erreur lors du traitement de %s: %s\n", ticker, e$message))
  })
}

# Créer un tableau récapitulatif des performances
performance_summary <- data.frame(
  ticker = names(local_rmse),
  local_rmse = unlist(local_rmse)
)

# Afficher le résumé des performances
print("Performance Summary:")
print(performance_summary)

# Sauvegarder les résultats
saveRDS(local_models, "local_models.rds")
saveRDS(local_forecasts, "local_forecasts.rds")
saveRDS(performance_summary, "local_performance_summary.rds")

# Visualisation des résultats
# 1. Graphique des données réelles et des prévisions sur le train set
plot_train_results <- function(ticker) {
  train_data <- training(splits_list[[which(unique(df_combined$ticker) == ticker)]])
  test_data <- testing(splits_list[[which(unique(df_combined$ticker) == ticker)]])
  
  # Créer un data frame pour les prévisions du train set
  train_forecast_df <- data.frame(
    date = train_data$date[1:length(train_forecasts[[ticker]])],
    forecast = train_forecasts[[ticker]]
  )
  
  # Calculer les limites de l'axe y pour éviter les valeurs manquantes
  y_min <- min(c(train_data$returns, test_data$returns, train_forecast_df$forecast), na.rm = TRUE)
  y_max <- max(c(train_data$returns, test_data$returns, train_forecast_df$forecast), na.rm = TRUE)
  
  p <- ggplot() +
    geom_line(data = train_data, aes(x = date, y = returns, color = "Données réelles (train)"), linewidth = 0.8) +
    geom_line(data = train_forecast_df, 
              aes(x = date, y = forecast, color = "Prévisions (train)"), linewidth = 0.8) +
    geom_line(data = test_data, aes(x = date, y = returns, color = "Données réelles (test)"), linewidth = 0.8) +
    labs(title = paste("Résultats du modèle pour", ticker),
         x = "Date",
         y = "Rendements",
         color = "Légende") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    ylim(y_min, y_max)  # Définir les limites de l'axe y
  
  return(p)
}

# 2. Graphique des prévisions futures
plot_future_forecasts <- function(ticker) {
  train_data <- training(splits_list[[which(unique(df_combined$ticker) == ticker)]])
  test_data <- testing(splits_list[[which(unique(df_combined$ticker) == ticker)]])
  
  # Générer les prévisions futures
  future_dates <- seq(max(test_data$date), by = "day", length.out = 60)
  if (inherits(local_models[[ticker]], "ARIMA")) {
    future_forecast <- forecast(local_models[[ticker]], h = 60)
  } else {
    # Pour ARIMAX, nous devons préparer les variables exogènes futures
    last_exog <- test_data %>%
      select(matches("pred_daily")) %>%
      tail(1) %>%
      as.matrix()
    future_forecast <- forecast(local_models[[ticker]], 
                              xreg = matrix(rep(last_exog, 60), ncol = ncol(last_exog), byrow = TRUE), 
                              h = 60)
  }
  
  # Calculer les limites de l'axe y
  y_min <- min(c(train_data$returns, test_data$returns, future_forecast$mean), na.rm = TRUE)
  y_max <- max(c(train_data$returns, test_data$returns, future_forecast$mean), na.rm = TRUE)
  
  p <- ggplot() +
    geom_line(data = train_data, aes(x = date, y = returns, color = "Données réelles (train)"), linewidth = 0.8) +
    geom_line(data = test_data, aes(x = date, y = returns, color = "Données réelles (test)"), linewidth = 0.8) +
    geom_line(data = data.frame(date = future_dates, forecast = future_forecast$mean), 
              aes(x = date, y = forecast, color = "Prévisions futures"), linewidth = 0.8) +
    geom_ribbon(data = data.frame(date = future_dates, 
                                 lower = future_forecast$lower[,2], 
                                 upper = future_forecast$upper[,2]),
                aes(x = date, ymin = lower, ymax = upper), alpha = 0.2) +
    labs(title = paste("Prévisions futures pour", ticker),
         x = "Date",
         y = "Rendements",
         color = "Légende") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    ylim(y_min, y_max)  # Définir les limites de l'axe y
  
  return(p)
}

# Créer les graphiques pour chaque ticker
library(gridExtra)
plots_list <- list()
future_plots_list <- list()

for (ticker in names(local_models)) {
  plots_list[[ticker]] <- plot_train_results(ticker)
  future_plots_list[[ticker]] <- plot_future_forecasts(ticker)
}

# Combiner les graphiques en une seule figure
n_cols <- 3
n_rows <- ceiling(length(plots_list) / n_cols)

# Sauvegarder les graphiques
pdf("local_models_results.pdf", width = 15, height = 10)
grid.arrange(grobs = plots_list, ncol = n_cols)
dev.off()

pdf("local_models_future_forecasts.pdf", width = 15, height = 10)
grid.arrange(grobs = future_plots_list, ncol = n_cols)
dev.off()



