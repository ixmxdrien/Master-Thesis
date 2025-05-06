############################################################################################
########################################## Projet ##########################################
############################################################################################


# Installer et charger les packages nécessaires
libraries <- c("tidymodels", "modeltime", "timetk", "tidyverse", "lubridate", "tseries",
               "ggplot2", "caret", "dplyr", "plm", "tidyr", "zoo", "forecast", "KFAS", "reshape2",
               "lmtest", "purrr", "urca", "Metrics")


# Verify if it's already installed
for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
    library(lib, character.only = TRUE)
  }
}

#############################################################################################
##################################### Data Processing #######################################
#############################################################################################

# CHARGER LES DATASETS

# TESLA
df_q1_tesla <- read_csv("analyzing the stock market/tesla_prod/kalshi-chart-data-tesla-24-q1.csv") %>% mutate(quarter = "Q1")
df_q2_tesla <- read_csv("analyzing the stock market/tesla_prod/kalshi-chart-data-tesla-24-q2.csv") %>% mutate(quarter = "Q2")
df_q3_tesla <- read_csv("analyzing the stock market/tesla_prod/kalshi-chart-data-tesla-24-q3.csv") %>% mutate(quarter = "Q3")
df_q4_tesla <- read_csv("analyzing the stock market/tesla_prod/kalshi-chart-data-tesla-24-q4.csv") %>% mutate(quarter = "Q4")


# NETFLIX
df_q1_netflix <- read_csv("analyzing the stock market/netflix_sub/kalshi-chart-data-netflixsubs-24-q1.csv") %>% mutate(quarter = "Q1")
df_q2_netflix <- read_csv("analyzing the stock market/netflix_sub/kalshi-chart-data-netflixsubs-24-q2.csv") %>% mutate(quarter = "Q2")
df_q3_netflix <- read_csv("analyzing the stock market/netflix_sub/kalshi-chart-data-netflixsubs-24-q3.csv") %>% mutate(quarter = "Q3")
df_q4_netflix <- read_csv("analyzing the stock market/netflix_sub/kalshi-chart-data-netflixsubs-24-q4.csv") %>% mutate(quarter = "Q4")


# META
df_q1_meta <- read_csv("analyzing the stock market/meta_users/kalshi-chart-data-metadap-24-q1.csv") %>% mutate(quarter = "Q1")
df_q2_meta <- read_csv("analyzing the stock market/meta_users/kalshi-chart-data-metadap-24-q2.csv") %>% mutate(quarter = "Q2")
df_q3_meta <- read_csv("analyzing the stock market/meta_users/kalshi-chart-data-metadap-24-q3.csv") %>% mutate(quarter = "Q3")

# GDP
df_q1_gdp <- read_csv("analyzing the stock market/GDP_US/kalshi-chart-data-gdp-24apr25.csv") %>% mutate(quarter = "Q1")
df_q2_gdp <- read_csv("analyzing the stock market/GDP_US/kalshi-chart-data-gdp-24jul25.csv") %>% mutate(quarter = "Q2")
df_q3_gdp <- read_csv("analyzing the stock market/GDP_US/kalshi-chart-data-gdp-24oct30.csv") %>% mutate(quarter = "Q3")
df_q4_gdp <- read_csv("analyzing the stock market/GDP_US/kalshi-chart-data-kxgdp-25jan31.csv") %>% mutate(quarter = "Q4")

# Fusionner les datasets
df_Tesla <- bind_rows(df_q1_tesla, df_q2_tesla, df_q3_tesla, df_q4_tesla) %>%
  mutate(ticker = "Tesla")
df_Netflix <- bind_rows(df_q1_netflix, df_q2_netflix, df_q3_netflix, df_q4_netflix) %>%
  mutate(ticker = "Netflix")
df_Meta <- bind_rows(df_q1_meta, df_q2_meta, df_q3_meta) %>%
  mutate(ticker = "Meta")
df_GDP <- bind_rows(df_q1_gdp, df_q2_gdp, df_q3_gdp, df_q4_gdp ) %>%
  mutate(ticker = "GDP")

# Autres datasets
df_SpaceX <- read_csv("analyzing the stock market/spaceX/kalshi-chart-data-spacexcount-24.csv") %>%
  mutate(ticker = "SpaceX")
df_gas_us <- read_csv("analyzing the stock market/price_gas_usa/kalshi-chart-data-aaagasmaxtx-24dec31.csv") %>%
  mutate(ticker = "Gas US")
df_wti_oil <- read_csv("analyzing the stock market/wti_oil/kalshi-chart-data-wtimin-24dec31.csv") %>%
  mutate(ticker = "WTI Oil")
df_google_sp <- read_csv("analyzing the stock market/Sundar_Pichai_google/kalshi-chart-data-googleceochange.csv") %>%
  mutate(ticker = "Google")
df_fed_rate <- read_csv("analyzing the stock market/fed_rate_us/kalshi-chart-data-fedratemin-24dec31.csv") %>%
  mutate(ticker = "Fed Rate")
df_btc <- read_csv("analyzing the stock market/btc/kalshi-chart-data-btcmaxy-24dec31.csv") %>%
  mutate(ticker = "BTC")
df_us_sc <- read_csv("analyzing the stock market/us_semi_conductor/kalshi-chart-data-semiprodh-24.csv") %>%
  mutate(ticker = "US Semi Conductor")
df_infla <- read_csv("analyzing the stock market/inflation/kalshi-chart-data-acpicore-2024.csv") %>%
  mutate(ticker = "Inflation")
df_layoffs <- read_csv("analyzing the stock market/big_tech_layoffs/kalshi-chart-data-bigtechlayoff-24dec31.csv") %>%
  mutate(ticker = "Layoffs")
df_huricane <- read_csv("analyzing the stock market/number_of_huricane/kalshi-chart-data-hurctot-24dec01.csv") %>%
  mutate(ticker = "Hurricanes")
df_eth <- read_csv("analyzing the stock market/eth/kalshi-chart-data-ethmaxy-24dec31.csv") %>%
  mutate(ticker = "ETH")
df_measles <- read_csv("analyzing the stock market/Measles_cases/kalshi-chart-data-measles-24.csv") %>%
  mutate(ticker = "Measles")
df_apple <- read_csv("analyzing the stock market/Apple/kalshi-chart-data-applecar-24dec31.csv") %>%
  mutate(ticker = "Apple")

# Visualisation des head des datasets
head(df_Tesla)
head(df_Netflix)
head(df_Meta)
head(df_GDP)
head(df_SpaceX)
head(df_gas_us)
head(df_wti_oil)
head(df_google_sp)
head(df_fed_rate)
head(df_btc)
head(df_us_sc)
head(df_infla)
head(df_layoffs)
head(df_huricane)
head(df_eth)
head(df_measles)
head(df_apple)


# Conversion de la colonne Timestamp en type Date pour chaque dataset
# Fonction pour transformer un df avec colonne 'Timestamp' et 'Value'
process_df_daily <- function(df, ticker) {
  df %>%
    mutate(date = as.Date(Timestamp)) %>% 
    mutate(id = ticker) %>%      # Conversion en date
    group_by(date, id) %>%
    summarise(pred_daily = mean(Value, na.rm = TRUE)) # Moyenne journalière
}

df_pred_daily_TESLA    <- process_df_daily(df_Tesla, "TESLA")
df_pred_daily_NETFLIX  <- process_df_daily(df_Netflix, "NETFLIX")
df_pred_daily_META     <- process_df_daily(df_Meta, "META")
df_pred_daily_GDP      <- process_df_daily(df_GDP, "GDP")
df_pred_daily_SpaceX   <- process_df_daily(df_SpaceX, "SpaceX")
df_pred_daily_gas_us   <- process_df_daily(df_gas_us, "gas_us")
df_pred_daily_wti_oil  <- process_df_daily(df_wti_oil, "wti_oil")
df_pred_daily_btc      <- process_df_daily(df_btc, "btc")
df_pred_daily_us_sc    <- process_df_daily(df_us_sc, "us_sc")
df_pred_daily_infla    <- process_df_daily(df_infla, "infla")
df_pred_daily_huricane <- process_df_daily(df_huricane, "huricane")
df_pred_daily_eth      <- process_df_daily(df_eth, "eth")
df_pred_daily_measles  <- process_df_daily(df_measles, "measles")


process_df_daily_forecast <- function(df, ticker) {
  df %>%
    mutate(
      date = as.Date(Timestamp),             # Conversion en date
      Forecast = as.numeric(gsub("%", "", Forecast)),  # Enlever le % et convertir en numeric
      id = ticker                        # Ajouter une colonne 'ticker'
    ) %>%
    group_by(date, id) %>%               # Groupement par date et ticker
    summarise(pred_daily = mean(Forecast, na.rm = TRUE)) %>%  # Moyenne journalière
    ungroup()                                # Annuler la mise en groupe
}


df_pred_daily_google   <- process_df_daily_forecast(df_google_sp, "Google")
df_pred_daily_fed      <- process_df_daily_forecast(df_fed_rate, "Fed Rate")
df_pred_daily_layoffs  <- process_df_daily_forecast(df_layoffs, "Layoffs")
df_pred_daily_apple    <- process_df_daily_forecast(df_apple, "Apple")


head(df_pred_daily_google)


# Créer une séquence complète de dates pour chaque dataset
complete_dates_TESLA <- tibble(date = seq(min(df_pred_daily_TESLA$date, na.rm = TRUE),
                                          max(df_pred_daily_TESLA$date, na.rm = TRUE), by = "day"))

complete_dates_NETFLIX <- tibble(date = seq(min(df_pred_daily_NETFLIX$date, na.rm = TRUE),
                                            max(df_pred_daily_NETFLIX$date, na.rm = TRUE), by = "day"))

complete_dates_META <- tibble(date = seq(min(df_pred_daily_META$date, na.rm = TRUE),
                                         max(df_pred_daily_META$date, na.rm = TRUE), by = "day"))

complete_dates_GDP <- tibble(date = seq(min(df_pred_daily_GDP$date, na.rm = TRUE),
                                         max(df_pred_daily_GDP$date, na.rm = TRUE), by = "day"))


# Joindre avec les datasets d'origine
df_pred_daily_TESLA <- complete_dates_TESLA %>%
  left_join(df_pred_daily_TESLA, by = "date")

df_pred_daily_NETFLIX <- complete_dates_NETFLIX %>%
  left_join(df_pred_daily_NETFLIX, by = "date")

df_pred_daily_META <- complete_dates_META %>%
  left_join(df_pred_daily_META, by = "date")

df_pred_daily_GDP <- complete_dates_GDP %>%
  left_join(df_pred_daily_GDP, by = "date")
  
  
  

# Fonction pour appliquer le filtre de Kalman et remplir les valeurs manquantes
apply_kalman_filter <- function(df) {
  # Convertir les données en série temporelle avec la bonne fréquence (365 jours)
  ts_data <- ts(df$pred_daily, frequency = 365, start = c(2024, 1))  # ajuster le début de la série selon les données
  
  # Créer un modèle d'état pour le filtre de Kalman avec tendance linéaire
  model <- SSModel(ts_data ~ -1 + SSMtrend(degree = 1, Q = 1))  # Modèle avec tendance (SSMtrend)
  
  # Appliquer le filtre de Kalman pour estimer les valeurs manquantes
  kalman_fit <- KFS(model, simplify = TRUE)
  
  # Extraire les valeurs filtrées (prévisions de Kalman)
  kalman_values <- kalman_fit$a
  
  # Vérifier si la longueur des valeurs filtrées est égale à celle du dataframe
  if (length(kalman_values) != nrow(df)) {
    # Ajuster la longueur en coupant la dernière ligne si nécessaire
    kalman_values <- kalman_values[1:nrow(df)]
  }
  
  # Mettre à jour la colonne 'pred_daily' avec les valeurs filtrées
  df$pred_daily <- ifelse(is.na(df$pred_daily), kalman_values, df$pred_daily)
  
  df$id[is.na(df$id)] <- unique(df$id[!is.na(df$id)])[1]
  
  return(df)
}

# Appliquer cette fonction à la série de données META
df_pred_daily_TESLA <- apply_kalman_filter(df_pred_daily_TESLA)
df_pred_daily_NETFLIX <- apply_kalman_filter(df_pred_daily_NETFLIX)
df_pred_daily_META <- apply_kalman_filter(df_pred_daily_META)
df_pred_daily_GDP <- apply_kalman_filter(df_pred_daily_GDP)


check_stationarity <- function(df, ticker) {
  # Vérifier s'il y a des NA et les supprimer avant le test ADF
  df_clean <- df[!is.na(df$pred_daily), ]
  
  # Tester la stationnarité avec le test de Dickey-Fuller (ADF)
  adf_result <- adf.test(df_clean$pred_daily, alternative = "stationary")
  
  # Afficher les résultats du test de stationnarité
  cat(paste("ADF test for", ticker, "p-value:", adf_result$p.value, "\n"))
  
  # Si la p-value est supérieure à 0.05, la série n'est pas stationnaire
  if (adf_result$p.value > 0.05) {
    cat(paste(ticker, "is not stationary. Applying first differencing...\n"))
    df$pred_daily <- c(NA, diff(df$pred_daily, differences = 1))  # Ajoute NA au début pour garder la même longueur
    
    # Vérifier s'il y a des NA et les supprimer avant le test ADF
    df_clean <- df[!is.na(df$pred_daily), ]
    
    # Refaire le test après la première différenciation
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


# Charger les données ETF et supprimer les lignes avec des NA
df_etf <- read_csv("ETF/combined_returns_2024.csv") %>%
  drop_na()  # Supprime toutes les lignes contenant au moins un NA


# Charger le fichier complet des actions
df_stock <- read_csv("analyzing the stock market/tilt_stocks_2024.csv") %>%
  select(-`...1`) %>%
  filter(ticker %in% c("TSLA", "NFLX", "META", "GOOG", "COIN", "INTC", "JPM", "XOM", "AAPL", "BAC", "TOT", "PFE", "JNJ", "MSFT", "AMZN", "WMT", "NVDA")) %>%
  mutate(date = as.Date(date))  # Convertir en format date


# Vérifier les premières lignes du DataFrame filtré
head(df_stock)

# Fusionner les deux DataFrame
df_stock <- bind_rows(df_stock, df_etf)

# Afficher le DataFrame final
print(df_stock)



check_stationarity <- function(df, ticker) {
  # Vérifier s'il y a des NA et les supprimer avant le test ADF
  df_clean <- df[!is.na(df$returns), ]
  
  # Tester la stationnarité avec le test de Dickey-Fuller (ADF)
  adf_result <- adf.test(df_clean$returns, alternative = "stationary")
  
  # Afficher les résultats du test de stationnarité
  cat(paste("ADF test for", ticker, "p-value:", adf_result$p.value, "\n"))
  
  # Si la p-value est supérieure à 0.05, la série n'est pas stationnaire
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




# granger causality 
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



correlation_tbl <- df_combined %>%
  group_by(ticker, id) %>%  # ticker = action, id = prédiction Kalshi
  summarise(
    correlation = cor(pred_daily, returns, use = "complete.obs"),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(correlation)))  # Trier par force de la corrélation

# Affichage
print(correlation_tbl)



ggplot(correlation_tbl, aes(x = reorder(paste(ticker, id, sep = "-"), correlation), y = correlation, fill = correlation)) +
  geom_col() +
  coord_flip() +
  labs(title = "Corrélation entre actions et marchés de prédiction",
       x = "Paire (Action - Marché Kalshi)",
       y = "Corrélation") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  theme_minimal()


correlation_tbl <- correlation_tbl %>%
  mutate(correlation = as.numeric(correlation)) %>%
  filter(abs(correlation) > 0.05)



corr_matrix <- df_combined %>%
  filter(!is.na(pred_daily), !is.na(returns)) %>%
  group_by(ticker, id) %>%
  summarise(correlation = cor(pred_daily, returns, use = "complete.obs"), .groups = "drop") %>%
  pivot_wider(names_from = ticker, values_from = correlation)

corr_matrix_long <- melt(corr_matrix, id.vars = "id")

ggplot(corr_matrix_long, aes(x = variable, y = id, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  theme_minimal() +
  labs(title = "Heatmap des corrélations", x = "Action", y = "Prédiction Kalshi", fill = "Corrélation")



# Créer un tableau simplifié avec 'id', 'date' et 'value'
data_tbl <- df_combined %>%
  select(date, ticker, value = returns, id, pred_daily)  # 'returns' comme valeur à prédire

# Vérifier les premières lignes du tableau
head(data_tbl)


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


splits

# Créer la recette de prétraitement pour le modèle XGBoost
rec_obj_xgb <- recipe(value ~ ., training(splits)) %>%
  step_dummy(id) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_timeseries_signature(date) %>%
  step_rm(date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

summary(prep(rec_obj_xgb))


#############################################################################################
################################# XGBoost Panel Data Modeling ##############################
#############################################################################################

# Création du modèle avec XGBoost et la recette de prétraitement
#wflw_xgb <- workflow() %>%
#  add_model(
#    boost_tree(mode = "regression", trees = 1000, tree_depth = 6, learn_rate = 0.01) %>% set_engine("xgboost")
#  ) %>%
#  add_recipe(rec_obj_xgb) %>%
#  fit(train_data)

wflw_xgb <- workflow() %>%
  add_model(
    boost_tree("regression") %>% set_engine("xgboost")
  ) %>%
  add_recipe(rec_obj_xgb) %>%
  fit(training(splits))

wflw_xgb



model_tbl <- modeltime_table(
  wflw_xgb
)

model_tbl



calib_tbl <- model_tbl %>%
  modeltime_calibrate(
    new_data = testing(splits), 
    id       = "ticker"
  )

calib_tbl



calib_tbl %>% 
  modeltime_accuracy(acc_by_id = FALSE) %>% 
  table_modeltime_accuracy(.interactive = FALSE)


calib_tbl %>% 
  modeltime_accuracy(acc_by_id = TRUE) %>% 
  table_modeltime_accuracy(.interactive = FALSE)


calib_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_tbl,
    conf_by_id  = TRUE
  ) %>%
  group_by(ticker) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 3,
    .interactive = FALSE
  )


refit_tbl <- calib_tbl %>%
  modeltime_refit(data = data_tbl)

refit_tbl


future_tbl <- data_tbl %>%
  group_by(id) %>%
  future_frame(.length_out = 52, .date_var = date ,.bind_data = FALSE)

future_tbl


# Créer la recette de prétraitement pour le modèle XGBoost
rec_obj_xgb <- recipe(value ~ ., training(splits)) %>%
  step_dummy(id) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_timeseries_signature(date) %>%
  step_rm(date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

summary(prep(rec_obj_xgb))




#############################################################################################
################################# XGBoost Panel Data Modeling ##############################
#############################################################################################

# Création du modèle avec XGBoost et la recette de prétraitement
#wflw_xgb <- workflow() %>%
#  add_model(
#    boost_tree(mode = "regression", trees = 1000, tree_depth = 6, learn_rate = 0.01) %>% set_engine("xgboost")
#  ) %>%
#  add_recipe(rec_obj_xgb) %>%
#  fit(train_data)

wflw_xgb <- workflow() %>%
  add_model(
    boost_tree("regression") %>% set_engine("xgboost")
  ) %>%
  add_recipe(rec_obj_xgb) %>%
  fit(training(splits))

wflw_xgb



model_tbl <- modeltime_table(
  wflw_xgb
)

model_tbl



calib_tbl <- model_tbl %>%
  modeltime_calibrate(
    new_data = testing(splits), 
    id       = "ticker"
  )

calib_tbl



calib_tbl %>% 
  modeltime_accuracy(acc_by_id = FALSE) %>% 
  table_modeltime_accuracy(.interactive = FALSE)


calib_tbl %>% 
  modeltime_accuracy(acc_by_id = TRUE) %>% 
  table_modeltime_accuracy(.interactive = FALSE)


calib_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_tbl,
    conf_by_id  = TRUE
  ) %>%
  group_by(ticker) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 3,
    .interactive = FALSE
  )


refit_tbl <- calib_tbl %>%
  modeltime_refit(data = data_tbl)

refit_tbl

data_tbl <- data_tbl %>%
  distinct(ticker, date, .keep_all = TRUE)

future_tbl <- data_tbl %>%
  group_by(ticker) %>%
  future_frame(.length_out = 52, .date_var = date, .bind_data = FALSE) %>%
  mutate(id = NA,          # Assigner 'ticker' à 'id'
       pred_daily = NA)   

future_tbl



refit_tbl %>%
  modeltime_forecast(
    new_data    = future_tbl,
    actual_data = data_tbl, 
    conf_by_id  = TRUE
  ) %>%
  group_by(ticker) %>%
  plot_modeltime_forecast(
    .interactive = F,
    .facet_ncol  = 2
  )


#############################################################################################
################################# Comparaison des modèles ##################################
#############################################################################################








