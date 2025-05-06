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


# Pas de stationnarité parce que le XGBoost n'en a pas besoin

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

# Effectuer un left_join entre les prédictions et les actions
df_combined <- df_pred_all %>%
  left_join(df_stock, by = c("date"))

# Vérifier les premières lignes du DataFrame combiné
head(df_combined)

# Supprimer les lignes avec NA returns
df_combined <- df_combined %>%
  filter(!is.na(returns))


# Créer un tableau simplifié avec 'id', 'date' et 'value'
data_tbl <- df_combined %>%
  select(date, ticker, value = returns, id, pred_daily)  # 'returns' comme valeur à prédire

# Vérifier les premières lignes du tableau
head(data_tbl)


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
  future_frame(.length_out = 60, .date_var = date ,.bind_data = FALSE) %>% 
  mutate(id = NA,pred_daily = NA)   

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
