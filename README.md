# Financial Market Prediction Model

## Overview
This project implements a sophisticated financial market prediction model that combines various market indicators, predictions, and time series analysis to forecast stock returns. The model utilizes a combination of Kalman filtering, XGBoost, and time series analysis techniques to provide accurate predictions across multiple financial instruments.

## Features
- Multi-asset prediction system covering stocks, ETFs, and market indicators
- Integration of various market predictions (Tesla production, Netflix subscriptions, Meta users, GDP, etc.)
- Kalman filter implementation for handling missing data
- Advanced time series analysis and forecasting
- XGBoost-based machine learning model
- Comprehensive error analysis and model evaluation

## Data Sources
The model incorporates multiple data sources including:
- Stock market data (TSLA, NFLX, META, GOOG, etc.)
- Market predictions from Kalshi
- Economic indicators (GDP, inflation, Fed rates)
- Commodity prices (gas, oil)
- Cryptocurrency data (BTC, ETH)
- Technology sector indicators
- Natural events (hurricanes)

## Project Structure
```
├── data/
│   ├── stocks_and_pred/    # Stock data and predictions
│   └── rds/               # Saved model results
├── ETF/                   # ETF-related data
└── global_model.R        # Main model implementation
```

## Model Components
1. **Data Processing**
   - Time series data integration
   - Missing value handling using Kalman filtering
   - Feature engineering and normalization

2. **Model Architecture**
   - XGBoost-based regression model
   - Time series signature features
   - Cross-validation and model calibration

3. **Evaluation Metrics**
   - RMSE (Root Mean Square Error)
   - Per-ticker accuracy analysis
   - Global model performance metrics

## Requirements
The project requires the following R packages:
- tidymodels
- modeltime
- timetk
- tidyverse
- lubridate
- tseries
- ggplot2
- caret
- dplyr
- plm
- tidyr
- zoo
- forecast
- KFAS
- reshape2
- lmtest
- purrr
- urca
- Metrics

## Usage
1. Ensure all required packages are installed
2. Place data files in appropriate directories
3. Run `global_model.R` to:
   - Process and combine all data sources
   - Train the prediction model
   - Generate forecasts
   - Evaluate model performance

## Output
The model generates:
- Forecast errors analysis
- RMSE measurements (both global and per-ticker)
- Future predictions for multiple time horizons
- Visualizations of model performance

## Future Improvements
- Integration of additional market indicators
- Enhanced feature engineering
- Model ensemble approaches
- Real-time prediction capabilities

## Author
Adrien Payen
