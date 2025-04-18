import yfinance as yf
import pandas as pd
import os

# Créer le dossier "ETF" s'il n'existe pas
output_dir = "ETF"
os.makedirs(output_dir, exist_ok=True)

# Liste des ETF
etfs = {
    "CAC 40": "CAC.PA",
    "S&P 500": "SPY",
    "MSCI World": "IWDA.AS",
    "Nasdaq 100": "QQQ"
}

# Dates
start_date = "2024-01-01"
end_date = "2024-04-18"

for name, ticker in etfs.items():
    # Récupération des données
    data = yf.download(ticker, start=start_date, end=end_date, interval='1d')
    
    # Si MultiIndex de colonnes, on aplatit
    if isinstance(data.columns, pd.MultiIndex):
        data.columns = [col[0] for col in data.columns]
    
    # Calcul des rendements journaliers
    data["Daily Return"] = data["Close"].pct_change()
    
    # Ajouter la colonne 'Ticker'
    data["Ticker"] = ticker
    
    # Réorganiser les colonnes (optionnel)
    cols = ["Ticker"] + [col for col in data.columns if col != "Ticker"]
    data = data[cols]
    
    # Sauvegarde
    filename = f"{name.replace(' ', '_')}_returns_2024.csv"
    filepath = os.path.join(output_dir, filename)
    data.to_csv(filepath)
    
    print(f"{name} : sauvegardé dans {filepath} ✅")
