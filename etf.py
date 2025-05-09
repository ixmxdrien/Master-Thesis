import yfinance as yf
import pandas as pd
import os

# Créer le dossier "ETF" s'il n'existe pas
output_dir = "ETF"
os.makedirs(output_dir, exist_ok=True)

# Liste des ETF
etfs = {
    "CAC 40": "^FCHI",
    "S&P 500": "^GSPC",
    "MSCI World": "URTH",
    "Nasdaq 100": "^IXIC"
}

# Dates
start_date = "2024-01-01"
end_date = "2024-12-12"

# Liste pour stocker les dataframes
all_returns = []

for name, ticker in etfs.items():
    # Récupération des données
    data = yf.download(ticker, start=start_date, end=end_date, interval='1d')
    
    # Si MultiIndex de colonnes, on aplatit
    if isinstance(data.columns, pd.MultiIndex):
        data.columns = [col[0] for col in data.columns]
    
    # Calcul des rendements journaliers
    returns = pd.DataFrame({
        'date': data.index,
        'ticker': ticker,
        'returns': data["Close"].pct_change()
    })
    
    all_returns.append(returns)

# Combiner tous les dataframes
combined_returns = pd.concat(all_returns, ignore_index=True)

# Sauvegarder le fichier combiné
combined_filepath = os.path.join(output_dir, "combined_returns_2024.csv")
combined_returns.to_csv(combined_filepath, index=False)

print(f"Fichier combiné sauvegardé dans {combined_filepath} ✅")
