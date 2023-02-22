import fundamentalanalysis as fa

ticker1 = "TSLA"
ticker2 = "AMZN"
ticker3 = "AAPL"

api_key = "8c51e557f77f4c7a9e542713a334e145"

#dcf_annually = fa.discounted_cash_flow(ticker, api_key, period="annual")
pd1 = fa.balance_sheet_statement(ticker1, api_key, period="annual")
pd2 = fa.balance_sheet_statement(ticker2, api_key, period="annual")
pd3 = fa.balance_sheet_statement(ticker3, api_key, period="annual")

#print(dcf_annually)
pd1.to_csv('tsla_balance_sheet.csv')
pd1.to_csv('amzn_balance_Sheet.csv')
pd1.to_csv('aapl_balance_sheet.csv')