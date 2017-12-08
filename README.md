# Stock-Correlations
The objective of Stock-Correlation model is to predict stock price for future (usually 12-18 hrs) using the current stock prices from NASDAQ
The Inputs to come up with Stock-correlation model are:
  1. predict stock matrix (whole number, greater than 1 & less than number of rows in stock_data_frame)
  2. response stock matrix (the period length between which rows should be subtracted in the response stocks)
  3. rolling correlation (if TRUE a rolling correlation will be conducted. If FALSE a standard correlation will be conducted)
  4. stock data
  5. lag period (this is the difference between predictor_diff ends and the corresponding response_diff begins)
  6. predictor difference (the period length between which rows should be subtracted in the predictor stocks)
The outputs of stock-correlation model are:
  1. correlation matrix (this a correlation matrix which represents the rolling correlation between the predictive stocks)
  2. Stock difference list (a list containing two matrices.  One is the so-called predictor matrix and one is the so-called response 
  matrix)
