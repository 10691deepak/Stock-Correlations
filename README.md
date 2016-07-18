# Stock-Correlations

The idea is to take time series data of all stocks trading on the us markets (open, close, volume, etc) plus time series data of any other metrics which could possibly be useful (daily commodity prices, oil prices, foreign stock indexes, market patterns, current affairs, any type of news, and any other data) to create a linear model and predict future stock prices a few days ahead.

Once this information is collected, the dates all need to be lined up for all stocks / various metrics.

Then a minimum number of time periods can be selected (for example, exclude any stocks that haven't been trading for at least 2 years).

Then each stock / index will be shifted by a given period (for example 3 days) and compared to all other stocks / indexes which have not been shifted, and a lag correlation will be found.  From these correlations, a matrix will be made:

      A-shifted 1   B-shifted-1  C-shifted-1  .....    n-shifted 1
A        cor                  cor                   cor
B        cor                  cor                   cor
C        cor                  cor                   cor
n

So far, all of this seems to be working, aside from collecting the stock price and other index information.

Next Steps :


Create a function to extract the strongest correlations from the matrix of correlations (theres too many to look at by hand...around 7000 stocks on the NASDAQ and US stock exchange, so this leaves a 7000x70000 matrix = 49 million correlations)

I also want to automate the functions to run multiple times with different lag periods (for example test all periods from 1 to 20 days).

Once the strongest correlations have been found, i want to create a linear model to try to predict future stock prices a few days ahead.

To summarize, if stock A moves by a given amount on day 1 and this consistently leads to stock B moving by a given amount several days later, then we know that an increase or decrease in stock A will lead to an increase or decrease in stock B before it happens.
