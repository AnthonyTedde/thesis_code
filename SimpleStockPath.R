# The ingredients to the construction of the stock price path are
# * A Brownian Motion
# * An equation base on theory page 148 (eq. 4.4.28)
# * The Itô-Doeblin formula for Brownian Motion
# 
# Creation of the Bronian Motion:
# 
library(RandomWalk)
brownian_motion <- sbmotion(time_to_maturity = 4, scale = 360, seed = 3)

# 
# Creation of the stock price path
# 
library(StockPriceSimulator)
S <- sstock(initial_stock_price = 50, time_to_maturity = 4, scale = 360, seed = 3,
       sigma = .2, alpha = .05)
S_ito <- sstock_ito(initial_stock_price = 50, time_to_maturity = 4, scale = 360, seed = 3,
       sigma = .2, alpha = .05)

# The following plot a stock price price daily for 4 years.
library(ggplot2)
ggplot(S, aes(time_periods, stock_price_path)) + geom_line()
ggplot(S_ito, aes(time_periods, stock_price_path)) + geom_line()