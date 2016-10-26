# R Code for S&P500 Stock Analysis
# Author: Matt Dancho
# Date: 2016-10-23


# Prerequisites ----------------------------------------------------------------
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series 
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots
library(corrplot)   # Visuazlize correlation plots


# Web Scraping: Get the List of S&P500 Stocks ----------------------------------

# Web-scrape S&P500 stock list
sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
    html_node("table.wikitable") %>%
    html_table() %>%
    select(`Ticker symbol`, Security, `GICS Sector`, `GICS Sub Industry`) %>%
    as_tibble()
# Format names
names(sp_500) <- sp_500 %>% 
    names() %>% 
    str_to_lower() %>% 
    make.names()


# Creating Functions to Map ----------------------------------------------------

get_stock_prices <- function(ticker, return_format = "tibble", ...) {
    # Get stock prices
    stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
    # Rename
    names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    # Return in xts format if tibble is not specified
    if (return_format == "tibble") {
        stock_prices <- stock_prices_xts %>%
            as_tibble() %>%
            rownames_to_column(var = "Date") %>%
            mutate(Date = ymd(Date))
    } else {
        stock_prices <- stock_prices_xts
    }
    stock_prices
}

get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
    # Convert tibble to xts
    if (!is.xts(x)) {
        x <- xts(x[,-1], order.by = x$Date)
    }
    # Get stock prices
    log_returns_xts <- periodReturn(x = x, type = 'log', period = period, ...)
    # Rename
    names(log_returns_xts) <- "Log.Returns"
    # Return in xts format if tibble is not specified
    if (return_format == "tibble") {
        log_returns <- log_returns_xts %>%
            as_tibble() %>%
            rownames_to_column(var = "Date") %>%
            mutate(Date = ymd(Date))
    } else {
        log_returns <- log_returns_xts
    }
    log_returns
}


# Mapping the Functions --------------------------------------------------------
from <- "2007-01-01"
to   <- today()
sp_500 <- sp_500 %>%
    mutate(
        stock.prices = map(ticker.symbol, 
                           function(.x) get_stock_prices(.x, 
                                                         return_format = "tibble",
                                                         from = from,
                                                         to   = to)
        ),
        log.returns  = map(stock.prices, 
                           function(.x) get_log_returns(.x, return_format = "tibble")),
        mean.log.returns = map_dbl(log.returns, ~ median(.$Log.Returns)),
        sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
        n.trade.days = map_dbl(stock.prices, nrow)
    ) 


# Visualizing the Results with Plotly ------------------------------------------

plot_ly(data   = sp_500,
        type   = "scatter",
        mode   = "markers",
        x      = ~ sd.log.returns,
        y      = ~ mean.log.returns,
        color  = ~ n.trade.days,
        colors = "Blues",
        size   = ~ n.trade.days,
        text   = ~ str_c("<em>", security, "</em><br>",
                         "Ticker: ", ticker.symbol, "<br>",
                         "Sector: ", gics.sector, "<br>",
                         "Sub Sector: ", gics.sub.industry, "<br>",
                         "No. of Trading Days: ", n.trade.days),
        marker = list(opacity = 0.8,
                      symbol = 'circle',
                      sizemode = 'diameter',
                      sizeref = 4.0,
                      line = list(width = 2, color = '#FFFFFF'))
        ) %>%
    layout(title   = 'S&P500 Analysis: Stock Risk vs Reward',
           xaxis   = list(title = 'Risk: StDev Log Returns',
                          gridcolor = 'rgb(255, 255, 255)',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
           yaxis   = list(title = 'Reward: Mean Log Returns',
                          gridcolor = 'rgb(255, 255, 255)',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 2),
           margin = list(l = 100,
                         t = 100,
                         b = 100),
           font   = list(color = '#FFFFFF'),
           paper_bgcolor = 'rgb(0, 0, 0)',
           plot_bgcolor = 'rgb(0, 0, 0)')


# Bonus: Computing Correlations ------------------------------------------------

# Filter high performing stocks
limit <- 20 # Limit for top n performers
sp_500_hp <- sp_500 %>%
    filter(n.trade.days > 1000) %>%
    mutate(rank = mean.log.returns %>% desc() %>% min_rank()) %>%
    filter(rank <= limit) %>%
    arrange(rank) %>%
    select(ticker.symbol, rank, mean.log.returns, log.returns)
sp_500_hp

# Unnest high performing stocks
sp_500_hp_unnest <- sp_500_hp %>%
    select(ticker.symbol, log.returns) %>%
    unnest()
sp_500_hp_unnest

# Spread format conducive to cor()
sp_500_hp_spread <- sp_500_hp_unnest %>%
    spread(key = ticker.symbol, value = Log.Returns) %>%
    na.omit()
sp_500_hp_spread

# Correlation plot
sp_500_hp_spread %>%
    select(-Date) %>%
    cor() %>%
    corrplot(order   = "hclust", 
             addrect = 10)