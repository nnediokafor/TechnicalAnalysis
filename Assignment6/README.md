# BDA400 Assignment 6 - Technical Analysis using R, Visualization Phase

**Student:** Nnedi Okafor

## Project Description

This project is an interactive stock portfolio dashboard built with R Shiny. It retrieves historical stock-market data from Yahoo Finance and allows the user to analyze stock performance using different time frames, chart types, technical indicators, and trading signals.

The dashboard includes:

- Yahoo Finance historical stock-data retrieval
- Daily, Weekly, and Monthly time frames
- Line, Area, and Candlestick charts
- Moving Average indicators
- Relative Strength Index (RSI)
- Moving Average Convergence Divergence (MACD)
- Dynamic on/off controls for technical indicators
- Custom short and long moving-average periods
- Buy, Sell, and Hold trading signals
- Buy and Sell annotations on the stock-price chart
- Basic error handling for invalid symbols, dates, missing data, and insufficient history

## Required R Packages

```r
install.packages("shiny")
install.packages("ggplot2")
install.packages("quantmod")
