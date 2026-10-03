# BDA400 Assignment 2 - Technical Analysis using R, Preliminary Stage
# Student: Nnedi Okafor
# This script reads stock symbols from portfolio.txt, downloads stock data,
# calculates required statistics, displays the data, and creates visualizations.

# -----------------------------
# 1. Install required packages
# -----------------------------
required_packages <- c("quantmod", "TTR")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(quantmod)
library(TTR)

# -----------------------------
# 2. Read portfolio.txt
# -----------------------------
portfolio_file <- "portfolio.txt"

symbols <- readLines(portfolio_file)
symbols <- trimws(symbols)
symbols <- symbols[symbols != ""]

cat("Stock symbols in portfolio:\n")
print(symbols)

# -----------------------------
# 3. Function to load stock data
# -----------------------------
load_stock_data <- function(portfolio_file = "portfolio.txt") {
  symbols <- readLines(portfolio_file)
  symbols <- trimws(symbols)
  symbols <- symbols[symbols != ""]

  stock_list <- list()

  for (symbol in symbols) {
    cat("\nDownloading data for", symbol, "...\n")

    stock_data <- getSymbols(
      Symbols = symbol,
      src = "yahoo",
      from = Sys.Date() - 180,
      to = Sys.Date(),
      auto.assign = FALSE
    )

    stock_list[[symbol]] <- stock_data
  }

  return(stock_list)
}

stock_data <- load_stock_data(portfolio_file)

# -----------------------------
# 4. Statistical mode function
# -----------------------------
statistical_mode <- function(x) {
  x <- na.omit(x)
  unique_values <- unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]
}

# -----------------------------
# 5. Function to calculate statistics
# -----------------------------
calculate_statistics <- function(stock_xts, symbol) {
  close_prices <- na.omit(Cl(stock_xts))

  moving_average_20 <- SMA(close_prices, n = 20)

  stats <- data.frame(
    Symbol = symbol,
    Mean = mean(close_prices),
    Mode = statistical_mode(as.numeric(close_prices)),
    Median = median(close_prices),
    Standard_Deviation = sd(close_prices),
    Latest_20_Day_Moving_Average = as.numeric(last(na.omit(moving_average_20)))
  )

  return(stats)
}

statistics_list <- list()

for (symbol in names(stock_data)) {
  statistics_list[[symbol]] <- calculate_statistics(stock_data[[symbol]], symbol)
}

statistics_table <- do.call(rbind, statistics_list)

cat("\nCalculated statistics:\n")
print(statistics_table)

# -----------------------------
# 6. Display imported stock data
# -----------------------------
for (symbol in names(stock_data)) {
  cat("\n==============================\n")
  cat("Stock data for:", symbol, "\n")
  cat("==============================\n")

  print(head(stock_data[[symbol]]))
  print(tail(stock_data[[symbol]]))
}

# -----------------------------
# 7. Create visualizations
# -----------------------------
for (symbol in names(stock_data)) {
  chartSeries(
    stock_data[[symbol]],
    name = paste(symbol, "- Stock Price"),
    theme = chartTheme("white")
  )

  addSMA(n = 20, on = 1)
}

# -----------------------------
# 8. Simple closing-price plots
# -----------------------------
for (symbol in names(stock_data)) {
  close_prices <- Cl(stock_data[[symbol]])

  plot(
    close_prices,
    main = paste(symbol, "Closing Price"),
    xlab = "Date",
    ylab = "Closing Price"
  )
}

cat("\nAssignment 2 preliminary technical analysis complete.\n")
