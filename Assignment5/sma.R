# BDA400 Assignment 5
# Simple Moving Average (SMA)
# Uses base R only.

sma <- function(data, period) {
  if (!is.numeric(data)) {
    stop("data must be numeric")
  }
  if (length(period) != 1 || period <= 0 || period != as.integer(period)) {
    stop("period must be a positive integer")
  }
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  sma_values <- numeric(length(data) - period + 1)

  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }

  return(sma_values)
}

# Example test
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
print(sma(data, period = 3))
