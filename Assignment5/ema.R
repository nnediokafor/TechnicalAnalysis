# BDA400 Assignment 5
# Exponential Moving Average (EMA)
# Uses base R only.

ema <- function(data, period) {
  if (!is.numeric(data)) {
    stop("data must be numeric")
  }
  if (length(data) == 0) {
    return(numeric(0))
  }
  if (length(period) != 1 || period <= 0 || period != as.integer(period)) {
    stop("period must be a positive integer")
  }

  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))

  ema_values[1] <- data[1]

  if (length(data) > 1) {
    for (i in 2:length(data)) {
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}

# Example test
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
print(ema(data, period = 3))
