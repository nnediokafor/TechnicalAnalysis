# BDA400 Assignment 5
# Stochastic RSI (StochRSI)
# Uses base R only.

sma <- function(data, period) {
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  sma_values <- numeric(length(data) - period + 1)

  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    if (any(is.na(current_window))) {
      sma_values[i] <- NA_real_
    } else {
      sma_values[i] <- sum(current_window) / period
    }
  }

  return(sma_values)
}

rsi <- function(data, period) {
  if (length(data) <= period) {
    stop("data must contain more values than the period")
  }

  diff_values <- diff(data)
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else if (diff_values[i] < 0) {
      losses[i] <- abs(diff_values[i])
    }
  }

  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values <- rep(NA_real_, length(data))

  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    if (avg_loss == 0) {
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}

stoch_rsi <- function(data, period, k_period, d_period) {
  rsi_values <- rsi(data, period)

  valid_rsi <- rsi_values[!is.na(rsi_values)]
  if (length(valid_rsi) == 0) {
    stop("No valid RSI values were produced")
  }

  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)

  if (max_rsi == min_rsi) {
    k_values <- rep(0, length(rsi_values))
    k_values[is.na(rsi_values)] <- NA_real_
  } else {
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }

  k_line <- sma(k_values, k_period)
  d_line <- sma(k_line, d_period)

  result <- list(
    k_line = k_line,
    d_line = d_line
  )

  return(result)
}

# Example test
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62, 64, 67, 66, 70, 72, 71, 74, 76, 75, 78)
print(stoch_rsi(data, period = 5, k_period = 3, d_period = 3))
