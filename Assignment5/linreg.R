# BDA400 Assignment 5
# Linear Regression
# Follows the assignment pseudocode and uses base R only.

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  if (!is.numeric(regressionSource)) {
    stop("regressionSource must be numeric")
  }

  n <- length(regressionSource)

  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }

  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)

  source_subset <- regressionSource[start_index:end_index]
  index_values <- 1:length(source_subset)

  sum_index <- sum(index_values)
  sum_source <- sum(source_subset)

  mean_index <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)

  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  if (denominator == 0) {
    stop("Cannot calculate slope because the denominator is zero")
  }

  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept

  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )

  return(result)
}

# Example test
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
print(linreg(data, regressionLength = 5, regressionOffset = 0))
