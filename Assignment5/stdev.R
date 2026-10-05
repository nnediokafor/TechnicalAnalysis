# BDA400 Assignment 5
# Standard Deviation
# Uses the population standard deviation formula provided in the assignment.
# Uses base R only.

stdev <- function(data) {
  if (!is.numeric(data)) {
    stop("data must be numeric")
  }
  if (length(data) == 0) {
    stop("data cannot be empty")
  }

  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values * diff_values
  variance <- sum(squared_diff) / length(squared_diff)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}

# Example test
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
print(stdev(data))
