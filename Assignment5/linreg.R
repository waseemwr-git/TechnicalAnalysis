# BDA400 / Data Science Tools and Techniques
# Assignment 5 - Technical Analysis using R, Development Phase
# Student: Waseem Rahmathulla Malayilthodi
# This implementation uses standard/base R functions only.

# Linear Regression
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  if (!is.numeric(regressionSource) || length(regressionSource) == 0) {
    stop("regressionSource must be a non-empty numeric vector")
  }
  if (any(is.na(regressionSource))) {
    stop("regressionSource cannot contain NA values")
  }

  n <- length(regressionSource)

  if (length(regressionLength) != 1 || is.na(regressionLength) ||
      regressionLength <= 0 || regressionLength != floor(regressionLength)) {
    stop("regressionLength must be a positive integer")
  }

  if (length(regressionOffset) != 1 || is.na(regressionOffset) ||
      regressionOffset < 0 || regressionOffset != floor(regressionOffset)) {
    stop("regressionOffset must be a non-negative integer")
  }

  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }

  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)

  if (start_index > end_index) {
    stop("The selected regression range is empty")
  }

  source_subset <- regressionSource[start_index:end_index]

  if (length(source_subset) < 2) {
    stop("At least two data points are required for linear regression")
  }

  index_values <- seq_len(length(source_subset))
  sum_index <- sum(index_values)
  sum_source <- sum(source_subset)
  mean_index <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)

  numerator <- sum((index_values - mean_index) *
                     (source_subset - mean_source))
  denominator <- sum((index_values - mean_index) ^ 2)

  if (denominator == 0) {
    stop("Linear regression denominator is zero")
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
