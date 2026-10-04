# BDA400 / Data Science Tools and Techniques
# Assignment 5 - Technical Analysis using R, Development Phase
# Student: Waseem Rahmathulla Malayilthodi
# This implementation uses standard/base R functions only.

# Simple Moving Average (SMA)
sma <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0) {
    stop("data must be a non-empty numeric vector")
  }

  if (length(period) != 1 || is.na(period) || period <= 0 ||
      period != floor(period)) {
    stop("period must be a positive integer")
  }

  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  sma_values <- numeric(length(data) - period + 1)

  for (i in seq_len(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]

    if (any(is.na(current_window))) {
      sma_values[i] <- NA_real_
    } else {
      mean_value <- sum(current_window) / period
      sma_values[i] <- mean_value
    }
  }

  return(sma_values)
}
