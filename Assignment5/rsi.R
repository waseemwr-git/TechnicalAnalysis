# BDA400 / Data Science Tools and Techniques
# Assignment 5 - Technical Analysis using R, Development Phase
# Student: Waseem Rahmathulla Malayilthodi
# This implementation uses standard/base R functions only.

# Relative Strength Index (RSI)
rsi <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0) {
    stop("data must be a non-empty numeric vector")
  }
  if (any(is.na(data))) {
    stop("data cannot contain NA values")
  }

  if (length(period) != 1 || is.na(period) || period <= 0 ||
      period != floor(period)) {
    stop("period must be a positive integer")
  }

  if (length(data) <= period) {
    stop("Data length must be greater than the RSI period")
  }

  diff_values <- data[2:length(data)] - data[1:(length(data) - 1)]
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
      losses[i] <- 0
    } else {
      gains[i] <- 0
      losses[i] <- abs(diff_values[i])
    }
  }

  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values <- rep(NA_real_, length(data))

  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    if (avg_loss == 0 && avg_gain == 0) {
      rsi_values[i] <- 50
    } else if (avg_loss == 0) {
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}
