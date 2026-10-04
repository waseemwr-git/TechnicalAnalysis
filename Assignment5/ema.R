# BDA400 / Data Science Tools and Techniques
# Assignment 5 - Technical Analysis using R, Development Phase
# Student: Waseem Rahmathulla Malayilthodi
# This implementation uses standard/base R functions only.

# Exponential Moving Average (EMA)
ema <- function(data, period) {
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

  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  ema_values[1] <- data[1]

  if (length(data) > 1) {
    for (i in 2:length(data)) {
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier +
        ema_values[i - 1]
    }
  }

  return(ema_values)
}
