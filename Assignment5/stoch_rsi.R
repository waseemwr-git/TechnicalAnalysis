# BDA400 / Data Science Tools and Techniques
# Assignment 5 - Technical Analysis using R, Development Phase
# Student: Waseem Rahmathulla Malayilthodi
# This implementation uses standard/base R functions only.

# Stochastic RSI (StochRSI)
# Dependencies: source sma.R and rsi.R before using this function.
stoch_rsi <- function(data, period, k_period, d_period) {
  if (!exists("rsi", mode = "function")) {
    stop("rsi() is required. Source rsi.R before using stoch_rsi().")
  }
  if (!exists("sma", mode = "function")) {
    stop("sma() is required. Source sma.R before using stoch_rsi().")
  }

  if (length(k_period) != 1 || is.na(k_period) || k_period <= 0 ||
      k_period != floor(k_period)) {
    stop("k_period must be a positive integer")
  }
  if (length(d_period) != 1 || is.na(d_period) || d_period <= 0 ||
      d_period != floor(d_period)) {
    stop("d_period must be a positive integer")
  }

  rsi_values <- rsi(data, period)
  valid_rsi <- rsi_values[!is.na(rsi_values)]

  if (length(valid_rsi) == 0) {
    stop("No valid RSI values are available")
  }

  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  k_values <- rep(NA_real_, length(rsi_values))

  if (max_rsi == min_rsi) {
    k_values[!is.na(rsi_values)] <- 0
  } else {
    k_values[!is.na(rsi_values)] <-
      (rsi_values[!is.na(rsi_values)] - min_rsi) /
      (max_rsi - min_rsi)
  }

  k_line <- sma(k_values, k_period)
  d_line <- sma(k_line, d_period)

  result <- list(
    k_line = k_line,
    d_line = d_line
  )

  return(result)
}
