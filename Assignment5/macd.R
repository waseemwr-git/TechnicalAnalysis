# BDA400 / Data Science Tools and Techniques
# Assignment 5 - Technical Analysis using R, Development Phase
# Student: Waseem Rahmathulla Malayilthodi
# This implementation uses standard/base R functions only.

# Moving Average Convergence Divergence (MACD)
# Dependency: source ema.R before using this function.
macd <- function(data, short_period, long_period, signal_period) {
  if (!exists("ema", mode = "function")) {
    stop("ema() is required. Source ema.R before using macd().")
  }

  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  macd_line <- short_ema - long_ema
  signal_line <- ema(macd_line, signal_period)
  histogram <- macd_line - signal_line

  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )

  return(result)
}
