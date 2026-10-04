# BDA400 / Data Science Tools and Techniques
# Assignment 5 - Technical Analysis using R, Development Phase
# Student: Waseem Rahmathulla Malayilthodi
# This implementation uses standard/base R functions only.

source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

cat("========================================\n")
cat("BDA400 Assignment 5 - Function Tests\n")
cat("========================================\n\n")

data1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
cat("SMA (period = 3):\n")
print(sma(data1, 3))
cat("\n")

cat("EMA (period = 3):\n")
print(ema(data1, 3))
cat("\n")

data_macd <- c(100, 105, 110, 115, 120, 125, 130)
cat("MACD (short = 3, long = 5, signal = 2):\n")
print(macd(data_macd, 3, 5, 2))
cat("\n")

cat("Population standard deviation:\n")
print(stdev(data1))
cat("\n")

reg_data <- c(10, 12, 15, 18, 20, 23, 25, 28, 30, 33)
cat("Linear regression (length = 5, offset = 0):\n")
print(linreg(reg_data, 5, 0))
cat("\n")

rsi_data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
cat("RSI (period = 5):\n")
print(rsi(rsi_data, 5))
cat("\n")

stoch_data <- c(
  45, 50, 48, 55, 52, 49, 58, 60, 65, 62,
  64, 67, 66, 70, 72, 69, 73, 75, 74, 78,
  80, 77, 79, 82, 84
)
cat("Stochastic RSI (RSI period = 14, K = 3, D = 3):\n")
print(stoch_rsi(stoch_data, 14, 3, 3))
cat("\n")

arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

cat("Crossover signals:\n")
print(crossover(arr1, arr2))
cat("\n")

cat("Crossunder signals:\n")
print(crossunder(arr1, arr2))
cat("\n")

stopifnot(length(sma(data1, 3)) == 7)
stopifnot(length(ema(data1, 3)) == length(data1))
stopifnot(abs(stdev(c(1, 2, 3)) - sqrt(2 / 3)) < 1e-10)
stopifnot(crossover(c(1, 3), c(2, 2))[2] == "Up")
stopifnot(crossunder(c(3, 1), c(2, 2))[2] == "True")

cat("All basic checks passed.\n")
