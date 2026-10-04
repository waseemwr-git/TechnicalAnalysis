# BDA400 / Data Science Tools and Techniques
# Assignment 5 - Technical Analysis using R, Development Phase
# Student: Waseem Rahmathulla Malayilthodi
# This implementation uses standard/base R functions only.

# Crossover
crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same.
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  if (!is.numeric(arr1) || !is.numeric(arr2) || length(arr1) == 0) {
    stop("arr1 and arr2 must be non-empty numeric vectors")
  }

  if (any(is.na(arr1)) || any(is.na(arr2))) {
    stop("arr1 and arr2 cannot contain NA values")
  }

  crossover_signals <- rep("None", length(arr1))

  if (length(arr1) > 1) {
    for (i in 2:length(arr1)) {
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}
