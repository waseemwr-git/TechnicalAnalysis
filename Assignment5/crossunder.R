# BDA400 / Data Science Tools and Techniques
# Assignment 5 - Technical Analysis using R, Development Phase
# Student: Waseem Rahmathulla Malayilthodi
# This implementation uses standard/base R functions only.

# Crossunder
crossunder <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  if (!is.numeric(arr1) || !is.numeric(arr2) || length(arr1) == 0) {
    stop("arr1 and arr2 must be non-empty numeric vectors")
  }

  if (any(is.na(arr1)) || any(is.na(arr2))) {
    stop("arr1 and arr2 cannot contain NA values")
  }

  crossunder_signals <- rep("False", length(arr1))
  crossunder_signals[1] <- "None"

  if (length(arr1) > 1) {
    for (i in 2:length(arr1)) {
      if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }

  return(crossunder_signals)
}
