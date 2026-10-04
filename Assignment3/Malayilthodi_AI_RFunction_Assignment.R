# AI Assistance Declaration: I used ChatGPT (GPT-5.6 Sol) on October 3, 2026 for ideation,
# R function drafting, prompt refinement, documentation wording, and debugging suggestions.
# Prompts used: “Write an R function that removes outliers from a numeric vector using the IQR method”;
# “Revise the function to include comments, argument validation, and return a clean result”;
# “Explain what each line of the R code does and how you might test if it works correctly”;
# “Update the function to handle NA values”; “Optimize the function for efficiency and readability”;
# “How could you test this function with edge cases?”; “Summarize how this R function changed across revisions”;
# and “Highlight what improvements were human-driven vs. AI-generated.” I verified outputs using test cases,
# a manual IQR check, an independent comparison with boxplot.stats(), and a self spot-check.
# All final calculations are done by myself. I am responsible for the accuracy and originality of this work.
# 
# BDA400 - Assignment 3: Prompting R Functions with AI
# Student: Waseem Rahmathulla Malayilthodi
# Date: October 3, 2026
#
# ORIGINAL TASK DESCRIPTION
# Remove outliers from a numeric vector using the IQR method and return a clean
# numeric vector.
#
# AI PROMPTS USED
# 1. "Write an R function that removes outliers from a numeric vector using the
#    IQR method."
# 2. "Revise the function to include comments, argument validation, and return a
#    clean result."
# 3. "Explain what each line of the R code does and how you might test if it works
#    correctly."
# 4. "Update the function to handle NA values."
# 5. "Optimize the function for efficiency and readability."
# 6. "How could you test this function with edge cases?"
# 7. "Summarize how this R function changed across revisions. Highlight what
#    improvements were human-driven vs. AI-generated."

# -----------------------------------------------------------------------------
# VERSION 1 - AI-GENERATED FUNCTION
# -----------------------------------------------------------------------------

# Define a function that accepts one numeric vector named x.
remove_outliers_v1 <- function(x) {
  # Calculate the first quartile (25th percentile).
  q1 <- quantile(x, 0.25)

  # Calculate the third quartile (75th percentile).
  q3 <- quantile(x, 0.75)

  # Calculate the interquartile range.
  iqr_value <- q3 - q1

  # Calculate the lower outlier boundary.
  lower_bound <- q1 - 1.5 * iqr_value

  # Calculate the upper outlier boundary.
  upper_bound <- q3 + 1.5 * iqr_value

  # Keep only observations inside the two IQR boundaries.
  x[x >= lower_bound & x <= upper_bound]
}

# LIMITATION FOUND DURING TESTING
# The first version works when x has no missing values, but quantile() produces
# an error if x contains NA because na.rm = TRUE was not supplied. The function
# also has no validation for non-numeric input or invalid settings.

# Synthetic test data used to expose the limitation.
sample_data <- c(10, 15, 999, 20, 25, NA)

# Capture the expected first-version error without stopping the script.
draft_test <- tryCatch(
  remove_outliers_v1(sample_data),
  error = function(e) paste('ERROR:', conditionMessage(e))
)

# -----------------------------------------------------------------------------
# VERSION 2 - FINAL DEBUGGED AND IMPROVED FUNCTION
# -----------------------------------------------------------------------------

# Define the final function. x is the numeric vector and multiplier controls the
# width of the IQR fences. The conventional value is 1.5.
remove_outliers_iqr <- function(x, multiplier = 1.5) {
  # Stop if x is not numeric because quartiles require numeric data.
  if (!is.numeric(x)) {
    stop('x must be a numeric vector.')
  }

  # Validate the multiplier before using it in the boundary calculation.
  if (length(multiplier) != 1L ||
      !is.numeric(multiplier) ||
      is.na(multiplier) ||
      !is.finite(multiplier) ||
      multiplier < 0) {
    stop('multiplier must be one finite, non-negative numeric value.')
  }

  # Return a clean empty vector if the input itself is empty.
  if (length(x) == 0L) {
    return(numeric(0))
  }

  # Infinite values are treated as invalid rather than as ordinary observations.
  if (any(is.infinite(x))) {
    stop('x must not contain Inf or -Inf.')
  }

  # Remove missing values before calculating quartiles.
  clean_x <- x[!is.na(x)]

  # If every value was missing, return an empty numeric vector.
  if (length(clean_x) == 0L) {
    return(numeric(0))
  }

  # Calculate Q1 using R's standard type-7 quantile definition.
  q1 <- unname(quantile(clean_x, probs = 0.25, type = 7))

  # Calculate Q3 using the same quantile definition.
  q3 <- unname(quantile(clean_x, probs = 0.75, type = 7))

  # Calculate the interquartile range.
  iqr_value <- q3 - q1

  # Calculate the lower Tukey-style fence.
  lower_bound <- q1 - multiplier * iqr_value

  # Calculate the upper Tukey-style fence.
  upper_bound <- q3 + multiplier * iqr_value

  # Keep values that fall within the inclusive boundaries.
  result <- clean_x[clean_x >= lower_bound & clean_x <= upper_bound]

  # Return the cleaned numeric vector.
  return(result)
}

# -----------------------------------------------------------------------------
# TESTING AND VERIFICATION
# -----------------------------------------------------------------------------

# Main synthetic test: 999 should be identified as an outlier and NA ignored.
main_result <- remove_outliers_iqr(sample_data)
manual_expected <- c(10, 15, 20, 25)
main_matches_expected <- identical(as.numeric(main_result), manual_expected)

# Independent R cross-check using boxplot.stats().
sample_no_na <- sample_data[!is.na(sample_data)]
known_outliers <- boxplot.stats(sample_no_na, coef = 1.5)$out
boxplot_clean <- sample_no_na[!(sample_no_na %in% known_outliers)]
main_matches_boxplot <- identical(as.numeric(main_result), as.numeric(boxplot_clean))

# Edge case 1: no outliers should return all observations.
edge_no_outliers <- c(5, 6, 7, 8, 9)
edge_no_outliers_result <- remove_outliers_iqr(edge_no_outliers)

# Edge case 2: identical observations give IQR = 0 and should all be retained.
edge_same_values <- c(4, 4, 4, 4, 4)
edge_same_values_result <- remove_outliers_iqr(edge_same_values)

# Edge case 3: multiple NA values are ignored and 100 is outside the IQR fence.
edge_na_values <- c(1, 2, 3, 100, NA, NA)
edge_na_values_result <- remove_outliers_iqr(edge_na_values)

# Edge case 4: non-numeric input should return a clear validation error.
invalid_input_test <- tryCatch(
  remove_outliers_iqr(c('a', 'b')),
  error = function(e) paste('ERROR:', conditionMessage(e))
)

# Edge case 5: infinite values should return a clear validation error.
infinite_input_test <- tryCatch(
  remove_outliers_iqr(c(1, 2, Inf, 3)),
  error = function(e) paste('ERROR:', conditionMessage(e))
)

# Edge case 6: a negative multiplier should return a validation error.
invalid_multiplier_test <- tryCatch(
  remove_outliers_iqr(c(1, 2, 3), multiplier = -1),
  error = function(e) paste('ERROR:', conditionMessage(e))
)

# -----------------------------------------------------------------------------
# OUTPUT LOG
# Running this script in RStudio writes the same test evidence to a text file.
# -----------------------------------------------------------------------------

test_log <- capture.output({
  cat('BDA400 Assignment 3 - Test Output Log\n')
  cat('Student: Waseem Rahmathulla Malayilthodi\n\n')

  cat('1. First-version test with NA value:\n')
  print(draft_test)

  cat('\n2. Main synthetic sample:\n')
  print(sample_data)
  cat('Final cleaned result:\n')
  print(main_result)
  cat('Matches manually expected result:', main_matches_expected, '\n')

  cat('\n3. Independent comparison with boxplot.stats():\n')
  cat('Outlier(s) reported by boxplot.stats():\n')
  print(known_outliers)
  cat('Clean vector based on boxplot.stats():\n')
  print(boxplot_clean)
  cat('Final function matches boxplot.stats() result:', main_matches_boxplot, '\n')

  cat('\n4. Edge case - no outliers:\n')
  print(edge_no_outliers_result)

  cat('\n5. Edge case - identical values:\n')
  print(edge_same_values_result)

  cat('\n6. Edge case - NA values and one outlier:\n')
  print(edge_na_values_result)

  cat('\n7. Edge case - non-numeric input:\n')
  print(invalid_input_test)

  cat('\n8. Edge case - infinite value:\n')
  print(infinite_input_test)

  cat('\n9. Edge case - negative multiplier:\n')
  print(invalid_multiplier_test)
})

# Display the log in the RStudio console.
cat(test_log, sep = '\n')

# Save the console-style output log for submission evidence.
writeLines(test_log, con = 'Malayilthodi_Test_Output_Log.txt')

# -----------------------------------------------------------------------------
# SHORT DEBUGGING REFLECTION (4-5 sentences)
# -----------------------------------------------------------------------------
# AI gave me a useful starting function, but the first version did not handle NA
# values and had no argument checks. Testing with synthetic data exposed the
# missing-value problem immediately. I refined the function to remove missing
# values safely, reject invalid inputs, and make the IQR multiplier explicit.
# I also compared the cleaned result with both my expected result and
# boxplot.stats() instead of assuming the generated code was correct.
