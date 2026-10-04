# BDA400 - Data Science Tools and Techniques
# Assignment 4 - Ask AI: Clean This Dataset with R and dplyr
# Student: Waseem Malayilthodi
#
# AI Assistance Declaration: I used ChatGPT (GPT-5.6 Sol) for ideation,
# draft data-cleaning steps, R code structure, comments, and writing assistance.
# Prompts used: see Malayilthodi_Waseem_AI_Appendix.txt.
# I verified outputs using RStudio console checks with summary(), dplyr::glimpse(),
# table(), nrow(), colSums(is.na()), duplicated(), and spot-checking records.
# All final calculations are done by myself. I am responsible for the accuracy
# and originality of this work.
#
# IMPORTANT: Run this complete script in RStudio before submission so the
# verification statement above is accurate and the output log is regenerated.

if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("The dplyr package is required. Install it with install.packages('dplyr').")
}

library(dplyr)

input_candidates <- c(
  "08 BDA400 Assignment 4 - sales_data_dirty.csv",
  "sales_data_dirty.csv"
)

input_file <- input_candidates[file.exists(input_candidates)][1]

if (is.na(input_file)) {
  stop(
    paste0(
      "Dataset not found. Put this R script in the same folder as either:\n",
      "  08 BDA400 Assignment 4 - sales_data_dirty.csv\n",
      "or\n",
      "  sales_data_dirty.csv"
    )
  )
}

sales_raw <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

sales_original <- sales_raw

cat("\n================ ORIGINAL DATA ================\n")
cat("Rows:", nrow(sales_original), "\n")
cat("Columns:", ncol(sales_original), "\n\n")
cat("Structure:\n")
glimpse(sales_original)
cat("\nSummary:\n")
print(summary(sales_original))
cat("\nMissing values by column:\n")
print(colSums(is.na(sales_original)))
cat("\nExact duplicated rows:", sum(duplicated(sales_original)), "\n")
cat("Duplicated OrderID values:", sum(duplicated(sales_original$OrderID)), "\n")
cat("\nProduct labels:\n")
print(table(sales_original$Product, useNA = "ifany"))
cat("\nCustomer labels:\n")
print(table(sales_original$Customer, useNA = "ifany"))
cat("\nQuantity values:\n")
print(table(sales_original$Quantity, useNA = "ifany"))

quantity_non_missing <- sales_original$Quantity[!is.na(sales_original$Quantity)]
quantity_iqr_outliers <- boxplot.stats(quantity_non_missing)$out
cat("\nQuantity values flagged by boxplot.stats():\n")
print(sort(unique(quantity_iqr_outliers)))

# Data-quality issues:
# 1. Product contains missing values.
# 2. Quantity contains missing values.
# 3. Exact duplicate rows and repeated OrderID values are present.
# 4. Quantity contains invalid/unreasonable values including negative/zero
#    quantities and an extreme value of 1000.

sales_clean <- sales_original %>%
  distinct() %>%
  filter(
    !is.na(Product),
    !is.na(Quantity),
    !is.na(Customer)
  ) %>%
  mutate(
    Product = trimws(Product),
    Customer = tolower(trimws(Customer))
  ) %>%
  filter(
    Quantity > 0,
    Quantity <= 100,
    Price > 0
  ) %>%
  arrange(OrderID)

cat("\n================ CLEANED DATA ================\n")
cat("Rows:", nrow(sales_clean), "\n")
cat("Columns:", ncol(sales_clean), "\n\n")
cat("Structure:\n")
glimpse(sales_clean)
cat("\nSummary:\n")
print(summary(sales_clean))
cat("\nMissing values by column:\n")
print(colSums(is.na(sales_clean)))
cat("\nExact duplicated rows:", sum(duplicated(sales_clean)), "\n")
cat("Duplicated OrderID values:", sum(duplicated(sales_clean$OrderID)), "\n")
cat("\nProduct labels after cleaning:\n")
print(table(sales_clean$Product, useNA = "ifany"))
cat("\nCustomer labels after cleaning:\n")
print(table(sales_clean$Customer, useNA = "ifany"))
cat("\nQuantity range after cleaning:\n")
print(range(sales_clean$Quantity, na.rm = TRUE))

stopifnot(
  sum(is.na(sales_clean$Product)) == 0,
  sum(is.na(sales_clean$Quantity)) == 0,
  sum(is.na(sales_clean$Customer)) == 0,
  sum(duplicated(sales_clean)) == 0,
  sum(duplicated(sales_clean$OrderID)) == 0,
  all(sales_clean$Quantity > 0),
  all(sales_clean$Quantity <= 100),
  all(sales_clean$Price > 0)
)

write.csv(
  sales_clean,
  "Malayilthodi_Waseem_sales_data_clean.csv",
  row.names = FALSE
)

output_log <- c(
  "AI Assistance Declaration: I used ChatGPT (GPT-5.6 Sol) for ideation, draft data-cleaning steps, R code structure, comments, and writing assistance. Prompts used: see Malayilthodi_Waseem_AI_Appendix.txt. I verified outputs using RStudio console checks with summary(), dplyr::glimpse(), table(), nrow(), colSums(is.na()), duplicated(), and spot-checking records. All final calculations are done by myself. I am responsible for the accuracy and originality of this work.",
  "",
  "BDA400 Assignment 4 - Before/After Data Summary",
  paste("Generated in R on:", Sys.time()),
  "",
  "================ BEFORE CLEANING ================",
  paste("Rows:", nrow(sales_original)),
  paste("Columns:", ncol(sales_original)),
  "Missing values by column:",
  capture.output(print(colSums(is.na(sales_original)))),
  paste("Exact duplicated rows:", sum(duplicated(sales_original))),
  paste("Duplicated OrderID values:", sum(duplicated(sales_original$OrderID))),
  "Quantity summary:",
  capture.output(print(summary(sales_original$Quantity))),
  "Product labels:",
  capture.output(print(table(sales_original$Product, useNA = "ifany"))),
  "",
  "================ AFTER CLEANING ================",
  paste("Rows:", nrow(sales_clean)),
  paste("Columns:", ncol(sales_clean)),
  "Missing values by column:",
  capture.output(print(colSums(is.na(sales_clean)))),
  paste("Exact duplicated rows:", sum(duplicated(sales_clean))),
  paste("Duplicated OrderID values:", sum(duplicated(sales_clean$OrderID))),
  "Quantity summary:",
  capture.output(print(summary(sales_clean$Quantity))),
  "Product labels:",
  capture.output(print(table(sales_clean$Product, useNA = "ifany"))),
  "",
  "Sanity checks:",
  paste("No missing Product values:", sum(is.na(sales_clean$Product)) == 0),
  paste("No missing Quantity values:", sum(is.na(sales_clean$Quantity)) == 0),
  paste("No exact duplicate rows:", sum(duplicated(sales_clean)) == 0),
  paste("Unique OrderID values:", sum(duplicated(sales_clean$OrderID)) == 0),
  paste("All quantities are > 0 and <= 100:", all(sales_clean$Quantity > 0 & sales_clean$Quantity <= 100)),
  "",
  "Result: all stopifnot() verification checks passed."
)

writeLines(
  output_log,
  "Malayilthodi_Waseem_before_after_summary.txt"
)

cat("\nCleaning completed successfully.\n")
cat("Created: Malayilthodi_Waseem_sales_data_clean.csv\n")
cat("Created: Malayilthodi_Waseem_before_after_summary.txt\n")
