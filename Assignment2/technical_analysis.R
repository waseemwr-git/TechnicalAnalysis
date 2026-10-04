# BDA400 - Data Science Tools and Techniques
# Assignment 2 - Technical Analysis using R, Preliminary Stage
# Student: Waseem
#
# This script:
# 1. Reads stock symbols from portfolio.txt
# 2. Downloads stock data with quantmod
# 3. Stores each stock as a separate data frame
# 4. Calculates a 20-trading-day simple moving average, mean, mode,
#    median, and standard deviation using Adjusted Close
# 5. Displays loaded data, statistics, and visualizations

# -----------------------------
# 1. Install/load required packages
# -----------------------------
required_packages <- c("quantmod", "TTR")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(quantmod)
library(TTR)

# -----------------------------
# 2. Read portfolio symbols
# -----------------------------
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) {
    stop("portfolio.txt was not found in the working directory.")
  }

  symbols <- readLines(file, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[nzchar(symbols)]

  if (length(symbols) == 0) {
    stop("portfolio.txt does not contain any stock symbols.")
  }

  unique(toupper(symbols))
}

# -----------------------------
# 3. Import stock data
# -----------------------------
load_stock_data <- function(portfolio_file = "portfolio.txt",
                             from = Sys.Date() - 365) {

  symbols <- read_portfolio(portfolio_file)
  stock_data <- list()

  for (symbol in symbols) {
    message("Downloading data for ", symbol, "...")

    tryCatch({
      x <- getSymbols(
        Symbols = symbol,
        src = "yahoo",
        from = from,
        auto.assign = FALSE
      )

      # Convert the xts object into a regular data frame.
      df <- data.frame(
        Date = as.Date(index(x)),
        coredata(x),
        row.names = NULL,
        check.names = FALSE
      )

      # Standardize column names.
      names(df) <- c(
        "Date",
        "Open",
        "High",
        "Low",
        "Close",
        "Volume",
        "Adjusted"
      )

      stock_data[[symbol]] <- df

    }, error = function(e) {
      warning("Could not download ", symbol, ": ", conditionMessage(e))
    })
  }

  if (length(stock_data) == 0) {
    stop("No stock data could be downloaded.")
  }

  stock_data
}

# -----------------------------
# 4. Statistical mode
# -----------------------------
calculate_mode <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  # Stock prices are usually continuous, so exact duplicates may be rare.
  # In that case, the function reports that there is no repeated mode.
  frequency <- table(x)
  max_frequency <- max(frequency)

  if (max_frequency == 1) {
    return(NA_real_)
  }

  as.numeric(names(frequency)[frequency == max_frequency][1])
}

# -----------------------------
# 5. Calculate required statistics
# -----------------------------
calculate_statistics <- function(stock_df, moving_average_period = 20) {

  if (!"Adjusted" %in% names(stock_df)) {
    stop("The data frame must contain an 'Adjusted' column.")
  }

  adjusted <- as.numeric(stock_df$Adjusted)
  adjusted <- adjusted[is.finite(adjusted)]

  if (length(adjusted) == 0) {
    stop("No valid adjusted closing prices were found.")
  }

  moving_average <- SMA(adjusted, n = moving_average_period)

  statistics <- data.frame(
    Statistic = c(
      paste0("20-Day Simple Moving Average (Latest)"),
      "Mean",
      "Mode",
      "Median",
      "Standard Deviation"
    ),
    Value = c(
      tail(moving_average[is.finite(moving_average)], 1),
      mean(adjusted),
      calculate_mode(adjusted),
      median(adjusted),
      sd(adjusted)
    )
  )

  statistics
}

# -----------------------------
# 6. Display loaded data
# -----------------------------
display_stock_data <- function(stock_data, rows = 10) {

  for (symbol in names(stock_data)) {
    cat("\n========================================\n")
    cat("Stock:", symbol, "\n")
    cat("========================================\n")

    print(utils::head(stock_data[[symbol]], rows))
  }
}

# -----------------------------
# 7. Display statistics
# -----------------------------
display_statistics <- function(stock_data, moving_average_period = 20) {

  all_statistics <- list()

  for (symbol in names(stock_data)) {
    stats <- calculate_statistics(
      stock_data[[symbol]],
      moving_average_period = moving_average_period
    )

    stats$Symbol <- symbol
    stats <- stats[, c("Symbol", "Statistic", "Value")]

    all_statistics[[symbol]] <- stats

    cat("\n========================================\n")
    cat("Statistics:", symbol, "\n")
    cat("========================================\n")
    print(stats, row.names = FALSE)
  }

  do.call(rbind, all_statistics)
}

# -----------------------------
# 8. Visualize stock prices
# -----------------------------
plot_stock_data <- function(stock_data) {

  for (symbol in names(stock_data)) {

    df <- stock_data[[symbol]]

    chart_data <- xts(
      df[, c("Open", "High", "Low", "Close", "Volume", "Adjusted")],
      order.by = df$Date
    )

    chartSeries(
      chart_data,
      name = paste(symbol, "Stock Data"),
      theme = chartTheme("white"),
      TA = "addSMA(n=20, col='blue')"
    )
  }
}

# -----------------------------
# 9. Run the analysis
# -----------------------------
portfolio_symbols <- read_portfolio("portfolio.txt")
cat("Portfolio symbols:\n")
print(portfolio_symbols)

stock_data <- load_stock_data("portfolio.txt")

display_stock_data(stock_data, rows = 10)

statistics_results <- display_statistics(
  stock_data,
  moving_average_period = 20
)

cat("\nCombined statistics:\n")
print(statistics_results, row.names = FALSE)

# Optional CSV output of calculated statistics.
write.csv(
  statistics_results,
  file = "statistics_results.csv",
  row.names = FALSE
)

# Display visualizations.
plot_stock_data(stock_data)
