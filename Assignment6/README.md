# BDA400 Assignment 6 - Technical Analysis using R, Visualization Phase

**Student:** Waseem Rahmathulla Malayilthodi

## Project

This project is an interactive R Shiny technical-analysis dashboard. It fetches historical stock data from Yahoo Finance and lets the user select a stock symbol, date range, time frame, chart type, technical indicators, and trading-rule parameters.

## Main features

- Yahoo Finance data through `quantmod`
- Daily, weekly, and monthly views
- Line, candlestick, and area charts
- Moving Average overlay with user-defined short and long periods
- RSI panel with 30/70 reference levels
- MACD, signal line, and histogram
- Dynamic on/off controls for indicators
- Moving-average crossover trading strategy
- Buy/Sell annotations and Hold states
- Error handling for unavailable data and invalid settings
- Recent-data and trading-signal table

## Required packages

```r
install.packages("shiny")
install.packages("ggplot2")
install.packages("quantmod")
install.packages("TTR")
```

The script also checks for missing packages and installs them automatically.

## How to run

1. Open `app.R` in RStudio.
2. Run the complete script or click **Run App**.
3. Enter or choose a stock symbol.
4. Select a date range and time frame.
5. Choose Line, Candlestick, or Area.
6. Turn Moving Averages, RSI, and MACD on or off.
7. Adjust the indicator and moving-average crossover parameters.
8. Review the Buy/Sell annotations and recent signal table.

## Repository location

```text
TechnicalAnalysis/
└── Assignment6/
    ├── app.R
    ├── README.md
    └── WaseemRahmathullaMalayilthodi_BDA400_A06.docx
```

GitHub repository: https://github.com/waseemwr-git/TechnicalAnalysis
