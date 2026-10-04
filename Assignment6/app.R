# BDA400 / Data Science Tools and Techniques
# Assignment 6 - Technical Analysis using R, Visualization Phase
# Student: Waseem Rahmathulla Malayilthodi
#
# This Shiny dashboard:
# 1. Fetches historical stock data from Yahoo Finance.
# 2. Lets the user choose a stock, date range, time frame, and chart type.
# 3. Displays line, area, or candlestick price charts.
# 4. Adds Moving Average, RSI, and MACD indicators with on/off controls.
# 5. Generates Buy, Sell, and Hold signals from a moving-average crossover rule.
# 6. Annotates Buy/Sell events directly on the price chart.
#
# Run the complete file in RStudio. The app opens in the Viewer/browser.

# ============================================================
# STEP 1 - DATA COLLECTION AND SETUP
# ============================================================

# Required packages. The assignment specifically requires shiny,
# ggplot2, and quantmod. TTR is used for indicator calculations.
required_packages <- c("shiny", "ggplot2", "quantmod", "TTR")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

# Download stock data from Yahoo Finance and convert the xts
# object returned by quantmod into a regular data frame.
fetch_stock_data <- function(symbol, start_date, end_date) {

  symbol <- trimws(toupper(symbol))

  if (symbol == "") {
    stop("Please enter a stock symbol.")
  }

  # Add one day to the end date because Yahoo's 'to' value can
  # behave as an exclusive upper boundary.
  raw_data <- suppressWarnings(
    getSymbols(
      Symbols = symbol,
      src = "yahoo",
      from = as.Date(start_date),
      to = as.Date(end_date) + 1,
      auto.assign = FALSE
    )
  )

  if (NROW(raw_data) == 0) {
    stop("No stock data were returned for the selected inputs.")
  }

  result <- data.frame(
    Date = as.Date(index(raw_data)),
    Open = as.numeric(Op(raw_data)),
    High = as.numeric(Hi(raw_data)),
    Low = as.numeric(Lo(raw_data)),
    Close = as.numeric(Cl(raw_data)),
    Volume = as.numeric(Vo(raw_data)),
    Adjusted = as.numeric(Ad(raw_data)),
    stringsAsFactors = FALSE
  )

  result <- result[complete.cases(result[, c("Date", "Open", "High", "Low", "Close")]), ]

  if (nrow(result) == 0) {
    stop("The downloaded data contain no complete OHLC observations.")
  }

  return(result)
}

# Aggregate daily OHLCV data into weekly or monthly observations.
# The first open, highest high, lowest low, last close, total volume,
# and last adjusted close are retained for each period.
aggregate_time_frame <- function(data, time_frame) {

  if (time_frame == "Daily") {
    return(data)
  }

  if (time_frame == "Weekly") {
    weekday_number <- as.POSIXlt(data$Date)$wday
    weekday_number[weekday_number == 0] <- 7
    period_start <- data$Date - (weekday_number - 1)
    group_key <- as.character(period_start)
  } else {
    group_key <- format(data$Date, "%Y-%m")
  }

  groups <- split(seq_len(nrow(data)), group_key)

  rows <- lapply(groups, function(idx) {
    block <- data[idx, , drop = FALSE]

    data.frame(
      Date = block$Date[nrow(block)],
      Open = block$Open[1],
      High = max(block$High, na.rm = TRUE),
      Low = min(block$Low, na.rm = TRUE),
      Close = block$Close[nrow(block)],
      Volume = sum(block$Volume, na.rm = TRUE),
      Adjusted = block$Adjusted[nrow(block)],
      stringsAsFactors = FALSE
    )
  })

  aggregated <- do.call(rbind, rows)
  rownames(aggregated) <- NULL
  aggregated <- aggregated[order(aggregated$Date), ]

  return(aggregated)
}

# Calculate the selected technical indicators and trading signals.
calculate_analysis <- function(data,
                               short_ma_period,
                               long_ma_period,
                               rsi_period,
                               macd_fast,
                               macd_slow,
                               macd_signal) {

  if (short_ma_period >= long_ma_period) {
    stop("The short moving-average period must be smaller than the long period.")
  }

  if (macd_fast >= macd_slow) {
    stop("The MACD fast period must be smaller than the MACD slow period.")
  }

  if (nrow(data) < long_ma_period) {
    stop(
      paste0(
        "Not enough observations for the selected long moving average. ",
        "Choose a wider date range, a shorter period, or a more frequent time frame."
      )
    )
  }

  data$Short_MA <- as.numeric(TTR::SMA(data$Close, n = short_ma_period))
  data$Long_MA <- as.numeric(TTR::SMA(data$Close, n = long_ma_period))

  if (nrow(data) > rsi_period) {
    data$RSI <- as.numeric(TTR::RSI(data$Close, n = rsi_period))
  } else {
    data$RSI <- rep(NA_real_, nrow(data))
  }

  if (nrow(data) > macd_slow + macd_signal) {
    macd_result <- TTR::MACD(
      data$Close,
      nFast = macd_fast,
      nSlow = macd_slow,
      nSig = macd_signal,
      maType = "EMA",
      percent = FALSE
    )

    data$MACD <- as.numeric(macd_result[, 1])
    data$MACD_Signal <- as.numeric(macd_result[, 2])
    data$MACD_Histogram <- data$MACD - data$MACD_Signal
  } else {
    data$MACD <- rep(NA_real_, nrow(data))
    data$MACD_Signal <- rep(NA_real_, nrow(data))
    data$MACD_Histogram <- rep(NA_real_, nrow(data))
  }

  # Moving Average Crossover trading rule:
  # Buy  = short MA crosses above long MA.
  # Sell = short MA crosses below long MA.
  # Hold = no crossover event.
  previous_short <- c(NA_real_, head(data$Short_MA, -1))
  previous_long <- c(NA_real_, head(data$Long_MA, -1))

  buy_condition <-
    !is.na(data$Short_MA) &
    !is.na(data$Long_MA) &
    !is.na(previous_short) &
    !is.na(previous_long) &
    data$Short_MA > data$Long_MA &
    previous_short <= previous_long

  sell_condition <-
    !is.na(data$Short_MA) &
    !is.na(data$Long_MA) &
    !is.na(previous_short) &
    !is.na(previous_long) &
    data$Short_MA < data$Long_MA &
    previous_short >= previous_long

  data$Signal <- "Hold"
  data$Signal[buy_condition] <- "Buy"
  data$Signal[sell_condition] <- "Sell"

  return(data)
}

# ============================================================
# STEP 2 - SHINY UI AND STOCK VISUALIZATION
# ============================================================

ui <- fluidPage(

  titlePanel("Portfolio Technical Analysis Dashboard"),

  sidebarLayout(

    sidebarPanel(

      selectizeInput(
        "stock_symbol",
        "Stock Symbol:",
        choices = c("AAPL", "MSFT", "NVDA", "AMZN", "TSLA", "GOOGL", "META"),
        selected = "AAPL",
        options = list(create = TRUE)
      ),

      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = Sys.Date() - 365,
        end = Sys.Date()
      ),

      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),

      selectInput(
        "chart_type",
        "Chart Type:",
        choices = c("Line", "Candlestick", "Area"),
        selected = "Candlestick"
      ),

      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI", "MACD")
      ),

      tags$hr(),

      h4("Trading Rule Parameters"),

      numericInput(
        "short_ma_period",
        "Short MA Period:",
        value = 20,
        min = 2,
        max = 100,
        step = 1
      ),

      numericInput(
        "long_ma_period",
        "Long MA Period:",
        value = 50,
        min = 3,
        max = 250,
        step = 1
      ),

      numericInput(
        "rsi_period",
        "RSI Period:",
        value = 14,
        min = 2,
        max = 100,
        step = 1
      ),

      fluidRow(
        column(
          4,
          numericInput(
            "macd_fast",
            "MACD Fast:",
            value = 12,
            min = 2,
            max = 100,
            step = 1
          )
        ),
        column(
          4,
          numericInput(
            "macd_slow",
            "MACD Slow:",
            value = 26,
            min = 3,
            max = 200,
            step = 1
          )
        ),
        column(
          4,
          numericInput(
            "macd_signal",
            "Signal:",
            value = 9,
            min = 2,
            max = 100,
            step = 1
          )
        )
      ),

      checkboxInput(
        "show_signals",
        "Show Buy/Sell Annotations",
        value = TRUE
      ),

      actionButton(
        "refresh_data",
        "Refresh Data",
        class = "btn-primary"
      )
    ),

    mainPanel(

      h3(textOutput("stock_heading")),

      wellPanel(
        strong("Latest Trading Signal: "),
        textOutput("latest_signal", inline = TRUE)
      ),

      plotOutput(
        "stock_chart",
        height = "520px"
      ),

      conditionalPanel(
        condition = "input.technical_indicators.indexOf('RSI') >= 0",
        plotOutput(
          "rsi_chart",
          height = "260px"
        )
      ),

      conditionalPanel(
        condition = "input.technical_indicators.indexOf('MACD') >= 0",
        plotOutput(
          "macd_chart",
          height = "300px"
        )
      ),

      h4("Recent Data and Trading Signals"),

      tableOutput("signal_table")
    )
  )
)

# ============================================================
# STEP 3 - SERVER LOGIC AND TECHNICAL INDICATORS
# ============================================================

server <- function(input, output, session) {

  # Fetch data when the app starts or when the Refresh Data button is clicked.
  raw_stock_data <- eventReactive(
    c(input$refresh_data, input$stock_symbol, input$date_range),
    {
      req(input$stock_symbol, input$date_range)

      tryCatch(
        {
          fetch_stock_data(
            symbol = input$stock_symbol,
            start_date = input$date_range[1],
            end_date = input$date_range[2]
          )
        },
        error = function(e) {
          showNotification(
            paste("Data download error:", conditionMessage(e)),
            type = "error",
            duration = 8
          )
          return(NULL)
        }
      )
    },
    ignoreNULL = FALSE
  )

  # Apply the Daily/Weekly/Monthly time-frame selection.
  time_frame_data <- reactive({
    data <- raw_stock_data()

    validate(
      need(!is.null(data), "Stock data could not be downloaded."),
      need(nrow(data) > 0, "No observations are available.")
    )

    aggregate_time_frame(
      data = data,
      time_frame = input$time_frame
    )
  })

  # Calculate indicators and trading signals using the user's parameters.
  analysis_data <- reactive({
    data <- time_frame_data()

    tryCatch(
      {
        calculate_analysis(
          data = data,
          short_ma_period = input$short_ma_period,
          long_ma_period = input$long_ma_period,
          rsi_period = input$rsi_period,
          macd_fast = input$macd_fast,
          macd_slow = input$macd_slow,
          macd_signal = input$macd_signal
        )
      },
      error = function(e) {
        validate(need(FALSE, conditionMessage(e)))
      }
    )
  })

  output$stock_heading <- renderText({
    paste(
      toupper(input$stock_symbol),
      "-",
      input$time_frame,
      "Technical Analysis"
    )
  })

  output$latest_signal <- renderText({
    data <- analysis_data()
    tail(data$Signal, 1)
  })

  # ==========================================================
  # STEP 2 - VISUALIZE STOCK DATA
  # ==========================================================

  output$stock_chart <- renderPlot({

    data <- analysis_data()

    validate(
      need(nrow(data) > 1, "At least two observations are required to draw the chart.")
    )

    title_text <- paste(
      toupper(input$stock_symbol),
      input$time_frame,
      "Price"
    )

    # Start with a common ggplot object.
    p <- ggplot(data, aes(x = Date))

    if (input$chart_type == "Line") {

      p <- p +
        geom_line(
          aes(y = Close),
          linewidth = 0.8
        )

    } else if (input$chart_type == "Area") {

      p <- p +
        geom_area(
          aes(y = Close),
          alpha = 0.25
        ) +
        geom_line(
          aes(y = Close),
          linewidth = 0.7
        )

    } else {

      # Candlestick chart drawn directly with ggplot2.
      median_spacing <- median(as.numeric(diff(data$Date)), na.rm = TRUE)

      if (!is.finite(median_spacing) || median_spacing <= 0) {
        median_spacing <- 1
      }

      candle_width <- median_spacing * 0.65

      candle_data <- data
      candle_data$Direction <- ifelse(
        candle_data$Close >= candle_data$Open,
        "Up",
        "Down"
      )

      p <- p +
        geom_segment(
          data = candle_data,
          aes(
            x = Date,
            xend = Date,
            y = Low,
            yend = High
          ),
          linewidth = 0.4
        ) +
        geom_rect(
          data = candle_data,
          aes(
            xmin = Date - candle_width / 2,
            xmax = Date + candle_width / 2,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Direction
          ),
          linewidth = 0.25,
          color = "black"
        ) +
        scale_fill_manual(
          values = c("Up" = "darkgreen", "Down" = "firebrick"),
          name = "Price Direction"
        )
    }

    # ========================================================
    # STEP 3 - OVERLAY TECHNICAL INDICATORS
    # ========================================================

    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(
          aes(
            y = Short_MA,
            color = paste0("Short MA (", input$short_ma_period, ")")
          ),
          linewidth = 0.75,
          na.rm = TRUE
        ) +
        geom_line(
          aes(
            y = Long_MA,
            color = paste0("Long MA (", input$long_ma_period, ")")
          ),
          linewidth = 0.75,
          na.rm = TRUE
        )
    }

    # ========================================================
    # STEP 4 - TRADING RULES AND ANNOTATIONS
    # ========================================================

    if (isTRUE(input$show_signals)) {

      signal_data <- data[data$Signal %in% c("Buy", "Sell"), , drop = FALSE]

      if (nrow(signal_data) > 0) {
        p <- p +
          geom_point(
            data = signal_data,
            aes(
              y = Close,
              shape = Signal,
              color = Signal
            ),
            size = 3,
            stroke = 1
          ) +
          geom_text(
            data = signal_data,
            aes(
              y = Close,
              label = Signal,
              color = Signal
            ),
            vjust = -1,
            fontface = "bold",
            size = 3.4,
            check_overlap = TRUE
          )
      }
    }

    p +
      labs(
        title = title_text,
        subtitle = paste(
          "Yahoo Finance |",
          format(input$date_range[1]),
          "to",
          format(input$date_range[2])
        ),
        x = "Date",
        y = "Price",
        color = NULL,
        shape = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })

  # RSI is displayed in an aligned indicator panel because RSI has
  # a different 0-100 scale from stock prices.
  output$rsi_chart <- renderPlot({

    data <- analysis_data()

    validate(
      need(any(!is.na(data$RSI)), "Not enough observations to calculate RSI.")
    )

    ggplot(data, aes(x = Date, y = RSI)) +
      geom_line(linewidth = 0.8, na.rm = TRUE) +
      geom_hline(
        yintercept = 70,
        linetype = "dashed"
      ) +
      geom_hline(
        yintercept = 30,
        linetype = "dashed"
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(
        title = paste0("Relative Strength Index (", input$rsi_period, ")"),
        x = NULL,
        y = "RSI"
      ) +
      theme_minimal(base_size = 11)
  })

  # MACD is displayed in a separate aligned panel so the MACD and
  # stock-price scales remain readable.
  output$macd_chart <- renderPlot({

    data <- analysis_data()

    validate(
      need(any(!is.na(data$MACD)), "Not enough observations to calculate MACD.")
    )

    ggplot(data, aes(x = Date)) +
      geom_col(
        aes(y = MACD_Histogram),
        alpha = 0.45,
        na.rm = TRUE
      ) +
      geom_line(
        aes(y = MACD, color = "MACD"),
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      geom_line(
        aes(y = MACD_Signal, color = "Signal"),
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      scale_color_manual(
        values = c("MACD" = "steelblue4", "Signal" = "firebrick")
      ) +
      labs(
        title = paste0(
          "MACD (",
          input$macd_fast,
          ", ",
          input$macd_slow,
          ", ",
          input$macd_signal,
          ")"
        ),
        x = NULL,
        y = "MACD",
        color = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom"
      )
  })

  # Display the most recent observations and all three possible
  # trading states (Buy, Sell, Hold).
  output$signal_table <- renderTable({

    data <- analysis_data()

    recent <- tail(
      data[, c(
        "Date",
        "Open",
        "High",
        "Low",
        "Close",
        "Volume",
        "Short_MA",
        "Long_MA",
        "RSI",
        "Signal"
      )],
      12
    )

    recent$Open <- round(recent$Open, 2)
    recent$High <- round(recent$High, 2)
    recent$Low <- round(recent$Low, 2)
    recent$Close <- round(recent$Close, 2)
    recent$Short_MA <- round(recent$Short_MA, 2)
    recent$Long_MA <- round(recent$Long_MA, 2)
    recent$RSI <- round(recent$RSI, 2)

    recent
  },
  striped = TRUE,
  bordered = TRUE,
  spacing = "s"
  )
}

# Launch the Shiny application.
shinyApp(ui = ui, server = server)
