# BDA400 - Data Science Tools and Techniques
# Assignment 6 - Technical Analysis using R, Visualization Phase
# Student: Nnedi Okafor
#
# Interactive R Shiny stock dashboard
# Data source: Yahoo Finance
#
# Features:
# - Historical stock data from Yahoo Finance
# - Daily, Weekly, and Monthly time frames
# - Line, Area, and Candlestick charts
# - Moving Average, RSI, and MACD indicators
# - Dynamic on/off indicator controls
# - Custom short/long moving-average periods
# - Buy/Sell/Hold trading rules
# - Buy/Sell annotations on the stock-price chart
# - Error handling for invalid symbols, dates, missing data, and insufficient history

# ------------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES
# ------------------------------------------------------------
library(shiny)
library(ggplot2)
library(quantmod)

# ------------------------------------------------------------
# 2. USER INTERFACE
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Interactive Stock Portfolio Dashboard"),

  sidebarLayout(
    sidebarPanel(
      textInput(
        "symbol",
        "Stock Symbol:",
        value = "AAPL"
      ),

      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = Sys.Date() - (5 * 365),
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
        "Select Chart Type:",
        choices = c("Line", "Area", "Candlestick"),
        selected = "Line"
      ),

      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c(
          "Moving Averages",
          "RSI",
          "MACD"
        ),
        selected = "Moving Averages"
      ),

      numericInput(
        "short_ma",
        "Short Moving Average Period:",
        value = 20,
        min = 2,
        max = 100,
        step = 1
      ),

      numericInput(
        "long_ma",
        "Long Moving Average Period:",
        value = 50,
        min = 3,
        max = 200,
        step = 1
      ),

      helpText(
        "Trading rule: Buy when the short moving average crosses above the long moving average. Sell when it crosses below. Otherwise, Hold."
      )
    ),

    mainPanel(
      h3(textOutput("stock_title")),

      plotOutput(
        "stock_chart",
        height = "500px"
      ),

      conditionalPanel(
        condition = "input.technical_indicators.indexOf('RSI') >= 0",
        plotOutput(
          "rsi_chart",
          height = "250px"
        )
      ),

      conditionalPanel(
        condition = "input.technical_indicators.indexOf('MACD') >= 0",
        plotOutput(
          "macd_chart",
          height = "280px"
        )
      ),

      h4("Latest Trading Signal"),
      verbatimTextOutput("latest_signal")
    )
  )
)

# ------------------------------------------------------------
# 3. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {

  # ----------------------------------------------------------
  # 3A. FETCH STOCK DATA FROM YAHOO FINANCE
  # ----------------------------------------------------------
  stock_raw <- reactive({
    req(input$symbol)
    req(input$date_range)

    validate(
      need(
        nzchar(trimws(input$symbol)),
        "Please enter a stock symbol, for example AAPL."
      ),
      need(
        input$date_range[1] < input$date_range[2],
        "The start date must be earlier than the end date."
      )
    )

    # Download a little extra history before the chosen start date.
    # This helps technical indicators calculate correctly near
    # the beginning of the selected range.
    fetch_start <- as.Date(input$date_range[1]) - 400
    fetch_end   <- as.Date(input$date_range[2]) + 1

    downloaded <- tryCatch(
      {
        suppressWarnings(
          getSymbols(
            Symbols = toupper(trimws(input$symbol)),
            src = "yahoo",
            from = fetch_start,
            to = fetch_end,
            auto.assign = FALSE
          )
        )
      },
      error = function(e) {
        NULL
      }
    )

    validate(
      need(
        !is.null(downloaded),
        "Unable to download stock data. Check the stock symbol, selected dates, and internet connection."
      )
    )

    # IMPORTANT:
    # Yahoo Finance can occasionally return incomplete rows.
    # Remove them before RSI/MACD/MA calculations.
    downloaded <- na.omit(downloaded)

    validate(
      need(
        NROW(downloaded) > 1,
        "No usable stock data was returned for this symbol and date range."
      )
    )

    downloaded
  })

  # ----------------------------------------------------------
  # 3B. CONVERT TO DAILY / WEEKLY / MONTHLY DATA
  # ----------------------------------------------------------
  stock_data <- reactive({
    x <- stock_raw()

    if (input$time_frame == "Weekly") {
      x <- to.weekly(
        x,
        indexAt = "lastof",
        drop.time = TRUE
      )
    } else if (input$time_frame == "Monthly") {
      x <- to.monthly(
        x,
        indexAt = "lastof",
        drop.time = TRUE
      )
    }

    # Remove any incomplete rows after time-frame conversion.
    x <- na.omit(x)

    validate(
      need(
        NROW(x) > 1,
        "Not enough observations are available after applying the selected time frame."
      )
    )

    x
  })

  # ----------------------------------------------------------
  # 3C. CALCULATE INDICATORS AND TRADING SIGNALS
  # ----------------------------------------------------------
  analysis_data_all <- reactive({
    x <- stock_data()

    validate(
      need(
        input$short_ma < input$long_ma,
        "The short moving-average period must be smaller than the long moving-average period."
      )
    )

    required_rows <- max(
      as.integer(input$long_ma) + 2,
      40
    )

    validate(
      need(
        NROW(x) >= required_rows,
        paste0(
          "Not enough historical observations for these settings. ",
          "Choose a longer date range, a shorter time frame, or a smaller long moving-average period."
        )
      )
    )

    close_prices <- as.numeric(Cl(x))
    open_prices  <- as.numeric(Op(x))
    high_prices  <- as.numeric(Hi(x))
    low_prices   <- as.numeric(Lo(x))

    # Extra safety check for missing/non-finite closing prices.
    good_rows <- is.finite(close_prices) &
      is.finite(open_prices) &
      is.finite(high_prices) &
      is.finite(low_prices)

    x <- x[good_rows, ]

    close_prices <- as.numeric(Cl(x))
    open_prices  <- as.numeric(Op(x))
    high_prices  <- as.numeric(Hi(x))
    low_prices   <- as.numeric(Lo(x))

    validate(
      need(
        length(close_prices) >= required_rows,
        "Not enough complete price observations are available to calculate the selected indicators."
      )
    )

    short_ma <- SMA(
      close_prices,
      n = as.integer(input$short_ma)
    )

    long_ma <- SMA(
      close_prices,
      n = as.integer(input$long_ma)
    )

    rsi_values <- RSI(
      close_prices,
      n = 14
    )

    macd_values <- MACD(
      close_prices,
      nFast = 12,
      nSlow = 26,
      nSig = 9,
      maType = "EMA"
    )

    # Relationship between short and long moving averages.
    relation <- ifelse(
      is.na(short_ma) | is.na(long_ma),
      NA_integer_,
      ifelse(short_ma > long_ma, 1L, -1L)
    )

    previous_relation <- c(
      NA_integer_,
      head(relation, -1)
    )

    # Default signal is Hold.
    signal <- rep(
      "Hold",
      length(close_prices)
    )

    # Buy when short MA crosses from below to above long MA.
    buy_index <- !is.na(relation) &
      !is.na(previous_relation) &
      relation == 1L &
      previous_relation == -1L

    # Sell when short MA crosses from above to below long MA.
    sell_index <- !is.na(relation) &
      !is.na(previous_relation) &
      relation == -1L &
      previous_relation == 1L

    signal[buy_index] <- "Buy"
    signal[sell_index] <- "Sell"

    data.frame(
      Date = as.Date(index(x)),
      Open = open_prices,
      High = high_prices,
      Low = low_prices,
      Close = close_prices,
      ShortMA = as.numeric(short_ma),
      LongMA = as.numeric(long_ma),
      RSI = as.numeric(rsi_values),
      MACD = as.numeric(macd_values[, 1]),
      MACDSignal = as.numeric(macd_values[, 2]),
      Signal = signal,
      stringsAsFactors = FALSE
    )
  })

  # ----------------------------------------------------------
  # 3D. FILTER TO THE DATE RANGE SELECTED BY THE USER
  # ----------------------------------------------------------
  analysis_data <- reactive({
    d <- analysis_data_all()

    start_date <- as.Date(input$date_range[1])
    end_date   <- as.Date(input$date_range[2])

    d <- d[
      d$Date >= start_date &
        d$Date <= end_date,
    ]

    validate(
      need(
        nrow(d) > 0,
        "No observations are available inside the selected date range."
      )
    )

    d
  })

  # ----------------------------------------------------------
  # 4. DASHBOARD TITLE
  # ----------------------------------------------------------
  output$stock_title <- renderText({
    paste(
      toupper(trimws(input$symbol)),
      "-",
      input$time_frame,
      "Stock Analysis"
    )
  })

  # ----------------------------------------------------------
  # 5. MAIN STOCK PRICE CHART
  # ----------------------------------------------------------
  output$stock_chart <- renderPlot({
    d <- analysis_data()

    validate(
      need(
        nrow(d) > 0,
        "No stock data is available to display."
      )
    )

    if (input$chart_type == "Area") {

      p <- ggplot(
        d,
        aes(x = Date, y = Close)
      ) +
        geom_area(
          alpha = 0.30
        ) +
        geom_line(
          linewidth = 0.8
        )

    } else if (input$chart_type == "Candlestick") {

      d$Up <- d$Close >= d$Open

      candle_width <- switch(
        input$time_frame,
        "Daily" = 0.35,
        "Weekly" = 2.5,
        "Monthly" = 10,
        0.35
      )

      p <- ggplot(
        d,
        aes(x = Date)
      ) +
        geom_segment(
          aes(
            xend = Date,
            y = Low,
            yend = High
          ),
          linewidth = 0.4
        ) +
        geom_rect(
          aes(
            xmin = Date - candle_width,
            xmax = Date + candle_width,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Up
          ),
          alpha = 0.75
        ) +
        scale_fill_manual(
          values = c(
            "TRUE" = "darkgreen",
            "FALSE" = "firebrick"
          ),
          guide = "none"
        )

    } else {

      p <- ggplot(
        d,
        aes(x = Date, y = Close)
      ) +
        geom_line(
          linewidth = 0.8
        )
    }

    # Add moving averages only when selected.
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(
          aes(
            y = ShortMA,
            linetype = paste0("SMA ", input$short_ma)
          ),
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        geom_line(
          aes(
            y = LongMA,
            linetype = paste0("SMA ", input$long_ma)
          ),
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        labs(
          linetype = "Moving Averages"
        )
    }

    # Add Buy/Sell annotations.
    signal_data <- d[
      d$Signal %in% c("Buy", "Sell"),
    ]

    if (nrow(signal_data) > 0) {
      p <- p +
        geom_point(
          data = signal_data,
          aes(
            x = Date,
            y = Close,
            shape = Signal
          ),
          size = 3,
          inherit.aes = FALSE
        ) +
        geom_text(
          data = signal_data,
          aes(
            x = Date,
            y = Close,
            label = Signal
          ),
          vjust = -1,
          size = 3.5,
          check_overlap = TRUE,
          inherit.aes = FALSE
        )
    }

    p +
      labs(
        title = paste(
          toupper(trimws(input$symbol)),
          "Stock Price and Trading Signals"
        ),
        x = "Date",
        y = "Price"
      ) +
      theme_minimal(
        base_size = 13
      ) +
      theme(
        plot.title = element_text(
          face = "bold"
        ),
        legend.position = "bottom"
      )
  })

  # ----------------------------------------------------------
  # 6. RSI CHART
  # ----------------------------------------------------------
  output$rsi_chart <- renderPlot({
    req(
      "RSI" %in% input$technical_indicators
    )

    d <- analysis_data()

    ggplot(
      d,
      aes(x = Date, y = RSI)
    ) +
      geom_line(
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      geom_hline(
        yintercept = 70,
        linetype = "dashed"
      ) +
      geom_hline(
        yintercept = 30,
        linetype = "dashed"
      ) +
      annotate(
        "text",
        x = min(d$Date),
        y = 70,
        label = "Overbought 70",
        hjust = 0,
        vjust = -0.5,
        size = 3
      ) +
      annotate(
        "text",
        x = min(d$Date),
        y = 30,
        label = "Oversold 30",
        hjust = 0,
        vjust = 1.5,
        size = 3
      ) +
      coord_cartesian(
        ylim = c(0, 100)
      ) +
      labs(
        title = "Relative Strength Index (RSI)",
        x = "Date",
        y = "RSI"
      ) +
      theme_minimal(
        base_size = 12
      )
  })

  # ----------------------------------------------------------
  # 7. MACD CHART
  # ----------------------------------------------------------
  output$macd_chart <- renderPlot({
    req(
      "MACD" %in% input$technical_indicators
    )

    d <- analysis_data()

    ggplot(
      d,
      aes(x = Date)
    ) +
      geom_line(
        aes(
          y = MACD,
          linetype = "MACD"
        ),
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      geom_line(
        aes(
          y = MACDSignal,
          linetype = "Signal"
        ),
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      geom_hline(
        yintercept = 0,
        linetype = "dotted"
      ) +
      labs(
        title = "Moving Average Convergence Divergence (MACD)",
        x = "Date",
        y = "MACD",
        linetype = "Series"
      ) +
      theme_minimal(
        base_size = 12
      ) +
      theme(
        legend.position = "bottom"
      )
  })

  # ----------------------------------------------------------
  # 8. LATEST TRADING SIGNAL
  # ----------------------------------------------------------
  output$latest_signal <- renderText({
    d <- analysis_data()

    last_row <- d[
      nrow(d),
    ]

    paste0(
      "Date: ",
      last_row$Date,
      "\nClosing Price: $",
      format(
        round(last_row$Close, 2),
        nsmall = 2
      ),
      "\nSignal: ",
      last_row$Signal,
      "\nRule: SMA ",
      input$short_ma,
      " compared with SMA ",
      input$long_ma
    )
  })
}

# ------------------------------------------------------------
# 9. RUN THE SHINY APPLICATION
# ------------------------------------------------------------
shinyApp(
  ui = ui,
  server = server
)
