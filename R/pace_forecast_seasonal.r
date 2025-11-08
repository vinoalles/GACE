#' PACE Forecast with Seasonality
#'
#' @description
#' PACE (Predictive Adjusted Capped Estimator) with seasonal adjustment.
#' Combines Year-over-Year, Quarter-over-Quarter, and Prior-Quarter growth rates,
#' applies volatility caps, and multiplies by quarterly seasonality factors
#' normalized to mean = 1.
#'
#' @param df A data frame with two columns: Time (Date) and Value (numeric).
#' @param periods Number of future periods to forecast (default = 12).
#'
#' @return A data frame containing future Time and Forecast values.
#'
#' @examples
#' df <- data.frame(Time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 12),
#'                  Value = rnorm(12, 100, 10))
#' pace_forecast_seasonal(df, periods = 4)
#'
#' @export
pace_forecast_seasonal <- function(df, periods = 12) {
  library(dplyr)
  library(lubridate)

  # ensure proper format
  df$Time <- as.Date(df$Time)
  df <- df %>% arrange(Time)
  df$year  <- year(df$Time)
  df$quart <- quarter(df$Time)

  # yearly growth
  ydf <- df %>%
    group_by(year) %>%
    summarise(Ysum = sum(Value), .groups = "drop") %>%
    mutate(Yearyoy = (Ysum / lag(Ysum, 1)) - 1)

  # quarterly growth
  qdf <- df %>%
    group_by(year, quart) %>%
    summarise(Qsum = sum(Value), .groups = "drop") %>%
    mutate(
      QoQyoy = (Qsum / lag(Qsum, 4)) - 1,  # same quarter last year
      PQoQ   = (Qsum / lag(Qsum, 1)) - 1   # prior quarter
    )

  # seasonal indices normalized to mean = 1
  season_idx <- df %>%
    group_by(quart) %>%
    summarise(season_mean = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      season_factor = season_mean / mean(season_mean)
    )

  forecasts <- data.frame()
  last_date <- max(df$Time)

  cap <- function(x) min(max(x, -0.25), 0.50)

  for (i in seq_len(periods)) {
    future_date <- last_date + months(3) * i

    yoy  <- tail(ydf$Yearyoy, 1)
    qyoy <- mean(tail(qdf$QoQyoy, 4), na.rm = TRUE)
    pq   <- tail(qdf$PQoQ, 1)

    yoy  <- cap(yoy)
    qyoy <- cap(qyoy)
    pq   <- cap(pq)

    last_val <- tail(df$Value, 1)
    growth <- mean(c(yoy, qyoy, pq), na.rm = TRUE)
    base_val <- last_val * (1 + growth)

    # seasonality
    q <- quarter(future_date)
    season_adj <- season_idx$season_factor[season_idx$quart == q]
    forecast_val <- base_val * season_adj

    forecast_val <- max(0, forecast_val)

    forecasts <- rbind(
      forecasts,
      data.frame(Time = future_date, Forecast = forecast_val)
    )

    df <- rbind(
      df,
      data.frame(Time = future_date, Value = forecast_val,
                 year = year(future_date), quart = quarter(future_date))
    )
  }

  forecasts
}
