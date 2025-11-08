#' Plot PACE Forecast
#'
#' @description
#' Visualizes PACE forecasts against actual historical data.
#' Displays Actual values (blue) vs Forecast values (red).
#'
#' @param forecast_df A data frame returned by pace_forecast or pace_forecast_seasonal
#'                    containing columns Time and Forecast.
#' @param actual_df (optional) A data frame with historical Time and Value columns.
#' @return A ggplot object
#'
#' @examples
#' df <- data.frame(Time = seq(as.Date("2020-01-01"), by="quarter", length.out=12),
#'                  Value = rnorm(12, 100, 10))
#' result <- pace_forecast(df, periods = 4)
#' plot_pace(result, df)
#'
#' @export
plot_pace <- function(forecast_df, actual_df = NULL) {
  library(ggplot2)

  # prepare forecast data for plotting
  forecast_df$Type <- "Forecast"
  names(forecast_df)[names(forecast_df) == "Forecast"] <- "Value"

  if (!is.null(actual_df)) {
    actual_df$Type <- "Actual"
    plot_df <- rbind(
      actual_df[, c("Time", "Value", "Type")],
      forecast_df[, c("Time", "Value", "Type")]
    )
  } else {
    plot_df <- forecast_df
  }

  ggplot(plot_df, aes(x = Time, y = Value, color = Type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
    labs(
      title = "PACE Forecast",
      x = "Time",
      y = "Value"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
