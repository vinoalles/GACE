#' Plot GACE Forecast
#'
#' Produces a plot of historical values and future forecasts returned by 
#' \link{gace_forecast}. The output is a ggplot2 object with a clear distinction
#' between historical and forecast periods.
#'
#' @param fc A data frame (or tibble) returned by \link{gace_forecast}, containing
#'   at least the columns:
#'   \itemize{
#'     \item \code{period} – numeric index for periods,
#'     \item \code{value} – observed or forecast values,
#'     \item \code{type} – "historical" or "forecast".
#'   }
#'
#' @return A ggplot2 object showing the GACE forecast.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' y  <- ts(rnorm(36, 100, 10), frequency = 12)
#' fc <- gace_forecast(y, periods = 12, freq = "month")
#' plot_gace(fc)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point
#' @importFrom ggplot2 scale_color_manual theme_minimal theme labs
#' @importFrom ggplot2 element_blank element_text
#'
#' @export
plot_gace <- function(fc) {
  
  required_cols <- c("period", "value", "type")
  if (!all(required_cols %in% names(fc))) {
    stop("`fc` must contain columns: period, value, type. ",
         "Use gace_forecast() to generate the input.")
  }
  
  ggplot2::ggplot(fc, ggplot2::aes(period, value, color = type)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_color_manual(
      values = c("historical" = "#2c7bb6", "forecast" = "#d7191c")
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title    = ggplot2::element_blank(),
      plot.title      = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.title      = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title    = "GACE Forecast",
      subtitle = "Historical vs Forecast",
      x        = "Period",
      y        = "Value"
    )
}