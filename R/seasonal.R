# R/seasonal.R

#' Internal helper: seasonal factor computation for GACE
#'
#' Computes multiplicative seasonal factors from a time series, dampens
#' extremes, and normalizes them so that the mean factor equals 1.
#'
#' @param y Numeric vector or time series.
#' @param freq_value Integer frequency of the time series.
#' @param alpha Damping parameter in (0, 1], controlling how strongly raw
#'   seasonal deviations from 1 are shrunk toward 1.
#'
#' @return Numeric vector of length \code{freq_value} containing seasonal
#'   multipliers.
#'
#' @keywords internal
#' @noRd
#' @importFrom stats ts cycle frequency
.gace_compute_seasonal_factors <- function(y,
                                           freq_value,
                                           alpha = 0.4) {
  
  if (is.null(freq_value) || freq_value <= 1L) {
    return(rep(1.0, 1L))
  }
  
  if (!stats::is.ts(y)) {
    y_ts <- stats::ts(y, frequency = freq_value)
  } else {
    y_ts <- y
  }
  
  cyc          <- stats::cycle(y_ts)
  seas_mean    <- tapply(as.numeric(y_ts), cyc, mean, na.rm = TRUE)
  overall_mean <- mean(y_ts, na.rm = TRUE)
  
  if (!is.finite(overall_mean) || overall_mean <= 0) {
    return(rep(1.0, freq_value))
  }
  
  raw <- seas_mean / overall_mean
  raw[!is.finite(raw)] <- 1.0
  
  damped <- 1 + alpha * (raw - 1)
  damped <- as.numeric(damped)
  
  damped <- damped / mean(damped, na.rm = TRUE)
  
  damped
}