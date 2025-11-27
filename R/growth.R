# R/growth.R

#' Internal helper: map frequency to YoY lag
#'
#' Determines the appropriate lag (in periods) for year-over-year style
#' growth based on either an explicit frequency name or a time series
#' frequency value.
#'
#' @param freq_name Optional string, e.g., "month", "week", "quarter", "year".
#' @param freq_value Optional integer time series frequency.
#'
#' @return Integer lag used for YoY growth.
#'
#' @keywords internal
#' @noRd
.gace_freq_to_yoy_lag <- function(freq_name, freq_value = NULL) {
  if (!is.null(freq_value) && freq_value > 1L) {
    if (freq_value == 12L) return(12L)
    if (freq_value == 4L)  return(4L)
    if (freq_value >= 50L && freq_value <= 53L) return(52L)
  }
  
  freq_name <- tolower(freq_name %||% "")
  if (freq_name %in% c("month", "monthly", "m"))   return(12L)
  if (freq_name %in% c("week", "weekly", "w"))     return(52L)
  if (freq_name %in% c("quarter", "qtr", "q"))     return(4L)
  if (freq_name %in% c("year", "yearly", "annual")) return(1L)
  
  12L
}

#' Internal helper: compute hybrid growth signals for GACE
#'
#' Combines multiple growth components:
#' \itemize{
#'   \item Year-over-year growth (long horizon),
#'   \item Short-term growth (local change),
#'   \item Rolling-window growth (smoothed),
#'   \item Linear-regression drift (long-run trend).
#' }
#' Each component is trimmed to avoid extreme spikes and then averaged.
#'
#' @param y Numeric vector of cleaned historical values.
#' @param freq_name Optional frequency name (e.g., "month").
#' @param freq_value Optional \code{ts} frequency.
#' @param min_points Minimum length required to compute meaningful signals.
#'
#' @return Numeric vector of raw growth signals.
#'
#' @keywords internal
#' @noRd
#' @importFrom stats lm
.gace_compute_growth_signals <- function(y,
                                         freq_name  = NULL,
                                         freq_value = NULL,
                                         min_points = 10L) {
  n <- length(y)
  if (n < min_points) {
    return(rep(0, n))
  }
  
  yoy_lag <- .gace_freq_to_yoy_lag(freq_name, freq_value)
  
  # YoY growth
  yoy <- rep(NA_real_, n)
  if (yoy_lag < n) {
    for (t in (yoy_lag + 1L):n) {
      denom <- y[t - yoy_lag]
      if (!is.na(denom) && denom != 0) {
        yoy[t] <- (y[t] - denom) / denom
      }
    }
  }
  
  # Short-term growth
  st <- rep(NA_real_, n)
  if (n >= 2L) {
    for (t in 2L:n) {
      denom <- mean(y[max(1L, t - 2L):(t - 1L)], na.rm = TRUE)
      if (!is.na(denom) && denom != 0) {
        st[t] <- (y[t] - y[t - 1L]) / denom
      }
    }
  }
  
  # Rolling growth
  roll <- rep(NA_real_, n)
  window_long  <- min(6L, floor(n / 3L))
  window_short <- max(2L, floor(window_long / 2L))
  
  if (window_long >= 3L) {
    for (t in (window_long + 1L):n) {
      prev_idx  <- (t - window_long):(t - window_short)
      curr_idx  <- (t - window_short + 1L):t
      prev_mean <- mean(y[prev_idx], na.rm = TRUE)
      curr_mean <- mean(y[curr_idx], na.rm = TRUE)
      
      if (!is.na(prev_mean) && prev_mean != 0) {
        roll[t] <- (curr_mean - prev_mean) / prev_mean
      }
    }
  }
  
  # Drift via regression
  t_seq  <- seq_len(n)
  lm_fit <- try(stats::lm(y ~ t_seq), silent = TRUE)
  drift  <- rep(0, n)
  if (!inherits(lm_fit, "try-error")) {
    slope <- unname(coef(lm_fit)[2L])
    level <- max(mean(y, na.rm = TRUE), .Machine$double.eps)
    drift <- rep(slope / level, n)
  }
  
  mat <- cbind(yoy, st, roll, drift)
  
  trim_col <- function(col, limit = 2) {
    idx <- which(abs(col) > limit & !is.na(col))
    if (length(idx) > 0L) {
      col[idx] <- sign(col[idx]) * limit
    }
    col
  }
  
  for (j in seq_len(ncol(mat))) {
    mat[, j] <- trim_col(mat[, j])
  }
  
  growth_raw <- rowMeans(mat, na.rm = TRUE)
  growth_raw[!is.finite(growth_raw)] <- 0
  
  growth_raw
}