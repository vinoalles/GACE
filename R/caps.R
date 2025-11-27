# R/caps.R

#' Internal helper: volatility-aware asymmetric caps
#'
#' Applies volatility-aware asymmetric caps to a vector of raw growth signals.
#' The cap bounds are adjusted using the empirical standard deviation of the
#' growth series and a frequency-specific scaling factor.
#'
#' @param growth_raw Numeric vector of raw growth signals.
#' @param base_low Baseline lower cap.
#' @param base_high Baseline upper cap.
#' @param freq_name Optional frequency name.
#' @param freq_value Optional time series frequency.
#'
#' @return Numeric vector of capped growth signals.
#'
#' @keywords internal
#' @noRd
#' @importFrom stats sd
.gace_apply_volatility_caps <- function(growth_raw,
                                        base_low   = -0.3,
                                        base_high  =  0.3,
                                        freq_name  = NULL,
                                        freq_value = NULL) {
  
  n <- length(growth_raw)
  if (n == 0L) return(growth_raw)
  
  k <- 1.5
  freq_name <- tolower(freq_name %||% "")
  if (freq_name %in% c("week", "weekly", "w"))      k <- 1.0
  if (freq_name %in% c("quarter", "qtr", "q"))      k <- 2.0
  if (freq_name %in% c("year", "yearly", "annual")) k <- 3.0
  
  if (!is.null(freq_value)) {
    if (freq_value == 12L) k <- 1.5
    if (freq_value >= 50L && freq_value <= 53L) k <- 1.0
    if (freq_value == 4L)  k <- 2.0
  }
  
  vol <- stats::sd(growth_raw, na.rm = TRUE)
  if (!is.finite(vol) || vol <= 0) {
    low  <- base_low
    high <- base_high
  } else {
    low  <- max(base_low,  -k * vol)
    high <- min(base_high,  k * vol)
  }
  
  pmin(pmax(growth_raw, low), high)
}