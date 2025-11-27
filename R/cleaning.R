# R/cleaning.R

#' Internal helper: safe linear interpolation for missing values
#'
#' Linearly interpolates missing values in a numeric vector using
#' \code{stats::approx}. If all values are missing, returns the input
#' unchanged. If only one non-missing value exists, it is repeated.
#'
#' @param x Numeric vector.
#'
#' @return Numeric vector with missing values interpolated when possible.
#'
#' @keywords internal
#' @noRd
#' @importFrom stats approx
.gace_interpolate_na <- function(x) {
  if (all(is.na(x))) return(x)
  idx <- which(!is.na(x))
  if (length(idx) == 1L) {
    return(rep(x[idx], length(x)))
  }
  approx_res <- stats::approx(
    x    = idx,
    y    = x[idx],
    xout = seq_along(x),
    rule = 2
  )
  approx_res$y
}

#' Internal helper: winsorization of extremes
#'
#' Caps a numeric vector at lower and upper empirical quantiles to reduce
#' the influence of extreme outliers.
#'
#' @param x Numeric vector.
#' @param probs Length-2 numeric vector of probabilities in (0, 1).
#'
#' @return Winsorized numeric vector.
#'
#' @keywords internal
#' @noRd
#' @importFrom stats quantile
.gace_winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) return(x)
  q <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  x
}

#' Internal series cleaner for GACE
#'
#' Cleans a numeric time series by:
#' \itemize{
#'   \item Converting non-positive values to \code{NA} (optional),
#'   \item Interpolating missing values,
#'   \item Winsorizing extremes.
#' }
#'
#' @param y Numeric vector of historical values.
#' @param zero_to_na Logical; if TRUE, values \eqn{\le 0} are treated as missing.
#' @param winsorize Logical; if TRUE, extremes are winsorized.
#' @param winsor_probs Quantile bounds used for winsorization.
#'
#' @return Cleaned numeric vector.
#'
#' @keywords internal
#' @noRd
.gace_clean_series <- function(y,
                               zero_to_na  = TRUE,
                               winsorize   = TRUE,
                               winsor_probs = c(0.01, 0.99)) {
  y <- as.numeric(y)
  
  if (zero_to_na) {
    y[y <= 0] <- NA_real_
  }
  
  if (all(is.na(y))) return(y)
  
  if (anyNA(y)) {
    y <- .gace_interpolate_na(y)
  }
  
  if (winsorize) {
    y <- .gace_winsorize(y, probs = winsor_probs)
  }
  
  y
}