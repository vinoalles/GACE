#' Internal GACE 1.0 engine (core logic)
#'
#' This function implements the core GACE 1.0 Enhanced algorithm:
#' cleaning, growth signal construction, volatility-aware capping,
#' seasonal factor computation, and recursive forecasting. 
#' It is designed for internal use; end users should call 
#' \link{gace_forecast}.
#'
#' @param y Numeric vector or time series of historical values.
#' @param h Integer forecast horizon (number of periods).
#' @param freq_name Optional frequency name ("week", "month", "quarter", "year").
#' @param seasonal Logical; whether to apply seasonal scaling.
#' @param caps Length-2 numeric vector of baseline lower and upper growth caps.
#' @param alpha_season Seasonal damping parameter.
#' @param gamma Decay parameter for future growth.
#' @param beta Blend between growth-driven and level-driven forecast.
#' @param verbose Logical; if TRUE, prints diagnostic messages.
#'
#' @return A list with elements:
#'   \itemize{
#'     \item mean   – numeric or \code{ts} forecast vector,
#'     \item x      – original input series,
#'     \item fitted – currently \code{NULL},
#'     \item method – character string describing the method,
#'     \item par    – list of key parameters.
#'   }
#'
#' @keywords internal
#' @noRd
#' @importFrom stats coef
#' @importFrom stats ts cycle frequency tsp
#' @importFrom utils tail
.gace_engine <- function(y,
                         h,
                         freq_name    = NULL,
                         seasonal     = TRUE,
                         caps         = c(-0.3, 0.3),
                         alpha_season = 0.4,
                         gamma        = 0.9,
                         beta         = 0.8,
                         verbose      = FALSE) {
  
  if (!is.numeric(h) || length(h) != 1L || h <= 0) {
    stop("`h` must be a positive scalar integer.")
  }
  h <- as.integer(h)
  
  if (!is.numeric(caps) || length(caps) != 2L) {
    stop("`caps` must be a numeric vector of length 2: c(lower, upper).")
  }
  
  freq_value   <- NULL
  season_index <- 1L
  
  if (stats::is.ts(y)) {
    freq_value <- stats::frequency(y)
    season_index <- if (freq_value > 1L) {
      stats::cycle(y)[length(y)]
    } else {
      1L
    }
  }
  
  y_clean <- .gace_clean_series(y)
  if (verbose) {
    message("GACE: cleaned series with length = ", length(y_clean))
  }
  
  if (all(is.na(y_clean)) || length(y_clean) < 3L) {
    if (verbose) message("GACE: insufficient data, falling back to flat forecast.")
    last_val <- tail(as.numeric(y), 1L)
    if (!is.finite(last_val)) last_val <- 0
    fc <- rep(last_val, h)
    return(list(
      mean   = fc,
      method = "GACE 1.0 (flat fallback)",
      x      = y,
      fitted = NULL,
      par    = list(
        caps         = caps,
        alpha_season = alpha_season,
        gamma        = gamma,
        beta         = beta
      )
    ))
  }
  
  growth_raw <- .gace_compute_growth_signals(
    y          = y_clean,
    freq_name  = freq_name,
    freq_value = freq_value
  )
  
  growth_cap <- .gace_apply_volatility_caps(
    growth_raw = growth_raw,
    base_low   = caps[1],
    base_high  = caps[2],
    freq_name  = freq_name,
    freq_value = freq_value
  )
  
  if (seasonal && (!is.null(freq_value) && freq_value > 1L)) {
    seas <- .gace_compute_seasonal_factors(
      y          = y_clean,
      freq_value = freq_value,
      alpha      = alpha_season
    )
  } else {
    seas         <- 1.0
    season_index <- 1L
  }
  
  y_last <- tail(y_clean, 1L)
  
  fc <- .gace_recursive_forecast(
    y_last          = y_last,
    growth_hist     = tail(growth_cap, max(6L, length(growth_cap))),
    seasonal_factors = seas,
    h               = h,
    gamma           = gamma,
    beta            = beta,
    start_season_index = season_index + 1L
  )
  
  fc[fc < 0] <- 0
  
  if (stats::is.ts(y) && !is.null(freq_value) && freq_value > 0) {
    fc_ts <- stats::ts(
      fc,
      start     = stats::tsp(y)[2] + 1 / freq_value,
      frequency = freq_value
    )
  } else {
    fc_ts <- fc
  }
  
  list(
    mean   = fc_ts,
    x      = y,
    fitted = NULL,
    method = "GACE 1.0 Enhanced (growth+caps+seasonal)",
    par    = list(
      caps         = caps,
      alpha_season = alpha_season,
      gamma        = gamma,
      beta         = beta
    )
  )
}

#' GACE Forecasting Engine (Generalized Adaptive Capped Estimator)
#'
#' A deterministic forecasting method that combines hybrid growth signals,
#' volatility-aware asymmetric caps, and optional seasonal scaling. Supports
#' weekly, monthly, quarterly, and yearly time series.
#'
#' This is the main user-facing function. It wraps the internal engine and
#' returns a data frame suitable for plotting and downstream analysis.
#'
#' @param df Numeric vector or time series of historical values.
#' @param periods Integer; number of future periods to forecast.
#' @param freq One of \code{"week"}, \code{"month"}, \code{"quarter"},
#'   or \code{"year"}. Used when \code{df} is not a \code{ts} object.
#' @param seasonal Logical; whether to apply seasonal scaling.
#' @param cap_low Numeric; baseline lower growth cap.
#' @param cap_high Numeric; baseline upper growth cap.
#' @param verbose Logical; if TRUE, prints diagnostic messages.
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{period} – integer index of historical and forecast periods,
#'     \item \code{value} – observed or forecast values,
#'     \item \code{type} – "historical" or "forecast".
#'   }
#' The returned object has S3 class \code{"gace_forecast"} and includes
#' engine details in the \code{"gace_details"} attribute.
#'
#' @examples
#' set.seed(1)
#' y <- ts(rnorm(60, mean = 100, sd = 10), frequency = 12)
#' fc <- gace_forecast(y, periods = 12, freq = "month")
#' head(fc)
#'
#' @export
gace_forecast <- function(df,
                          periods  = 12,
                          freq     = c("week", "month", "quarter", "year"),
                          seasonal = TRUE,
                          cap_low  = -0.3,
                          cap_high =  0.3,
                          verbose  = FALSE) {
  
  if (!is.numeric(periods) || length(periods) != 1L || periods <= 0) {
    stop("`periods` must be a positive scalar integer.")
  }
  periods <- as.integer(periods)
  
  freq <- match.arg(freq)
  
  engine_res <- .gace_engine(
    y           = df,
    h           = periods,
    freq_name   = freq,
    seasonal    = seasonal,
    caps        = c(cap_low, cap_high),
    alpha_season = 0.4,
    gamma       = 0.9,
    beta        = 0.8,
    verbose     = verbose
  )
  
  y_vec <- as.numeric(df)
  n_hist <- length(y_vec)
  
  hist_df <- data.frame(
    period = seq_len(n_hist),
    value  = y_vec,
    type   = "historical",
    stringsAsFactors = FALSE
  )
  
  fc_vec <- as.numeric(engine_res$mean)
  fc_df <- data.frame(
    period = n_hist + seq_len(periods),
    value  = fc_vec,
    type   = "forecast",
    stringsAsFactors = FALSE
  )
  
  out <- rbind(hist_df, fc_df)
  class(out) <- c("gace_forecast", class(out))
  attr(out, "gace_details") <- engine_res
  
  out
}