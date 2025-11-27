# R/recursive.R

#' Internal helper: recursive GACE forecast generator
#'
#' Produces a recursive multi-step forecast given the last observed value,
#' a history of capped growth rates, and seasonal factors. Growth is
#' gradually shrunk toward zero as the horizon increases.
#'
#' @param y_last Last observed (cleaned) value.
#' @param growth_hist Numeric vector of capped growth signals.
#' @param seasonal_factors Numeric vector of seasonal multipliers.
#' @param h Forecast horizon (number of periods).
#' @param gamma Geometric decay factor for growth.
#' @param beta Blending factor between pure growth-based forecast and
#'   level-based persistence.
#' @param start_season_index Integer index of the next seasonal position.
#'
#' @return Numeric vector of length \code{h} containing future forecasts.
#'
#' @keywords internal
#' @noRd
.gace_recursive_forecast <- function(y_last,
                                     growth_hist,
                                     seasonal_factors,
                                     h,
                                     gamma = 0.9,
                                     beta  = 0.8,
                                     start_season_index = 1L) {
  
  if (!is.finite(y_last) || y_last <= 0) {
    y_last <- max(1e-8, abs(y_last))
  }
  
  s <- length(seasonal_factors)
  if (s == 0L) {
    seasonal_factors <- 1.0
    s <- 1L
  }
  
  g_base <- stats::median(growth_hist[is.finite(growth_hist)], na.rm = TRUE)
  if (!is.finite(g_base)) g_base <- 0
  
  fc         <- numeric(h)
  last_val   <- y_last
  season_pos <- start_season_index
  
  for (i in seq_len(h)) {
    g_t <- g_base * (gamma^(i - 1L))
    s_t <- seasonal_factors[((season_pos - 1L) %% s) + 1L]
    
    raw_next   <- last_val * (1 + g_t) * s_t
    final_next <- beta * raw_next + (1 - beta) * last_val
    
    fc[i]      <- final_next
    last_val   <- final_next
    season_pos <- season_pos + 1L
  }
  
  fc
}