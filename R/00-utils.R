# R/utils.R

#' Internal null-coalescing operator
#'
#' Returns `a` if it is not NULL; otherwise returns `b`.
#'
#' Used internally to simplify optional argument handling.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}