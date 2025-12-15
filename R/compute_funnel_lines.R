#' Compute funnel lines (mean and ±SD bands) for proportions
#'
#' This internal helper computes the reference lines for a funnel plot assuming
#' `y` are proportions in \[0, 1] and `x` are non-negative counts (sample sizes).
#' The weighted mean \eqn{\hat{p} = \sum x y / \sum x} is used, and the standard
#' error at effective sample size \eqn{n} is \eqn{\sqrt{\hat{p} (1 - \hat{p}) / n}}.
#' Lines are generated across a sequence of effective sample sizes from 1 to
#' the maximum of the x scale limits or data range. The sequence is smoothed
#' via an exponential mapping controlled by `k`.
#'
#' @param data A data frame passed by ggplot2 `Stat` with columns `x` and `y`.
#' @param scales ggplot2 scales object; x limits are preferred over the data range.
#' @param n Integer, number of points per line (default: 100). Must be >= 2.
#' @param k Non-negative numeric, exponential smoothing parameter (default: 4).
#'
#' @returns A tibble with columns:
#' \describe{
#'   \item{x}{effective sample size at which the line is evaluated}
#'   \item{y}{value of the reference line at \code{x}}
#'   \item{line}{identifier for the line: \code{"mean"}, \code{"twoSD"}, \code{"minustwoSD"},
#'              \code{"threeSD"}, \code{"minusthreeSD"}}
#'   \item{linetype}{factor indicating display grouping: \code{"Mean"}, \code{"+-2SD"}, \code{"+-3SD"}}
#' }
#'
#' @section Assumptions:
#' \itemize{
#'   \item \code{y} are proportions in \[0, 1].
#'   \item \code{x} are non-negative counts (sample sizes).
#'   \item The normal approximation for the standard error is acceptable.
#' }
#'
#' @keywords internal
#' @noRd
compute_funnel_lines <- function(data, scales, n = 100, k = 4) {
  stopifnot(is.data.frame(data))
  if (!all(c("x", "y") %in% names(data))) {
    stop("compute_funnel_lines() expects columns 'x' and 'y'.", call. = FALSE)
  }
  if (!is.numeric(n) || length(n) != 1 || n < 2 || !is.finite(n)) {
    stop("'n' must be a finite integer >= 2.", call. = FALSE)
  }
  n <- as.integer(n)
  if (!is.numeric(k) || length(k) != 1 || !is.finite(k) || k < 0) {
    stop("'k' must be a finite, non-negative numeric.", call. = FALSE)
  }

  exp_seq <- function(x_max, n, k) {
    t <- seq(0, 1, length.out = n)
    denom <- exp(k) - 1
    if (abs(denom) < 1e-12) {
      return(1 + (x_max - 1) * t)
    }
    1 + (x_max - 1) * (exp(k * t) - 1) / denom
  }

  xlim <- tryCatch(scales$x$range$range, error = function(e) c(NA_real_, NA_real_))
  xlim <- suppressWarnings(as.numeric(xlim))
  x_max <- if (all(is.finite(xlim))) max(xlim) else max(data$x, na.rm = TRUE)

  if (!is.finite(x_max) || x_max <= 1) {
    return(dplyr::tibble(x = numeric(0), y = numeric(0), line = character(0), linetype = factor()))
  }

  w_sum <- sum(data$x, na.rm = TRUE)
  if (w_sum <= 0 || !is.finite(w_sum)) {
    return(dplyr::tibble(x = numeric(0), y = numeric(0), line = character(0), linetype = factor()))
  }
  phat <- sum(data$x * data$y, na.rm = TRUE) / w_sum

  steps <- exp_seq(x_max, n = n, k = k)

  # Mean line
  df_mean <- dplyr::tibble(
    x = steps,
    y = phat,
    line = "mean",
    linetype = "Mean"
  )

  # ±2SD and ±3SD bands
  se <- sqrt(phat * (1 - phat) / steps)
  df_bands <- dplyr::bind_rows(
    dplyr::tibble(x = steps, y = phat + 2 * se, line = "twoSD",          linetype = "+-2SD"),
    dplyr::tibble(x = steps, y = phat - 2 * se, line = "minustwoSD",     linetype = "+-2SD"),
    dplyr::tibble(x = steps, y = phat + 3 * se, line = "threeSD",        linetype = "+-3SD"),
    dplyr::tibble(x = steps, y = phat - 3 * se, line = "minusthreeSD",   linetype = "+-3SD")
  )

  dplyr::bind_rows(df_mean, df_bands) |>
    dplyr::filter(dplyr::between(y, 0, 1)) |>
    dplyr::mutate(
      linetype = forcats::fct_relevel(as.factor(linetype), "Mean", "+-2SD", "+-3SD")
    ) |>
    dplyr::select(x, y, line, linetype)
}
