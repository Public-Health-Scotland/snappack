#' StatFunnelLines ggproto
#'
#' A ggplot2 `Stat` that computes funnel reference lines (mean and ±SD bands)
#' based on weighted mean proportion and normal-approximation standard errors.
#'
#' @section Computation:
#' The stat expects aesthetics `x` (counts / sample size) and `y` (proportion).
#' It returns a tidy data frame with columns `x`, `y`, `line`, `linetype`. The
#' default aesthetics group and style the output by the computed `line` and
#' `linetype` using `after_stat()`.
#'
#' @section Aesthetics:
#' \itemize{
#'   \item \code{x} (required): counts / sample sizes
#'   \item \code{y} (required): proportions in \[0, 1]
#'   \item \code{colour}, \code{linetype}, \code{group}: set automatically via `after_stat()`
#' }
#'
#' @examples
#' # Not to be used directly; prefer stat_funnel_lines()
#' # Use: ggplot(df, aes(x = n, y = p)) + stat_funnel_lines()
#'
#' @keywords internal
#' @noRd
StatFunnelLines <- ggplot2::ggproto(
  `_class`   = "StatFunnelLines",
  `_inherit` = ggplot2::Stat,
  required_aes = c("x", "y"),
  default_aes  = ggplot2::aes(
    group    = ggplot2::after_stat(line),
    linetype = ggplot2::after_stat(linetype),
    colour   = ggplot2::after_stat(linetype)
  ),
  compute_panel = function(data, scales, n = 100, k = 4) {
    compute_funnel_lines(data, scales, n = n, k = k)
  }
)
