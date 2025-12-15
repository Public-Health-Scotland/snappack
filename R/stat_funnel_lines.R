#' Draw funnel reference lines (mean and ±2SD/±3SD bands)
#'
#' This stat adds reference lines often used in funnel plots for proportions.
#' It assumes `y` are proportions in \[0, 1] and `x` are non-negative counts
#' (sample sizes). The weighted mean \eqn{\hat{p}} is computed as
#' \eqn{\sum x y / \sum x}, and the bands use the normal approximation with
#' standard error \eqn{\sqrt{\hat{p} (1 - \hat{p}) / n}} evaluated over an
#' exponential sequence of effective sample sizes from 1 to the maximum of the
#' x scale limits or the data range.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams compute_funnel_lines
#'
#' @param geom Which geom to use for drawing (default: `"line"`). Any line-like
#'   geom is acceptable (e.g., `"path"`), but `GeomLine` is the conventional choice.
#' @param position Position adjustment (default: `"identity"`).
#' @param n Integer, number of points per line (default: 100). Must be >= 2.
#' @param k Non-negative numeric, exponential smoothing parameter (default: 4).
#' @param na.rm Logical, drop missing values (default: `FALSE`).
#' @param show.legend Logical; should the layer be included in the legends?
#'   `NA` includes if any aesthetics are mapped. (default: `NA`)
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than
#'   combining with them. (default: `TRUE`)
#' @param ... Additional parameters passed to the geom.
#'
#' @return A ggplot2 layer that draws mean and ±2SD/±3SD reference lines.
#'
#' @examples
#' library(ggplot2)
#'
#' # Example data: n = sample size, p = observed proportion
#' set.seed(1)
#' df <- data.frame(
#'   n = sample(20:500, 200, replace = TRUE),
#'   p = pmin(pmax(rbeta(200, 20, 80), 0), 1)
#' )
#'
#' ggplot(df, aes(x = n, y = p)) +
#'   stat_funnel_lines() +
#'   geom_point(alpha = 0.5) +
#'   scale_linetype_manual(values = c("Mean" = "solid", "+-2SD" = "dashed", "+-3SD" = "dotted")) +
#'   scale_colour_manual(values = c("Mean" = "black", "+-2SD" = "grey50", "+-3SD" = "grey70")) +
#'   theme_minimal()
#'
#' @export
stat_funnel_lines <- function(mapping = NULL, data = NULL,
                              geom = "line", position = "identity",
                              ...,
                              n = 100, k = 4,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatFunnelLines,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(n = n, k = k, na.rm = na.rm, ...)
  )
}
