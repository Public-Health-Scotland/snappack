#' Convenience geom for funnel reference lines
#'
#' A thin wrapper that draws funnel reference lines using `GeomLine`. It is
#' equivalent to `stat_funnel_lines(geom = "line")`, but provided for users who
#' prefer geoms-first syntax.
#'
#' @inheritParams stat_funnel_lines
#'
#' @examples
#' library(ggplot2)
#'
#' set.seed(1)
#' df <- data.frame(
#'   n = sample(20:500, 200, replace = TRUE),
#'   p = pmin(pmax(rbeta(200, 20, 80), 0), 1)
#' )
#'
#' ggplot(df, aes(x = n, y = p)) +
#'   geom_funnel_lines() +
#'   geom_point(alpha = 0.5) +
#'   theme_minimal()
#'
#' @export
geom_funnel_lines <- function(mapping = NULL, data = NULL,
                              position = "identity",
                              ...,
                              n = 100, k = 4,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatFunnelLines,
    geom = ggplot2::GeomLine,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(n = n, k = k, na.rm = na.rm, ...)
  )
}
