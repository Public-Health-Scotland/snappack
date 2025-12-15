#' Funnel plot scales (colour + linetype)
#'
#' Convenience wrapper that applies manual colour and linetype scales
#' for funnel reference lines produced by \code{stat_funnel_lines()} /
#' \code{geom_funnel_lines()}.
#'
#' @param labels Optional named character vector for legend labels; names must be
#'   \code{"Mean"}, \code{"+-2SD"}, \code{"+-3SD"}.
#'   Defaults to Scotland/limits wording.
#' @return A list of ggplot2 layers (two scales) to add with \code{+}.
#' @examples
#' # ggplot(df, aes(x = n, y = p)) +
#' #   stat_funnel_lines() +
#' #   scale_funnel_phs()
#' @export
scale_funnel_phs <- function(
    labels = c(
      "Mean"  = "Scotland",
      "+-2SD" = "2SD upper/lower control limit",
      "+-3SD" = "3SD upper/lower control limit"
    )
) {
  list(
    ggplot2::scale_colour_manual(
      values = c("Mean" = "#80BCEA", "+-2SD" = "#9B4393", "+-3SD" = "#3F3685"),
      breaks = c("Mean", "+-2SD", "+-3SD"),
      labels = labels
    ),
    ggplot2::scale_linetype_manual(
      values = c("Mean" = "solid", "+-2SD" = "dotted", "+-3SD" = "solid"),
      breaks = c("Mean", "+-2SD", "+-3SD"),
      labels = labels
    )
  )
}
