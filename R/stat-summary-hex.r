#' @export
#' @rdname stat_summary_2d
#' @inheritParams stat_bin_hex
stat_summary_hex <- function(mapping = NULL, data = NULL,
                             geom = "hex", position = "identity",
                             ...,
                             bins = 30,
                             binwidth = NULL,
                             drop = TRUE,
                             fun = "mean",
                             fun.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSummaryHex,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      drop = drop,
      fun = fun,
      fun.args = fun.args,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSummaryHex <- ggproto("StatSummaryHex", Stat,
  default_aes = aes(fill = ..value..),

  required_aes = c("x", "y", "z"),

  compute_group = function(data, scales, binwidth = NULL, bins = 30, drop = TRUE,
                           fun = "mean", fun.args = list()) {
    try_require("hexbin", "stat_summary_hex")

    binwidth <- binwidth %||% hex_binwidth(bins, scales)
    hexBinSummarise(data$x, data$y, data$z, binwidth,
      fun = fun, fun.args = fun.args, drop = drop)
  }
)
