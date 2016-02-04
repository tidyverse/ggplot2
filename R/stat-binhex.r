#' @export
#' @rdname geom_hex
#' @inheritParams stat_bin_2d
stat_bin_hex <- function(mapping = NULL, data = NULL,
                         geom = "hex", position = "identity",
                         ...,
                         bins = 30,
                         binwidth = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBinhex,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname geom_hex
#' @usage NULL
stat_binhex <- stat_bin_hex

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBinhex <- ggproto("StatBinhex", Stat,
  default_aes = aes(fill = ..value..),

  required_aes = c("x", "y"),

  compute_group = function(data, scales, binwidth = NULL, bins = 30,
                           na.rm = FALSE) {
    try_require("hexbin", "stat_binhex")

    binwidth <- binwidth %||% hex_binwidth(bins, scales)
    wt <- data$weight %||% rep(1L, nrow(data))
    hexBinSummarise(data$x, data$y, wt, binwidth, sum)
  }
)

