#' @export
#' @rdname geom_hex
#' @inheritParams stat_bin_2d
#' @eval rd_computed_vars(
#'   count    = "number of points in bin.",
#'   density  = "density of points in bin, scaled to integrate to 1.",
#'   ncount   = "count, scaled to maximum of 1.",
#'   ndensity = "density, scaled to maximum of 1."
#' )
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
    params = list2(
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
  default_aes = aes(weight = 1, fill = after_stat(count)),

  required_aes = c("x", "y"),

  compute_group = function(data, scales, binwidth = NULL, bins = 30,
                           na.rm = FALSE) {
    check_installed("hexbin", reason = "for `stat_bin_hex()`")

    binwidth <- binwidth %||% hex_binwidth(bins, scales)
    wt <- data$weight %||% rep(1L, nrow(data))
    out <- hexBinSummarise(data$x, data$y, wt, binwidth, sum)
    out$density <- as.vector(out$value / sum(out$value, na.rm = TRUE))
    out$ndensity <- out$density / max(out$density, na.rm = TRUE)
    out$count <- out$value
    out$ncount <- out$count / max(out$count, na.rm = TRUE)
    out$value <- NULL

    out
  },

  # weight is no longer available after transformation
  dropped_aes = "weight"
)

