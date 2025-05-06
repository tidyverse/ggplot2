#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSummaryHex <- ggproto(
  "StatSummaryHex", Stat,
  default_aes = aes(fill = after_stat(value)),

  required_aes = c("x", "y", "z"),

  dropped_aes = "z", # z gets dropped during statistical transformation

  compute_group = function(data, scales, binwidth = NULL, bins = 30, drop = TRUE,
                           fun = "mean", fun.args = list()) {
    check_installed("hexbin", reason = "for `stat_summary_hex()`.")

    binwidth <- binwidth %||% hex_binwidth(bins, scales)
    fun <- as_function(fun)
    hexBinSummarise(data$x, data$y, data$z, binwidth,
                    fun = fun, fun.args = fun.args, drop = drop)
  }
)

#' @export
#' @rdname stat_summary_2d
#' @inheritParams stat_bin_hex
stat_summary_hex <- make_constructor(StatSummaryHex, geom = "hex")
