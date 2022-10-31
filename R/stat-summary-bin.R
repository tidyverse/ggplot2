#' @rdname stat_summary
#' @inheritParams stat_bin
#' @export
stat_summary_bin <- function(mapping = NULL, data = NULL,
                             geom = "pointrange", position = "identity",
                             ...,
                             fun.data = NULL,
                             fun = NULL,
                             fun.max = NULL,
                             fun.min = NULL,
                             fun.args = list(),
                             bins = 30,
                             binwidth = NULL,
                             breaks = NULL,
                             na.rm = FALSE,
                             orientation = NA,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             fun.y = deprecated(),
                             fun.ymin = deprecated(),
                             fun.ymax = deprecated()) {
  if (lifecycle::is_present(fun.y)) {
    deprecate_warn0("3.3.0", "stat_summary_bin(fun.y)", "stat_summary_bin(fun)")
    fun = fun %||% fun.y
  }
  if (lifecycle::is_present(fun.ymin)) {
    deprecate_warn0("3.3.0", "stat_summary_bin(fun.ymin)", "stat_summary_bin(fun.min)")
    fun.min = fun.min %||% fun.ymin
  }
  if (lifecycle::is_present(fun.ymax)) {
    deprecate_warn0("3.3.0", "stat_summary_bin(fun.ymax)", "stat_summary_bin(fun.max)")
    fun.max = fun.max %||% fun.ymax
  }
  layer(
    data = data,
    mapping = mapping,
    stat = StatSummaryBin,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      fun.data = fun.data,
      fun = fun,
      fun.max = fun.max,
      fun.min = fun.min,
      fun.args = fun.args,
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSummaryBin <- ggproto("StatSummaryBin", Stat,
  required_aes = c("x", "y"),

  extra_params = c("na.rm", "orientation"),
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  compute_group = function(data, scales, fun.data = NULL, fun = NULL,
                           fun.max = NULL, fun.min = NULL, fun.args = list(),
                           bins = 30, binwidth = NULL, breaks = NULL,
                           origin = NULL, right = FALSE, na.rm = FALSE,
                           flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    fun <- make_summary_fun(fun.data, fun, fun.max, fun.min, fun.args)
    x <- flipped_names(flipped_aes)$x
    breaks <- bin2d_breaks(scales[[x]], breaks, origin, binwidth, bins, right = right)

    data$bin <- cut(data$x, breaks, include.lowest = TRUE, labels = FALSE)
    out <- dapply(data, "bin", fun)

    locs <- bin_loc(breaks, out$bin)
    out$x <- locs$mid
    out$width <- if (scales[[x]]$is_discrete()) 0.9 else locs$length
    out$flipped_aes <- flipped_aes
    flip_data(out, flipped_aes)
  }
)

make_summary_fun <- function(fun.data, fun, fun.max, fun.min, fun.args) {
  force(fun.data)
  force(fun)
  force(fun.max)
  force(fun.min)
  force(fun.args)

  if (!is.null(fun.data)) {
    # Function that takes complete data frame as input
    fun.data <- as_function(fun.data)
    function(df) {
      inject(fun.data(df$y, !!!fun.args))
    }
  } else if (!is.null(fun) || !is.null(fun.max) || !is.null(fun.min)) {
    # Three functions that take vectors as inputs

    call_f <- function(fun, x) {
      if (is.null(fun)) return(NA_real_)
      fun <- as_function(fun)
      inject(fun(x, !!!fun.args))
    }

    function(df, ...) {
      data_frame0(
        ymin = call_f(fun.min, df$y),
        y = call_f(fun, df$y),
        ymax = call_f(fun.max, df$y)
      )
    }
  } else {
    cli::cli_inform("No summary function supplied, defaulting to {.fn mean_se}")
    function(df) {
      mean_se(df$y)
    }
  }
}
