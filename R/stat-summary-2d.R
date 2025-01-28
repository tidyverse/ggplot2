#' Bin and summarise in 2d (rectangle & hexagons)
#'
#' `stat_summary_2d()` is a 2d variation of [stat_summary()].
#' `stat_summary_hex()` is a hexagonal variation of
#' [stat_summary_2d()]. The data are divided into bins defined
#' by `x` and `y`, and then the values of `z` in each cell is
#' are summarised with `fun`.
#'
#' @section Aesthetics:
#'  - `x`: horizontal position
#'  - `y`: vertical position
#'  - `z`: value passed to the summary function
#'
#' @eval rd_computed_vars(
#'   "x,y" = "Location.",
#'   value = "Value of summary statistic."
#' )
#'
#' @section Dropped variables:
#' \describe{
#'   \item{`z`}{After binning, the z values of individual data points are no longer available.}
#' }
#' @seealso [stat_summary_hex()] for hexagonal summarization.
#'   [stat_bin_2d()] for the binning options.
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams stat_bin_2d
#' @param drop drop if the output of `fun` is `NA`.
#' @param fun function for summary.
#' @param fun.args A list of extra arguments to pass to `fun`
#' @inheritSection stat_bin_2d Controlling binning parameters for the x and y directions
#' @export
#' @examples
#' d <- ggplot(diamonds, aes(carat, depth, z = price))
#' d + stat_summary_2d()
#'
#' # Specifying function
#' d + stat_summary_2d(fun = function(x) sum(x^2))
#' d + stat_summary_2d(fun = ~ sum(.x^2))
#' d + stat_summary_2d(fun = var)
#' d + stat_summary_2d(fun = "quantile", fun.args = list(probs = 0.1))
#'
#' if (requireNamespace("hexbin")) {
#' d + stat_summary_hex()
#' d + stat_summary_hex(fun = ~ sum(.x^2))
#' }
stat_summary_2d <- function(mapping = NULL, data = NULL,
                            geom = "tile", position = "identity",
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
    stat = StatSummary2d,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
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

#' @export
#' @rdname stat_summary_2d
#' @usage NULL
stat_summary2d <- function(...) {
  cli::cli_inform("Please use {.fn stat_summary_2d} instead")
  stat_summary_2d(...)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSummary2d <- ggproto("StatSummary2d", Stat,
  default_aes = aes(fill = after_stat(value)),

  required_aes = c("x", "y", "z"),
  dropped_aes = "z", # z gets dropped during statistical transformation

  setup_params = function(self, data, params) {

    if (is.character(params$drop)) {
      params$drop <- !identical(params$drop, "none")
    }

    params <- fix_bin_params(params, fun = snake_class(self), version = "3.5.2")
    vars <- c("origin", "binwidth", "breaks", "center", "boundary")
    params[vars] <- lapply(params[vars], dual_param, default = NULL)
    params$closed <- dual_param(params$closed, list(x = "right", y = "right"))

    params
  },

  compute_group = function(data, scales, binwidth = NULL, bins = 30,
                           breaks = NULL, origin = NULL, drop = TRUE,
                           fun = "mean", fun.args = list(),
                           boundary = 0, closed = NULL, center = NULL) {
    bins <- dual_param(bins, list(x = 30, y = 30))

    xbin <- compute_bins(
      data$x, scales$x, breaks$x, binwidth$x, bins$x,
      center$x, boundary$x, closed$x
    )
    ybin <- compute_bins(
      data$y, scales$y, breaks$y, binwidth$y, bins$y,
      center$y, boundary$y, closed$y
    )
    cut_id <- list(
      xbin = as.integer(bin_cut(data$x, xbin)),
      ybin = as.integer(bin_cut(data$y, ybin))
    )

    fun <- as_function(fun)
    f <- function(x) {
      inject(fun(x, !!!fun.args))
    }
    out <- tapply_df(data$z, cut_id, f, drop = drop)

    xdim <- bin_loc(xbin$breaks, out$xbin)
    out$x <- xdim$mid
    out$width <- xdim$length

    ydim <- bin_loc(ybin$breaks, out$ybin)
    out$y <- ydim$mid
    out$height <- ydim$length

    out
  }
)

# Adaptation of tapply that returns a data frame instead of a matrix
tapply_df <- function(x, index, fun, ..., drop = TRUE) {
  labels <- lapply(index, ulevels, na.last = NA) # drop NA
  out <- expand.grid(labels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  grps <- split(x, index)
  names(grps) <- NULL
  out$value <- unlist(lapply(grps, fun, ...))

  if (drop) {
    n <- lengths(grps)
    out <- out[n > 0, , drop = FALSE]
  }

  out
}
