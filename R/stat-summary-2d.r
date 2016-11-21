#' Bin and summarise in 2d (rectangle & hexagons)
#'
#' \code{stat_summary_2d} is a 2d variation of \code{\link{stat_summary}}.
#' \code{stat_summary_hex} is a hexagonal variation of
#' \code{\link{stat_summary_2d}}. The data are divided into bins defined
#' by \code{x} and \code{y}, and then the values of \code{z} in each cell is
#' are summarised with \code{fun}.
#'
#' @section Aesthetics:
#' \itemize{
#'  \item \code{x}: horizontal position
#'  \item \code{y}: vertical position
#'  \item \code{z}: value passed to the summary function
#' }
#' @section Computed variables:
#' \describe{
#'   \item{x,y}{Location}
#'   \item{value}{Value of summary statistic.}
#' }
#' @seealso \code{\link{stat_summary_hex}} for hexagonal summarization.
#'   \code{\link{stat_bin2d}} for the binning options.
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams stat_bin_2d
#' @param drop drop if the output of \code{fun} is \code{NA}.
#' @param fun function for summary.
#' @param fun.args A list of extra arguments to pass to \code{fun}
#' @export
#' @examples
#' d <- ggplot(diamonds, aes(carat, depth, z = price))
#' d + stat_summary_2d()
#'
#' # Specifying function
#' d + stat_summary_2d(fun = function(x) sum(x^2))
#' d + stat_summary_2d(fun = var)
#' d + stat_summary_2d(fun = "quantile", fun.args = list(probs = 0.1))
#'
#' if (requireNamespace("hexbin")) {
#' d + stat_summary_hex()
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

#' @export
#' @rdname stat_summary_2d
#' @usage NULL
stat_summary2d <- function(...) {
  message("Please use stat_summary_2d() instead")
  stat_summary_2d(...)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSummary2d <- ggproto("StatSummary2d", Stat,
  default_aes = aes(fill = ..value..),

  required_aes = c("x", "y", "z"),

  compute_group = function(data, scales, binwidth = NULL, bins = 30,
                           breaks = NULL, origin = NULL, drop = TRUE,
                           fun = "mean", fun.args = list()) {
    origin <- dual_param(origin, list(NULL, NULL))
    binwidth <- dual_param(binwidth, list(NULL, NULL))
    breaks <- dual_param(breaks, list(NULL, NULL))
    bins <- dual_param(bins, list(x = 30, y = 30))

    xbreaks <- bin2d_breaks(scales$x, breaks$x, origin$x, binwidth$x, bins$x)
    ybreaks <- bin2d_breaks(scales$y, breaks$y, origin$y, binwidth$y, bins$y)

    xbin <- cut(data$x, xbreaks, include.lowest = TRUE, labels = FALSE)
    ybin <- cut(data$y, ybreaks, include.lowest = TRUE, labels = FALSE)

    f <- function(x) {
      do.call(fun, c(list(quote(x)), fun.args))
    }
    out <- tapply_df(data$z, list(xbin = xbin, ybin = ybin), f, drop = drop)

    xdim <- bin_loc(xbreaks, out$xbin)
    out$x <- xdim$mid
    out$width <- xdim$length

    ydim <- bin_loc(ybreaks, out$ybin)
    out$y <- ydim$mid
    out$height <- ydim$length

    out
  }
)

# Adaptation of tapply that returns a data frame instead of a matrix
tapply_df <- function(x, index, fun, ..., drop = TRUE) {
  labels <- lapply(index, ulevels)
  out <- expand.grid(labels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  grps <- split(x, index)
  names(grps) <- NULL
  out$value <- unlist(lapply(grps, fun, ...))

  if (drop) {
    n <- vapply(grps, length, integer(1))
    out <- out[n > 0, , drop = FALSE]
  }

  out
}
