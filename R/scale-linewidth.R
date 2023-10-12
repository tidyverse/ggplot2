#' Scales for line width
#'
#' `scale_linewidth` scales the width of lines and polygon strokes. Due to
#' historical reasons, it is also possible to control this with the `size`
#' aesthetic, but using `linewidth` is encourage to clearly differentiate area
#' aesthetics from stroke width aesthetics.
#'
#' @name scale_linewidth
#' @inheritParams continuous_scale
#' @inheritParams binned_scale
#' @seealso
#' The documentation for [differentiation related aesthetics][aes_linetype_size_shape].
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum size of the plotting symbol after transformation.
#' @examples
#' p <- ggplot(economics, aes(date, unemploy, linewidth = uempmed)) +
#'   geom_line(lineend = "round")
#' p
#' p + scale_linewidth("Duration of\nunemployment")
#' p + scale_linewidth(range = c(0, 4))
#'
#' # Binning can sometimes make it easier to match the scaled data to the legend
#' p + scale_linewidth_binned()
#'
NULL

#' @rdname scale_linewidth
#' @export
#' @usage NULL
scale_linewidth_continuous <- function(name = waiver(), breaks = waiver(),
                                       labels = waiver(), limits = NULL,
                                       range = c(1, 6), trans = "identity",
                                       guide = "legend") {
  continuous_scale("linewidth", palette = rescale_pal(range), name = name,
                   breaks = breaks, labels = labels, limits = limits, trans = trans,
                   guide = guide)
}

#' @rdname scale_linewidth
#' @export
scale_linewidth <- scale_linewidth_continuous

#' @rdname scale_linewidth
#' @export
scale_linewidth_binned <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                              limits = NULL, range = c(1, 6), n.breaks = NULL,
                              nice.breaks = TRUE, trans = "identity", guide = "bins") {
  binned_scale("linewidth", palette = rescale_pal(range), name = name,
               breaks = breaks, labels = labels, limits = limits, trans = trans,
               n.breaks = n.breaks, nice.breaks = nice.breaks, guide = guide)
}

#' @rdname scale_linewidth
#' @export
#' @usage NULL
scale_linewidth_discrete <- function(...) {
  cli::cli_warn("Using {.field linewidth} for a discrete variable is not advised.")
  args <- list2(...)
  args$call <- args$call %||% current_call()
  exec(scale_linewidth_ordinal, !!!args)
}

#' @rdname scale_linewidth
#' @export
#' @usage NULL
scale_linewidth_ordinal <- function(..., range = c(2, 6)) {
  force(range)

  discrete_scale(
    "linewidth",
    palette = function(n) seq(range[1], range[2], length.out = n),
    ...
  )
}

#' @rdname scale_linewidth
#' @export
#' @usage NULL
scale_linewidth_datetime <- function(..., range = c(1, 6)) {
  datetime_scale("linewidth", "time", palette = rescale_pal(range), ...)
}

#' @rdname scale_linewidth
#' @export
#' @usage NULL
scale_linewidth_date <- function(..., range = c(1, 6)) {
  datetime_scale("linewidth", "date", palette = rescale_pal(range), ...)
}
