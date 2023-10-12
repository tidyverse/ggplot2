#' Alpha transparency scales
#'
#' Alpha-transparency scales are not tremendously useful, but can be a
#' convenient way to visually down-weight less important observations.
#' `scale_alpha()` is an alias for `scale_alpha_continuous()` since
#' that is the most common use of alpha, and it saves a bit of typing.
#'
#' @param ... Other arguments passed on to [continuous_scale()], [binned_scale()],
#'   or [discrete_scale()] as appropriate, to control name, limits,
#'   breaks, labels and so forth.
#' @param range Output range of alpha values. Must lie between 0 and 1.
#' @family colour scales
#' @family alpha scales
#' @seealso
#' The documentation on [colour aesthetics][aes_colour_fill_alpha].
#'
#' Other alpha scales: [scale_alpha_manual()], [scale_alpha_identity()].
#' @export
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(alpha = year))
#'
#' p
#' p + scale_alpha("cylinders")
#' p + scale_alpha(range = c(0.4, 0.8))
scale_alpha <- function(..., range = c(0.1, 1)) {
  continuous_scale("alpha", palette = rescale_pal(range), ...)
}

#' @rdname scale_alpha
#' @export
scale_alpha_continuous <- scale_alpha

#' @rdname scale_alpha
#' @export
scale_alpha_binned <- function(..., range = c(0.1, 1)) {
  binned_scale("alpha", palette = rescale_pal(range), ...)
}

#' @rdname scale_alpha
#' @export
scale_alpha_discrete <- function(...) {
  cli::cli_warn("Using alpha for a discrete variable is not advised.")
  args <- list2(...)
  args$call <- args$call %||% current_call()
  exec(scale_alpha_ordinal, !!!args)
}

#' @rdname scale_alpha
#' @export
scale_alpha_ordinal <- function(..., range = c(0.1, 1)) {
  discrete_scale(
    "alpha",
    palette = function(n) seq(range[1], range[2], length.out = n),
    ...
  )
}

#' @rdname scale_alpha
#' @export
#' @usage NULL
scale_alpha_datetime <- function(..., range = c(0.1, 1)) {
  datetime_scale("alpha", "time", palette = rescale_pal(range), ...)
}

#' @rdname scale_alpha
#' @export
#' @usage NULL
scale_alpha_date <- function(..., range = c(0.1, 1)){
  datetime_scale("alpha", "date", palette = rescale_pal(range), ...)
}
