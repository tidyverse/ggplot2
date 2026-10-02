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
#' @inheritParams continuous_scale
#' @family colour scales
#' @family alpha scales
#' @seealso
#' The documentation on [colour aesthetics][aes_colour_fill_alpha].
#'
#' Other alpha scales: [scale_alpha_manual()], [scale_alpha_identity()].
#'
#' The `r link_book("alpha scales section", "scales-colour#sec-scales-alpha")`
#' @export
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(alpha = year))
#'
#' # The default range of 0.1-1.0 leaves all data visible
#' p
#'
#' # Include 0 in the range to make data invisible
#' p + scale_alpha(range = c(0, 1))
#'
#' # Changing the title
#' p + scale_alpha("cylinders")
scale_alpha <- function(name = waiver(), ..., range = NULL, aesthetics = "alpha") {
  palette <- if (!is.null(range)) pal_rescale(range) else NULL
  continuous_scale(aesthetics, name = name, palette = palette, ...)
}

#' @rdname scale_alpha
#' @export
scale_alpha_continuous <- scale_alpha

#' @rdname scale_alpha
#' @export
scale_alpha_binned <- function(name = waiver(), ..., range = NULL, aesthetics = "alpha") {
  palette <- if (!is.null(range)) pal_rescale(range) else NULL
  binned_scale(aesthetics, name = name, palette = palette, ...)
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
scale_alpha_ordinal <- function(name = waiver(), ..., range = NULL, aesthetics = "alpha") {
  palette <- if (!is.null(range)) {
    function(n) seq(range[1], range[2], length.out = n)
  } else {
    NULL
  }
  discrete_scale(aesthetics, name = name, palette = palette, ...)
}

#' @rdname scale_alpha
#' @export
#' @usage NULL
scale_alpha_datetime <- function(name = waiver(), ..., range = NULL, aesthetics = "alpha") {
  palette <- if (!is.null(range)) pal_rescale(range) else NULL
  datetime_scale(
    aesthetics = aesthetics, transform = "time", name = name,
    palette = palette, ...
  )
}

#' @rdname scale_alpha
#' @export
#' @usage NULL
scale_alpha_date <- function(name = waiver(), ..., range = NULL, aesthetics = "alpha"){
  palette <- if (!is.null(range)) pal_rescale(range) else NULL
  datetime_scale(
    aesthetics = aesthetics, transform = "date", name = name,
    palette = palette, ...
  )
}
