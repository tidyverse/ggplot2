#' Alpha transparency scales
#'
#' Alpha-transparency scales are not tremendously useful, but can be a
#' convenient way to visually down-weight less important observations.
#' \code{scale_alpha} is an alias for \code{scale_alpha_continuous} since
#' that is the most common use of alpha, and it saves a bit of typing.
#'
#' @param ... Other arguments passed on to \code{\link{continuous_scale}}
#'   or \code{\link{discrete_scale}} as appropriate, to control name, limits,
#'   breaks, labels and so forth.
#' @param range Output range of alpha values. Must lie between 0 and 1.
#' @family colour scales
#' @export
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(alpha = year))
#'
#' p
#' p + scale_alpha("cylinders")
#' p + scale_alpha(range = c(0.4, 0.8))
scale_alpha <- function(..., range = c(0.1, 1)) {
  continuous_scale("alpha", "alpha_c", rescale_pal(range), ...)
}

#' @rdname scale_alpha
#' @export
scale_alpha_continuous <- scale_alpha

#' @rdname scale_alpha
#' @export
scale_alpha_discrete <- function(..., range = c(0.1, 1)) {
  discrete_scale("alpha", "alpha_d",
    function(n) seq(range[1], range[2], length.out = n), ...)
}
