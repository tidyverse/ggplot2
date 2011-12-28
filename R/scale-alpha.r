#' Alpha scale for continuous variable.
#'
#' @param ... Other arguments passed on to \code{\link{continuous_scale}} 
#'   to control name, limits, breaks, labels and so forth.
#' @param range range of output alpha values.  Should lie between 0 and 1.
#' @export
#' @examples
#' (p <- qplot(mpg, cyl, data=mtcars, alpha=cyl))
#' p + scale_alpha("cylinders")
#' p + scale_alpha("number\nof\ncylinders")
#' 
#' p + scale_alpha(range = c(0.4, 0.8))
scale_alpha <- function(..., range = c(0.1, 1)) {
  continuous_scale("alpha", "alpha_c", rescale_pal(range), ...)
}

#' @rdname scale_alpha
#' @export
scale_alpha_continuous <- scale_alpha

#' Alpha scale for discrete variable.
#' @param range range of output alpha values.  Should lie between 0 and 1.
#'
#' @param ... Other arguments passed on to \code{\link{discrete_scale}} 
#'   to control name, limits, breaks, labels and so forth.
#' @export
scale_alpha_discrete <- function(..., range = c(0.1, 1)) {
  discrete_scale("alpha", "alpha_d",
    function(n) seq(range[1], range[2], length = n), ...)
}

icon.alpha <- function() {
  x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  rectGrob(x, width=0.25, 
    gp=gpar(fill=alpha("black", x), col=NA)
  )
}
