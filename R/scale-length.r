#' Length scale.
#' 
#' @inheritParams scale_x_continuous
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum length of the plotting symbol after transformation.
#' @rdname scale_length
#' @export
#' @examples
#' p <- ggplot(mtcars)
#' p + geom_point(aes(x=wt, y=mpg))
#' p + geom_point(aes(x=wt, y=mpg)) + geom_rug(aes(x=wt))
#' mtQ <- data.frame(wt=quantile(x=mtcars$wt))
#' p + geom_point(aes(x=wt, y=mpg)) + geom_rug(aes(x=wt)) +
#'   geom_rug(aes(x=wt), colour="red", data=mtQ)
#' p + geom_point(aes(x=wt, y=mpg)) + geom_rug(aes(x=wt)) +
#'   geom_rug(aes(x=wt), colour="red", length=0.05, data=mtQ)
#' p + geom_point(aes(x=wt, y=mpg)) + geom_rug(aes(x=wt, length=mpg))
scale_length_continuous <- function(..., range = c(0.005, 0.1)) {
  continuous_scale("length", "length_c", rescale_pal(range), ...)
}

#' @rdname scale_length
#' @export
scale_length <- scale_length_continuous

#' @rdname scale_length
#' @export
scale_length_discrete <- function(..., range = c(0.005, 0.1)) {
  discrete_scale("length", "length_d",
    function(n) seq(range[1], range[2], length = n), ...)
}

# icon.length <- function() {
#   pos <- c(0.15, 0.3, 0.5, 0.75)
#   circleGrob(pos, pos, r=(c(0.1, 0.2, 0.3, 0.4)/2.5), gp=gpar(fill="grey50", col=NA))
# }
