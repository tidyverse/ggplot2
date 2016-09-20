#' Alpha scales.
#'
#' \code{scale_alpha} is an alias for \code{scale_alpha_continuous} since
#' that is the most common use of alpha, and it saves a bit of typing.
#'
#' @inheritParams continuous_scale
#' @param range range of output alpha values.  Should lie between 0 and 1.
#' @export
#' @examples
#' (p <- ggplot(mtcars, aes(mpg, cyl)) +
#'   geom_point(aes(alpha = cyl)))
#' p + scale_alpha("cylinders")
#' p + scale_alpha("number\nof\ncylinders")
#'
#' p + scale_alpha(range = c(0.4, 0.8))
#'
#' (p <- ggplot(mtcars, aes(mpg, cyl)) +
#'   geom_point(aes(alpha = factor(cyl))))
#' p + scale_alpha_discrete(range = c(0.4, 0.8))
scale_alpha <- function(name = waiver(), range = c(0.1, 1), breaks = waiver(),
                        minor_breaks = waiver(), labels = waiver(),
                        limits = NULL, oob = censor, expand = waiver(),
                        na.value = NA_real_, trans = "identity",
                        guide = "legend") {
  args <- as.list(environment)
  args$aesthetics <- "alpha"
  args$name <- "alpha_c"
  args$palette <- rescale_pal(range)
  args$range <- NULL
  do.call(continuous_scale, args)
}

#' @rdname scale_alpha
#' @export
scale_alpha_continuous <- scale_alpha

#' @rdname scale_alpha
#' @inheritParams discrete_scale
#' @export
scale_alpha_discrete <- function(name = waiver(), range = c(0.1, 1),
                                 breaks = waiver(), labels = waiver(),
                                 limits = NULL, expand = waiver(),
                                 na.value = NA, drop = TRUE, guide = "legend") {
  args <- as.list(environment)
  args$aesthetics <- "alpha"
  args$name <- "alpha_d"
  args$palette <- function(n) seq(range[1], range[2], length.out = n)
  args$range <- NULL
  do.call(discrete_scale, args)
}
