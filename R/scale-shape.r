#' Scale for shapes, aka glyphs.
#'
#' A continuous variable can not be mapped to shape.
#'
#' @param solid Are the shapes solid, \code{TRUE}, or hollow \code{FALSE}?
#' @inheritParams scale_x_discrete
#' @rdname scale_shape
#' @export
#' @examples
#' dsmall <- diamonds[sample(nrow(diamonds), 100), ]
#'
#' (d <- ggplot(dsmall, aes(carat, price)) + geom_point(aes(shape = cut)))
#' d + scale_shape(solid = TRUE) # the default
#' d + scale_shape(solid = FALSE)
#' d + scale_shape(name = "Cut of diamond")
#' d + scale_shape(name = "Cut of\ndiamond")
#'
#' # To change order of levels, change order of
#' # underlying factor
#' levels(dsmall$cut) <- c("Fair", "Good", "Very Good", "Premium", "Ideal")
#'
#' # Need to recreate plot to pick up new data
#' ggplot(dsmall, aes(price, carat)) + geom_point(aes(shape = cut))
#'
#' # Or for short:
#' d %+% dsmall
scale_shape <- function(..., solid = TRUE) {
  discrete_scale("shape", "shape_d", shape_pal(solid), ...)
}

#' @rdname scale_shape
#' @export
#' @usage NULL
scale_shape_discrete <- scale_shape

#' @rdname scale_shape
#' @export
#' @usage NULL
scale_shape_continuous <- function(...) {
  stop("A continuous variable can not be mapped to shape", call. = FALSE)
}
