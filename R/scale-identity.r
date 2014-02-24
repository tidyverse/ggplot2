#' Use values without scaling.
#'
#' @name scale_identity
#' @param ... Other arguments passed on to \code{\link{discrete_scale}} or
#'   \code{\link{continuous_scale}}
#' @param guide Guide to use for this scale - defaults to \code{"none"}.
#' @examples
#' colour <- c("red", "green", "blue", "yellow")
#' qplot(1:4, 1:4, fill = colour, geom = "tile")
#' qplot(1:4, 1:4, fill = colour, geom = "tile") + scale_fill_identity()
#'
#' # To get a legend guide, specify guide = "legend"
#' qplot(1:4, 1:4, fill = colour, geom = "tile") +
#'   scale_fill_identity(guide = "legend")
#' # But you'll typically also need to supply breaks and labels:
#' qplot(1:4, 1:4, fill = colour, geom = "tile") +
#'   scale_fill_identity("trt", labels = letters[1:4], breaks = colour,
#'   guide = "legend")
#'
#' # cyl scaled to appropriate size
#' qplot(mpg, wt, data = mtcars, size = cyl)
#'
#' # cyl used as point size
#' qplot(mpg, wt, data = mtcars, size = cyl) + scale_size_identity()
NULL

#' @rdname scale_identity
#' @export
scale_colour_identity <- function(..., guide = "none") {
  identity_scale(discrete_scale("colour", "identity", identity_pal(), ..., guide = guide))
}

#' @rdname scale_identity
#' @export
scale_fill_identity <- function(..., guide = "none") {
  identity_scale(discrete_scale("fill", "identity", identity_pal(), ..., guide = guide))

}

#' @rdname scale_identity
#' @export
scale_shape_identity <- function(..., guide = "none") {
  identity_scale(continuous_scale("shape", "identity", identity_pal(), ...,  guide = guide))
}

#' @rdname scale_identity
#' @export
scale_linetype_identity <- function(..., guide = "none") {
  identity_scale(discrete_scale("linetype", "identity", identity_pal(), ..., guide = guide))

}

#' @rdname scale_identity
#' @export
scale_alpha_identity <- function(..., guide = "none") {
  identity_scale(continuous_scale("alpha", "identity", identity_pal(), ..., guide = guide))

}

#' @rdname scale_identity
#' @export
scale_size_identity <- function(..., guide = "none") {
  identity_scale(continuous_scale("size", "identity", identity_pal(), ..., guide = guide))
}

identity_scale <- function(x) {
  structure(x, class = c("identity", class(x)))
}

#' @export
scale_map.identity <- function(scale, x) {
  if (is.factor(x)) {
    as.character(x)
  } else {
    x
  }
}
#' @export
scale_train.identity <- function(scale, x) {
  # do nothing if no guide, otherwise train so we know what breaks to use
  if (scale$guide == "none") return()
  NextMethod()
}
