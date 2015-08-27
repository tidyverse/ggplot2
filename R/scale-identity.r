#' Use values without scaling.
#'
#' @name scale_identity
#' @param ... Other arguments passed on to \code{\link{discrete_scale}} or
#'   \code{\link{continuous_scale}}
#' @param guide Guide to use for this scale - defaults to \code{"none"}.
#' @examples
#' ggplot(luv_colours, aes(u, v)) +
#'   geom_point(aes(colour = col), size = 3) +
#'   scale_color_identity() +
#'   coord_equal()
#'
#' df <- data.frame(
#'   x = 1:4,
#'   y = 1:4,
#'   colour = c("red", "green", "blue", "yellow")
#' )
#' ggplot(df, aes(x, y)) + geom_tile(aes(fill = colour))
#' ggplot(df, aes(x, y)) +
#'   geom_tile(aes(fill = colour)) +
#'   scale_fill_identity()
#'
#' # To get a legend guide, specify guide = "legend"
#' ggplot(df, aes(x, y)) +
#'   geom_tile(aes(fill = colour)) +
#'   scale_fill_identity(guide = "legend")
#' # But you'll typically also need to supply breaks and labels:
#' ggplot(df, aes(x, y)) +
#'   geom_tile(aes(fill = colour)) +
#'   scale_fill_identity("trt", labels = letters[1:4], breaks = df$colour,
#'   guide = "legend")
#'
#' # cyl scaled to appropriate size
#' ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(size = cyl))
#'
#' # cyl used as point size
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(size = cyl)) +
#'   scale_size_identity()
NULL

#' @rdname scale_identity
#' @export
scale_colour_identity <- function(..., guide = "none") {
  sc <- discrete_scale("colour", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- ScaleDiscreteIdentity
  class(sc) <- class(ScaleDiscreteIdentity)
  sc
}

#' @rdname scale_identity
#' @export
scale_fill_identity <- function(..., guide = "none") {
  sc <- discrete_scale("fill", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- ScaleDiscreteIdentity
  class(sc) <- class(ScaleDiscreteIdentity)
  sc
}

#' @rdname scale_identity
#' @export
scale_shape_identity <- function(..., guide = "none") {
  sc <- continuous_scale("shape", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- ScaleContinuousIdentity
  class(sc) <- class(ScaleContinuousIdentity)
  sc
}

#' @rdname scale_identity
#' @export
scale_linetype_identity <- function(..., guide = "none") {
  sc <- discrete_scale("linetype", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- ScaleDiscreteIdentity
  class(sc) <- class(ScaleDiscreteIdentity)
  sc
}

#' @rdname scale_identity
#' @export
scale_alpha_identity <- function(..., guide = "none") {
  sc <- continuous_scale("alpha", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- ScaleContinuousIdentity
  class(sc) <- class(ScaleContinuousIdentity)
  sc
}

#' @rdname scale_identity
#' @export
scale_size_identity <- function(..., guide = "none") {
  sc <- continuous_scale("size", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- ScaleContinuousIdentity
  class(sc) <- class(ScaleContinuousIdentity)
  sc
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleDiscreteIdentity <- ggproto("ScaleDiscreteIdentity", ScaleDiscrete,
  map = function(x) {
    if (is.factor(x)) {
      as.character(x)
    } else {
      x
    }
  },

  train = function(self, x) {
    # do nothing if no guide, otherwise train so we know what breaks to use
    if (self$guide == "none") return()
    ggproto_parent(ScaleDiscrete, self)$train(x)
  }
)


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousIdentity <- ggproto("ScaleContinuousIdentity", ScaleContinuous,
  map = function(x) {
    if (is.factor(x)) {
      as.character(x)
    } else {
      x
    }
  },

  train = function(self, x) {
    # do nothing if no guide, otherwise train so we know what breaks to use
    if (self$guide == "none") return()
    ggproto_parent(ScaleDiscrete, self)$train(x)
  }
)
