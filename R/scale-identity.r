#' Use values without scaling
#'
#' Use this set of scales when your data has already been scaled, i.e. it
#' already represents aesthetic values that ggplot2 can handle directly.
#' These scales will not produce a legend unless you also supply the `breaks`,
#' `labels`, and type of `guide` you want.
#'
#' The functions `scale_colour_identity()`, `scale_fill_identity()`, `scale_size_identity()`,
#' etc. work on the aesthetics specified in the scale name: `colour`, `fill`, `size`,
#' etc. However, the functions `scale_colour_identity()` and `scale_fill_identity()` also
#' have an optional `aesthetics` argument that can be used to define both `colour` and
#' `fill` aesthetic mappings via a single function call. The functions
#' `scale_discrete_identity()` and `scale_continuous_identity()` are generic scales that
#' can work with any aesthetic or set of aesthetics provided via the `aesthetics`
#' argument.
#'
#' @param ... Other arguments passed on to [discrete_scale()] or
#'   [continuous_scale()]
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @param guide Guide to use for this scale. Defaults to `"none"`.
#' @examples
#' ggplot(luv_colours, aes(u, v)) +
#'   geom_point(aes(colour = col), size = 3) +
#'   scale_color_identity() +
#'   coord_fixed()
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
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(size = cyl))
#'
#' # cyl used as point size
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(size = cyl)) +
#'   scale_size_identity()
#' @name scale_identity
#' @aliases NULL
NULL

#' @rdname scale_identity
#' @export
scale_colour_identity <- function(..., guide = "none", aesthetics = "colour") {
  sc <- discrete_scale(aesthetics, "identity", identity_pal(), ..., guide = guide,
    super = ScaleDiscreteIdentity)

  sc
}

#' @rdname scale_identity
#' @export
scale_fill_identity <- function(..., guide = "none", aesthetics = "fill") {
  sc <- discrete_scale(aesthetics, "identity", identity_pal(), ..., guide = guide,
    super = ScaleDiscreteIdentity)

  sc
}

#' @rdname scale_identity
#' @export
scale_shape_identity <- function(..., guide = "none") {
  sc <- continuous_scale("shape", "identity", identity_pal(), ..., guide = guide,
    super = ScaleContinuousIdentity)

  sc
}

#' @rdname scale_identity
#' @export
scale_linetype_identity <- function(..., guide = "none") {
  sc <- discrete_scale("linetype", "identity", identity_pal(), ..., guide = guide,
    super = ScaleDiscreteIdentity)

  sc
}

#' @rdname scale_identity
#' @export
scale_alpha_identity <- function(..., guide = "none") {
  sc <- continuous_scale("alpha", "identity", identity_pal(), ..., guide = guide,
    super = ScaleContinuousIdentity)

  sc
}

#' @rdname scale_identity
#' @export
scale_size_identity <- function(..., guide = "none") {
  sc <- continuous_scale("size", "identity", identity_pal(), ..., guide = guide,
    super = ScaleContinuousIdentity)

  sc
}

#' @rdname scale_identity
#' @export
scale_discrete_identity <- function(aesthetics, ..., guide = "none") {
  sc <- discrete_scale(aesthetics, "identity", identity_pal(), ..., guide = guide,
                       super = ScaleDiscreteIdentity)

  sc
}

#' @rdname scale_identity
#' @export
scale_continuous_identity <- function(aesthetics, ..., guide = "none") {
  sc <- continuous_scale(aesthetics, "identity", identity_pal(), ..., guide = guide,
                         super = ScaleContinuousIdentity)

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
    if (identical(self$guide, "none")) return()
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
    if (identical(self$guide, "none")) return()
    ggproto_parent(ScaleContinuous, self)$train(x)
  }
)
