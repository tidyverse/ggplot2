#' Connect observations by stairs.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "step")}
#'
#' @inheritParams geom_point
#' @param direction direction of stairs: 'vh' for vertical then horizontal, or
#'   'hv' for horizontal then vertical
#' @export
#' @examples
#' # Simple quantiles/ECDF from examples(plot)
#' set.seed(1492)
#' df <- data.frame(
#'   x = sort(rnorm(47))
#' )
#' ggplot(df, aes(seq_along(x), x)) + geom_step()
#'
#' # Steps go horizontally, then vertically (default)
#' ggplot(df, aes(seq_along(x), x)) + geom_step(direction = "hv")
#' plot(df$x, type = "s")
#' # Steps go vertically, then horizontally
#' ggplot(df, aes(seq_along(x), x)) + geom_step(direction = "vh")
#' plot(df$x, type = "S")
#'
#' # Also works with other aesthetics
#' df <- data.frame(
#'   x = sort(rnorm(50)),
#'   trt = sample(c("a", "b"), 50, rep = TRUE)
#' )
#' ggplot(df, aes(seq_along(x), x)) + geom_step(aes(colour = trt))
geom_step <- function (mapping = NULL, data = NULL, stat = "identity",
  position = "identity", direction = "hv", show_guide = NA, inherit.aes = TRUE,
  ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStep,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    geom_params = list(direction = direction),
    params = list(...)
  )
}

GeomStep <- R6::R6Class("GeomStep", inherit = Geom,
  public = list(
    objname = "step",

    details = "Equivalent to plot(type='s').",

    default_aes = function() aes(colour="black", size=0.5, linetype=1, alpha = NA),

    draw = function(data, scales, coordinates, direction = "hv", ...) {
      data <- stairstep(data, direction)
      # R6 TODO: Avoid instantiation
      GeomPath$new()$draw(data, scales, coordinates, ...)
    },

    guide_geom = function() "path",

    default_stat = function() StatIdentity
  )
)


# Calculate stairsteps
# Used by \code{\link{geom_step}}
#
# @keyword internal
stairstep <- function(data, direction="hv") {
  direction <- match.arg(direction, c("hv", "vh"))
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)

  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2*n]
    ys <- c(1, rep(2:n, each=2))
  } else {
    ys <- rep(1:n, each = 2)[-2*n]
    xs <- c(1, rep(2:n, each=2))
  }

  data.frame(
    x = data$x[xs],
    y = data$y[ys],
    data[xs, setdiff(names(data), c("x", "y"))]
  )
}
