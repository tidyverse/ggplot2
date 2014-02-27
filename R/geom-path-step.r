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
#' x <- sort(rnorm(47))
#' qplot(seq_along(x), x, geom="step")
#'
#' # Steps go horizontally, then vertically (default)
#' qplot(seq_along(x), x, geom="step", direction = "hv")
#' plot(x, type = "s")
#' # Steps go vertically, then horizontally
#' qplot(seq_along(x), x, geom="step", direction = "vh")
#' plot(x, type = "S")
#'
#' # Also works with other aesthetics
#' df <- data.frame(
#'   x = sort(rnorm(50)),
#'   trt = sample(c("a", "b"), 50, rep = TRUE)
#' )
#' qplot(seq_along(x), x, data = df, geom="step", colour = trt)
geom_step <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
direction = "hv", ...) {
  GeomStep$new(mapping = mapping, data = data, stat = stat, position = position,
  direction = direction, ...)
}

GeomStep <- proto(Geom, {
  objname <- "step"

  details <- "Equivalent to plot(type='s')."

  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)

  draw <- function(., data, scales, coordinates, direction = "hv", ...) {
    data <- stairstep(data, direction)
    GeomPath$draw(data, scales, coordinates, ...)
  }
  guide_geom <- function(.) "path"

  default_stat <- function(.) StatIdentity
})


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
