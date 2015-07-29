#' @param direction direction of stairs: 'vh' for vertical then horizontal, or
#'   'hv' for horizontal then vertical
#' @export
#' @rdname geom_path
geom_step <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", direction = "hv",
                      show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStep,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    geom_params = list(direction = direction),
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomStep <- ggproto("GeomStep", Geom,
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw = function(data, scales, coordinates, direction = "hv", ...) {
    data <- stairstep(data, direction)
    GeomPath$draw(data, scales, coordinates, ...)
  },

  draw_key = draw_key_path
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
    ys <- c(1, rep(2:n, each = 2))
  } else {
    ys <- rep(1:n, each = 2)[-2*n]
    xs <- c(1, rep(2:n, each = 2))
  }

  data.frame(
    x = data$x[xs],
    y = data$y[ys],
    data[xs, setdiff(names(data), c("x", "y"))]
  )
}
