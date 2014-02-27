#' Horizontal error bars
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "errorbarh")}
#'
#' @seealso \code{\link{geom_errorbar}}: vertical error bars
#' @inheritParams geom_point
#' @export
#' @examples
#' df <- data.frame(
#'   trt = factor(c(1, 1, 2, 2)),
#'   resp = c(1, 5, 3, 4),
#'   group = factor(c(1, 2, 1, 2)),
#'   se = c(0.1, 0.3, 0.3, 0.2)
#' )
#'
#' # Define the top and bottom of the errorbars
#'
#' p <- ggplot(df, aes(resp, trt, colour = group))
#' p + geom_point() +
#'   geom_errorbarh(aes(xmax = resp + se, xmin = resp - se))
#' p + geom_point() +
#'   geom_errorbarh(aes(xmax = resp + se, xmin = resp - se, height = .2))
geom_errorbarh <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) {
  GeomErrorbarh$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomErrorbarh <- proto(Geom, {
  objname <- "errorbarh"

  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour = "black", size=0.5, linetype=1, height=0.5, alpha = NA)
  guide_geom <- function(.) "path"
  required_aes <- c("x", "xmin", "xmax", "y")

  reparameterise <- function(., df, params) {
    df$height <- df$height %||%
      params$height %||% (resolution(df$y, FALSE) * 0.9)

    transform(df,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  }

  draw <- function(., data, scales, coordinates, height = NULL, ...) {
    GeomPath$draw(with(data, data.frame(
      x = as.vector(rbind(xmax, xmax, NA, xmax, xmin, NA, xmin, xmin)),
      y = as.vector(rbind(ymin, ymax, NA, y,    y,    NA, ymin, ymax)),
      colour = rep(colour, each = 8),
      alpha = rep(alpha, each = 8),
      size = rep(size, each = 8),
      linetype = rep(linetype, each = 8),
      group = rep(1:(nrow(data)), each = 8),
      stringsAsFactors = FALSE,
      row.names = 1:(nrow(data) * 8)
    )), scales, coordinates, ...)
  }

})
