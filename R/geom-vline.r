#' Line, vertical.
#'
#' This geom allows you to annotate the plot with vertical lines (see
#' \code{\link{geom_hline}} and \code{\link{geom_abline}} for other types of
#' lines.
#'
#' There are two ways to use it.  You can either specify the intercept of the
#' line in the call to the geom, in which case the line will be in the same
#' position in every panel.  Alternatively, you can supply a different
#' intercept for each panel using a data.frame.  See the examples for the
#' differences.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "vline")}
#'
#' @param show_guide should a legend be drawn? (defaults to \code{FALSE})
#' @inheritParams geom_point
#' @seealso
#'  \code{\link{geom_hline}} for horizontal lines,
#'  \code{\link{geom_abline}} for lines defined by a slope and intercept,
#'  \code{\link{geom_segment}} for a more general approach"
#' @export
#' @examples
#' # Fixed lines
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' p + geom_vline(xintercept = 5)
#' p + geom_vline(xintercept = 1:5)
#' p + geom_vline(xintercept = 1:5, colour="green", linetype = "longdash")
#' p + geom_vline(aes(xintercept = wt))
#'
#' # With coordinate transforms
#' p + geom_vline(aes(xintercept = wt)) + coord_equal()
#' p + geom_vline(aes(xintercept = wt)) + coord_flip()
#' p + geom_vline(aes(xintercept = wt)) + coord_polar()
#'
#' p2 <- p + aes(colour = factor(cyl))
#' p2 + geom_vline(xintercept = 15)
#'
#' # To display different lines in different facets, you need to
#' # create a data frame.
#' p <- qplot(mpg, wt, data=mtcars, facets = vs ~ am)
#' vline.data <- data.frame(z = c(15, 20, 25, 30), vs = c(0, 0, 1, 1), am = c(0, 1, 0, 1))
#' p + geom_vline(aes(xintercept = z), vline.data)
geom_vline <- function (mapping = NULL, data = NULL, stat = "vline", position = "identity", show_guide = FALSE, ...) {
  GeomVline$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

GeomVline <- proto(Geom, {
  objname <- "vline"

  new <- function(., data = NULL, mapping = NULL, xintercept = NULL, ...) {
    if (is.numeric(xintercept)) {
      data <- data.frame(xintercept = xintercept)
      xintercept <- NULL
      mapping <- aes_all(names(data))
    }
    .super$new(., data = data, mapping = mapping, inherit.aes = FALSE,
      xintercept = xintercept, ...)
  }

  draw <- function(., data, scales, coordinates, ...) {
    ranges <- coord_range(coordinates, scales)

    data$y    <- ranges$y[1]
    data$yend <- ranges$y[2]

    GeomSegment$draw(unique(data), scales, coordinates)
  }


  default_stat <- function(.) StatVline
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "vline"

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data,
      ggname(.$my_name(), segmentsGrob(0.5, 0, 0.5, 1, default.units="npc",
      gp=gpar(col=alpha(colour, alpha), lwd=size * .pt, lty=linetype, lineend="butt")))
    )
  }

})
