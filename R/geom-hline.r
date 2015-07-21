#' Horizontal line.
#'
#' This geom allows you to annotate the plot with horizontal lines (see
#' \code{\link{geom_vline}} and \code{\link{geom_abline}} for other types of
#' lines).
#'
#' There are two ways to use it. You can either specify the intercept of
#' the line in the call to the geom, in which case the line will be in the
#' same position in every panel. Alternatively, you can supply a different
#' intercept for each panel using a data.frame. See the examples for the
#' differences
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "hline")}
#'
#' @seealso \code{\link{geom_vline}} for vertical lines,
#'  \code{\link{geom_abline}} for lines defined by a slope and intercept,
#'  \code{\link{geom_segment}} for a more general approach
#' @export
#' @inheritParams geom_point
#' @param show_guide should a legend be drawn? (defaults to \code{FALSE})
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y=mpg)) + geom_point()
#'
#' p + geom_hline(aes(yintercept=mpg))
#' p + geom_hline(yintercept=20)
#' p + geom_hline(yintercept=seq(10, 30, by=5))
#'
#' # With coordinate transforms
#' p + geom_hline(aes(yintercept=mpg)) + coord_equal()
#' p + geom_hline(aes(yintercept=mpg)) + coord_flip()
#' p + geom_hline(aes(yintercept=mpg)) + coord_polar()
#'
#' # To display different lines in different facets, you need to
#' # create a data frame.
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'      geom_point() +
#'      facet_wrap(vs ~ am)
#'
#' hline.data <- data.frame(z = 1:4, vs = c(0,0,1,1), am = c(0,1,0,1))
#' p + geom_hline(aes(yintercept = z), hline.data)
geom_hline <- function (mapping = NULL, data = NULL, stat = "hline",
  position = "identity", show_guide = FALSE, ...)
{
  yintercept <- list(...)$yintercept
  if (is.numeric(yintercept)) {
    data <- data.frame(yintercept = yintercept)
    yintercept <- NULL
    mapping <- aes_all(names(data))
  }

  Layer$new(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHline,
    position = position,
    show_guide = show_guide,
    inherit.aes = FALSE,
    params = list(...)
  )
}

GeomHline <- proto2(
  class = "GeomHline",
  inherit = Geom,
  members = list(
    draw = function(self, data, scales, coordinates, ...) {
      ranges <- coord_range(coordinates, scales)

      data$x    <- ranges$x[1]
      data$xend <- ranges$x[2]

      GeomSegment$draw(unique(data), scales, coordinates)
    },

    default_aes = aes(colour="black", size=0.5, linetype=1, alpha = NA),

    guide_geom = function(self) "path"
  )
)
