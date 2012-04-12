#' Horizontal line.
#'
#' This geom allows you to add horizontal lines (see
#' \code{\link{geom_vline}} and \code{\link{geom_abline}} for other types of
#' lines).
#'
#' To map variables from the data to the y-intercept position of the lines,
#' use \code{geom_hline}. To specify the intercept of the line directly,
#' use \code{annotate_hline}. In this case, the lines will be in the
#' same position in every panel.
#'
#' @seealso \code{\link{geom_vline}} for vertical lines, 
#'  \code{\link{geom_abline}} for lines defined by a slope and intercept,
#'  \code{\link{geom_segment}} for a more general approach
#' @export
#' @inheritParams geom_point
#' @param show_guide should a legend be drawn? (defaults to \code{FALSE})
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' 
#' # Map a variable to yintercept
#' p + geom_hline(aes(yintercept = mpg))
#'
#' # Specify yintercept directly
#' p + annotate_hline(yintercept = 20)
#' p + annotate_hline(yintercept = seq(10, 30, by = 5))
#' 
#' # With coordinate transforms
#' p + geom_hline(aes(yintercept = mpg)) + coord_equal()
#' p + geom_hline(aes(yintercept = mpg)) + coord_flip()
#' p + geom_hline(aes(yintercept = mpg)) + coord_polar()
#' 
#' # To display different lines in different facets, you need to 
#' # create a data frame.
#' p <- qplot(mpg, wt, data=mtcars, facets = vs ~ am)
#' 
#' hline.data <- data.frame(z = 1:4, vs = c(0,0,1,1), am = c(0,1,0,1))
#' p + geom_hline(aes(yintercept = z), hline.data, inherit.aes = FALSE)
geom_hline <- function (mapping = NULL, data = NULL, stat = "hline", position = "identity", show_guide = FALSE, ...) { 
  GeomHline$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

GeomHline <- proto(Geom, {
  objname <- "hline"

  draw <- function(., data, scales, coordinates, ...) {
    ranges <- coord_range(coordinates, scales)

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    
    GeomSegment$draw(unique(data), scales, coordinates)
  }

  icon <- function(.) linesGrob(c(0, 1), c(0.5, 0.5))
    
  default_stat <- function(.) StatHline
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
})
