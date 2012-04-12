#' Line, vertical.
#' 
#' This geom allows you to add vertical lines (see
#' \code{\link{geom_vline}} and \code{\link{geom_abline}} for other types of
#' lines).
#'
#' To specify the intercept of the line directly, use \code{annotate_vline}.
#' If you use this, the lines will be in the same position in each panel.
#'
#' To map variables from the data to the x-intercept position of the lines,
#' use \code{geom_vline}. If you use this the lines can be in different
#' positions in each panel.
#'
#' @param show_guide should a legend be drawn? (defaults to \code{FALSE})
#' @inheritParams geom_point
#' @seealso
#'  \code{\link{geom_hline}} for horizontal lines,
#'  \code{\link{geom_abline}} for lines defined by a slope and intercept,
#'  \code{\link{geom_segment}} for a more general approach
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#'
#' # Map a variable to yintercept
#' p + geom_vline(aes(xintercept = wt))
#'
#' # Specify yintercept directly
#' p + annotate_vline(xintercept = 5)
#' p + annotate_vline(xintercept = 1:5)
#' p + annotate_vline(xintercept = 1:5, colour="green", linetype = "longdash")
#' 
#' # With coordinate transforms
#' p + geom_vline(aes(xintercept = wt)) + coord_equal()
#' p + geom_vline(aes(xintercept = wt)) + coord_flip()
#' p + geom_vline(aes(xintercept = wt)) + coord_polar()
#' 
#' p2 <- p + aes(colour = factor(cyl))
#' p2 + annotate_vline(xintercept = 15)
#'
#' # To display different lines in different facets, you need to 
#' # create a data frame.
#' p <- qplot(mpg, wt, data = mtcars, facets = vs ~ am)
#' vline.data <- data.frame(z = c(15, 20, 25, 30), vs = c(0, 0, 1, 1), am = c(0, 1, 0, 1)) 
#' p + geom_vline(aes(xintercept = z), vline.data, inherit.aes = FALSE)
geom_vline <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", show_guide = FALSE, ...) { 
  GeomVline$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

GeomVline <- proto(Geom, {
  objname <- "vline"

  draw <- function(., data, scales, coordinates, xintercept = NULL, ...) {
    ranges <- coord_range(coordinates, scales)

    data$x    <- xintercept %||% data$xintercept
    data$xend <- data$x
    data$y    <- ranges$y[1]
    data$yend <- ranges$y[2]
    
    GeomSegment$draw(data, scales, coordinates)
  }

  
  icon <- function(.) linesGrob(c(0.5, 0.5), c(0, 1))
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
