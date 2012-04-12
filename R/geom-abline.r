#' Line specified by slope and intercept.
#' 
#' The abline geom adds lines with a slope and intercept (see
#' \code{\link{geom_vline}} and \code{\link{geom_abline}} for other types of
#' lines).
#' 
#' To specify the slope and intercept of the line with fixed values, use
#' \code{annotate_vline}. With this method, the lines with be the same in
#' every panel.
#' To map variables from the data to the slope and y-intercept,
#' use \code{geom_abline}. With this method, the slope and intercept can vary
#' from panel to panel. See the examples for more ideas.
#'
#' @seealso
#'  \code{\link{stat_smooth}} to add lines derived from the data,
#'  \code{\link{geom_hline}} for horizontal lines,
#'  \code{\link{geom_vline}} for vertical lines
#'  \code{\link{geom_segment}}
#' @param show_guide should a legend be drawn? (defaults to \code{FALSE})
#' @inheritParams geom_point
#' @export
#' @examples
#' p <- qplot(wt, mpg, data = mtcars)
#'
#' # Specify a fixed slope and intercept
#' p + annotate_abline(intercept = 20, slope = 1)
#' p + annotate_abline(intercept = 20, slope = 1, colour = "red", size = 2)
#'
#' # Calculate slope and intercept of line of best fit
#' coef(lm(mpg ~ wt, data = mtcars))
#' p + annotate_abline(intercept = 37, slope = -5)
#' 
#' # See ?stat_smooth for fitting smooth models to data
#' p + stat_smooth(method = "lm", se=FALSE)
#' 
#' # Mapping data to slopes and intercepts
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' df <- data.frame(a = rnorm(10, 25), b = rnorm(10, 0))
#' p + geom_abline(data = df, aes(intercept = a, slope = b),
#'   inherit.aes = FALSE)
#'
#' # Slopes and intercepts from linear model
#' library(plyr)
#' coefs <- ddply(mtcars, .(cyl), function(df) { 
#'   m <- lm(mpg ~ wt, data=df)
#'   data.frame(a = coef(m)[1], b = coef(m)[2]) 
#' })
#' str(coefs)
#' p + geom_abline(data = coefs, aes(intercept = a, slope = b),
#'   inherit.aes = FALSE)
#' 
#' # It's actually a bit easier to do this with stat_smooth
#' p + geom_smooth(aes(group = cyl), method = "lm", se = FALSE)
#' p + geom_smooth(aes(group = cyl), method = "lm", se = FALSE, fullrange = TRUE)
#' 
#' # With coordinate transforms
#' p + annotate_abline(intercept = 37, slope = -5) + coord_flip()
#' p + annotate_abline(intercept = 37, slope = -5) + coord_polar()
geom_abline <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", show_guide = FALSE, ...) { 
  GeomAbline$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

GeomAbline <- proto(Geom, {
  objname <- "abline"

  draw <- function(., data, scales, coordinates, intercept = NULL, slope = NULL, ...) {
    ranges <- coord_range(coordinates, scales)

    if (!is.null(intercept))
      data$intercept <- intercept
    if (!is.null(slope))
      data$slope <- slope

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- ranges$x[1] * data$slope + data$intercept
    data$yend <- ranges$x[2] * data$slope + data$intercept

    GeomSegment$draw(data, scales, coordinates)
  }

  icon <- function(.) linesGrob(c(0, 1), c(0.2, 0.8))
  guide_geom <- function(.) "abline"

  default_stat <- function(.) StatAbline
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  
  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data, 
      ggname(.$my_name(), segmentsGrob(0, 0, 1, 1, default.units="npc",
      gp=gpar(col=alpha(colour, alpha), lwd=size * .pt, lty=linetype,
        lineend="butt")))
    )
  }
})
