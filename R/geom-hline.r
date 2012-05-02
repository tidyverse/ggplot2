#' Horizontal, vertical, and sloped lines.
#'
#' You can either add lines at specified positions with
#' \code{annotate(geom="hline")} (or \code{vline} or \code{abline},
#' or you can use variables from a data frame to specify the positions,
#' using \code{geom_hline}.
#'
#' The \code{annotate} form is useful for adding individual lines to a plot,
#' while the \code{geom} form is useful for drawing lines directly from the
#' data
#'
#' For \code{geom_hline}, specify the y-intercept with \code{yintercept}.
#'
#' For \code{geom_vline}, specify the x-intercept with \code{xintercept}.
#'
#' For \code{geom_abline}, specify the y-intercept with \code{intercept}
#' and the slope with \code{slope}.
#'
#' @param show_guide should a legend be drawn? (defaults to \code{FALSE})
#' @inheritParams geom_point
#' @seealso
#'  \code{\link{annotate}} for adding annotations.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' 
#' # Individual lines
#' p + annotate("hline", yintercept = 20)
#' p + annotate("vline", xintercept = 5, colour = "blue")
#' p + annotate("abline", intercept = 20, slope = 1)
#'
#' # Using vectors to specify lines
#' p + annotate("hline", yintercept = seq(10, 30, by = 5))
#' p + annotate("vline", xintercept = 1:5, colour="darkgreen", linetype = "longdash")
#' p + annotate("abline", intercept = c(17, 22), slope = c(0.5, 1))
#' 
#' # Map a variable to line properties
#' p + geom_hline(aes(yintercept = mpg))
#' p + geom_vline(aes(xintercept = wt), colour = "blue")
#' p + geom_abline(aes(intercept = mpg, slope = wt))
#'
#' # Calculate slope and intercept of line of best fit
#' coef(lm(mpg ~ wt, data = mtcars))
#' p + annotate_abline(intercept = 37, slope = -5)
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
geom_hline <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", show_guide = FALSE, ...) { 
  GeomHline$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

GeomHline <- proto(Geom, {
  objname <- "hline"

  draw <- function(., data, scales, coordinates, yintercept = NULL, ...) {
    ranges <- coord_range(coordinates, scales)

    data$y    <- yintercept %||% data$yintercept
    data$yend <- data$y
    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    
    if(nrow(data) > 1 && nrow(unique(data)) == 1)
      message(nrow(data), " identical hlines were drawn. If you want just one line, use annontate(\"hline\") instead of geom_hline().")

    GeomSegment$draw(data, scales, coordinates)
  }

  icon <- function(.) linesGrob(c(0, 1), c(0.5, 0.5))
    
  default_stat <- function(.) StatHline
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
})
