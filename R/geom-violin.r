#' Violin plot.
#'
#' @seealso \code{\link{stat_quantile}} to view quantiles conditioned on a
#'   continuous variable,  \code{\link{geom_jitter}} for another way to look 
#'   at conditional distributions"
#' @param trim
#' @param outlier.shape shape of outlying points
#' @param outlier.size size of outlying points
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(factor(cyl), mpg))
#' 
#' p + geom_violin()
#' qplot(factor(cyl), mpg, data = mtcars, geom = "violin")
#' 
#' p + geom_violin() + geom_jitter(height = 0)
#' p + geom_violin() + coord_flip()
#' qplot(factor(cyl), mpg, data = mtcars, geom = "violin") +
#'   coord_flip()
#' 
#' # Default is to scale each violin so that maximum width is the same.
#' # However, each density curve may have a different width before scaling.
#' # To scale each violin relative to the width of the widest:
#' p + geom_violin(fullwidth = FALSE)
#' 
#' # Default is to trim violins to the range of the data. To disable:
#' p + geom_violin(trim = FALSE)
#' 
#' # Use a smaller bandwidth for closer density fit (default is 1).
#' p + geom_violin(adjust = .5)
#' 
#' # Add aesthetic mappings
#' # Note that violins are automatically dodged when any aesthetic is 
#' # a factor
#' p + geom_violin(aes(fill = cyl))
#' p + geom_violin(aes(fill = factor(cyl)))
#' p + geom_violin(aes(fill = factor(vs)))
#' p + geom_violin(aes(fill = factor(am)))
#' 
#' # Set aesthetics to fixed value
#' p + geom_violin(fill = "grey80", colour = "#3366FF")
#' qplot(factor(cyl), mpg, data = mtcars, geom = "violin", 
#'   colour = I("#3366FF"))
#' 
#' # Scales vs. coordinate transforms -------
#' # Scale transformations occur before the density statistics are computed.
#' # Coordinate transformations occur afterwards.  Observe the effect on the
#' # number of outliers.
#' m <- ggplot(movies, aes(y = votes, x = rating,
#'    group = round_any(rating, 0.5)))
#' m + geom_violin()
#' m + geom_violin() + scale_y_log10()
#' m + geom_violin() + coord_trans(y = "log10")
#' m + geom_violin() + scale_y_log10() + coord_trans(y = "log10")
#' 
#' # Violin plots with continuous x:
#' # Use the group aesthetic to group observations in violins
#' qplot(year, budget, data = movies, geom = "violin")
#' qplot(year, budget, data = movies, geom = "violin", 
#'   group = round_any(year, 10, floor))
#'
geom_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "dodge", 
trim = TRUE, fullwidth = TRUE, ...) {
  GeomViolin$new(mapping = mapping, data = data, stat = stat, 
  position = position, trim = trim, fullwidth = fullwidth, ...)
}

GeomViolin <- proto(Geom, {
  objname <- "violin"

  reparameterise <- function(., df, params) {
    df$width <- df$width %||% 
      params$width %||% (resolution(df$x, FALSE) * 0.9)

    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    ddply(df, .(group), transform,
          ymin = min(y),
          ymax = max(y),
          xmin = x - width / 2,
          xmax = x + width / 2)

  }
  
  draw <- function(., data, ..., fatten = 2, outlier.colour = NULL, outlier.shape = NULL, outlier.size = 2) { 

    # Find the points for the line to go all the way around
    data <- transform(data, xminv = x - scaled * (x-xmin),
                            xmaxv = x + scaled * (xmax-x))

    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(arrange(transform(data, x = xminv), y),
                     arrange(transform(data, x = xmaxv), -y))

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])

    ggname(.$my_name(), grobTree(
      GeomPolygon$draw(newdata, ...)
    ))

  }

  guide_geom <- function(.) "violin"
  draw_legend <- function(., data, ...)  {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data, grobTree(
      rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), lty = linetype)),
      linesGrob(gp = gpar(col = colour, lwd = size * .pt, lineend="butt", lty = linetype))
    ))
  }

  icon <- function(.) {
    y <- seq(-.3, .3, length=40)
    x <- dnorm(y, mean=0, sd=0.1)

    y <- c(y,rev(y))
    x <- c(x,-rev(x))/max(6*x)

    linesGrob(x + .25, y + .35, default="npc")
    linesGrob(x + .75, y + .65, default="npc")
  }
  
  default_stat <- function(.) StatYdensity
  default_pos <- function(.) PositionDodge
  default_aes <- function(.) aes(weight=1, colour="grey20", fill="white", size=0.5, alpha = 1, shape = 16, linetype = "solid")
  required_aes <- c("x", "y")

})
