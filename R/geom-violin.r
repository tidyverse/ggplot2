#' Violin plot.
#'
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "violin")}
#'
#' @inheritParams geom_point
#' @param trim If \code{TRUE} (default), trim the tails of the violins
#'   to the range of the data. If \code{FALSE}, don't trim the tails.
#' @param scale if "equal" (default), all violins have the same area (to be
#'   precise, they would have the same area if tails are not trimmed). If
#'   "count", the areas are scaled proportionally to the number of observations.
#' @export
#' @examples
#' \donttest{
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
#' # Scale maximum width proportional to sample size:
#' p + geom_violin(scale = "count")
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
#' library(plyr) # to access round_any
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
#' }
geom_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "dodge",
trim = TRUE, scale = "equal", ...) {
  GeomViolin$new(mapping = mapping, data = data, stat = stat, 
  position = position, trim = trim, scale = scale, ...)
}

GeomViolin <- gg(proto(Geom, {
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
  
  draw <- function(., data, ...) { 

    # Find the points for the line to go all the way around
    data <- transform(data, xminv = x - scaled * (x-xmin),
                            xmaxv = x + scaled * (xmax-x))

    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(arrange(transform(data, x = xminv), y),
                     arrange(transform(data, x = xmaxv), -y))

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])

    ggname(.$my_name(), GeomPolygon$draw(newdata, ...))
  }

  guide_geom <- function(.) "polygon"

  default_stat <- function(.) StatYdensity
  default_pos <- function(.) PositionDodge
  default_aes <- function(.) aes(weight=1, colour="grey20", fill="white", size=0.5, alpha = NA, linetype = "solid")
  required_aes <- c("x", "y")

}))
