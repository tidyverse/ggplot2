#' Violin plot.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "violin")}
#'
#' @inheritParams geom_point
#' @param trim If \code{TRUE} (default), trim the tails of the violins
#'   to the range of the data. If \code{FALSE}, don't trim the tails.
#' @param scale if "area" (default), all violins have the same area (before trimming
#'   the tails). If "count", areas are scaled proportionally to the number of
#'   observations. If "width", all violins have the same maximum width.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(factor(cyl), mpg))
#' p + geom_violin()
#'
#' \donttest{
#' p + geom_violin() + geom_jitter(height = 0)
#' p + geom_violin() + coord_flip()
#'
#' # Scale maximum width proportional to sample size:
#' p + geom_violin(scale = "count")
#'
#' # Scale maximum width to 1 for all violins:
#' p + geom_violin(scale = "width")
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
#' # To make horizontal violin plots, use the `h` variant:
#' m + geom_violinh(aes(x = votes, y = rating)) +
#'   scale_x_log10() + coord_trans(x = "log10")
#'
#' # Violin plots with continuous x:
#' # Use the group aesthetic to group observations in violins
#' ggplot(movies, aes(year, budget)) + geom_violin()
#' ggplot(movies, aes(year, budget)) +
#'   geom_violin(aes(group = round_any(year, 10, floor)))
#' }
geom_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "dodge",
trim = TRUE, scale = "area", ...) {
  GeomViolin$new(mapping = mapping, data = data, stat = stat,
    position = position, trim = trim, scale = scale, ...)
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

  draw <- function(., data, ...) {
    # Find the points for the line to go all the way around
    data <- transform(data, xminv = x - violinwidth * (x-xmin),
                            xmaxv = x + violinwidth * (xmax-x))

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

})


#' @rdname geom_violin
#' @export
geom_violinh <- function (mapping = NULL, data = NULL, stat = "xdensity",
                          position = "dodgeh", trim = TRUE,
                          scale = "area", ...) {
  GeomViolinh$new(mapping = mapping, data = data, stat = stat,
    position = position, trim = trim, scale = scale, ...)
}

GeomViolinh <- proto(Geom, {
  objname <- "violinh"

  reparameterise <- function(., df, params) {
    df$height <- df$height %||%
      params$height %||% (resolution(df$y, FALSE) * 0.9)

    # xmin, xmax, ymin, and ymax define the bounding rectangle for each group
    ddply(df, .(group), transform,
      xmin = min(x),
      xmax = max(x),
      ymin = y - height / 2,
      ymax = y + height / 2
    )

  }

  draw <- function(., data, ...) {

    # Find the points for the line to go all the way around
    data <- transform(data,
      yminv = y - violinheight * (y-ymin),
      ymaxv = y + violinheight * (ymax-y)
    )

    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(
      arrange(transform(data, y = yminv), x),
      arrange(transform(data, y = ymaxv), -x)
    )

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])

    ggname(.$my_name(), GeomPolygon$draw(newdata, ...))
  }

  guide_geom <- function(.) "polygon"

  default_stat <- function(.) StatXdensity
  default_pos <- function(.) PositionDodgeh
  default_aes <- function(.) {
    aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
      alpha = NA, linetype = "solid")
  }
  required_aes <- c("x", "y")

})
