#' Calculate contours of 3d data.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "contour")}
#'
#' @inheritParams stat_identity
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @return A data frame with additional column:
#'  \item{level}{height of contour}
#' @export
#' @examples
#' \donttest{
#' # Generate data
#' library(reshape2) # for melt
#' volcano3d <- melt(volcano)
#' names(volcano3d) <- c("x", "y", "z")
#'
#' # Basic plot
#' v <- ggplot(volcano3d, aes(x, y, z = z))
#' v + stat_contour()
#'
#' # Setting bins creates evenly spaced contours in the range of the data
#' v + stat_contour(bins = 2)
#' v + stat_contour(bins = 10)
#'
#' # Setting binwidth does the same thing, parameterised by the distance
#' # between contours
#' v + stat_contour(binwidth = 2)
#' v + stat_contour(binwidth = 5)
#' v + stat_contour(binwidth = 10)
#' v + stat_contour(binwidth = 2, size = 0.5, colour = "grey50") +
#'   stat_contour(binwidth = 10, size = 1)
#'
#' # Add aesthetic mappings
#' v + stat_contour(aes(size = ..level..))
#' v + stat_contour(aes(colour = ..level..))
#'
#' # Change scale
#' v + stat_contour(aes(colour = ..level..), size = 2) +
#'   scale_colour_gradient(low = "brown", high = "white")
#'
#' # Set aesthetics to fixed value
#' v + stat_contour(colour = "red")
#' v + stat_contour(size = 2, linetype = 4)
#'
#' # Try different geoms
#' v + stat_contour(geom="polygon", aes(fill=..level..))
#' v + geom_tile(aes(fill = z)) + stat_contour()
#'
#' # Use qplot instead
#' qplot(x, y, z = z, data = volcano3d, geom = "contour")
#' qplot(x, y, z = z, data = volcano3d, stat = "contour", geom = "path")
#' }
stat_contour <- function (mapping = NULL, data = NULL, geom = "path", position = "identity",
na.rm = FALSE, ...) {
  StatContour$new(mapping = mapping, data = data, geom = geom,
  position = position, na.rm = na.rm, ...)
}

StatContour <- proto(Stat, {
  objname <- "contour"

  calculate <- function(., data, scales, bins=NULL, binwidth=NULL, breaks = NULL, complete = FALSE, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm, name = "stat_contour", finite = TRUE)

    # If no parameters set, use pretty bins
    if (is.null(bins) && is.null(binwidth) && is.null(breaks)) {
      breaks <- pretty(range(data$z), 10)
    }
    # If provided, use bins to calculate binwidth
    if (!is.null(bins)) {
      binwidth <- diff(range(data$z)) / bins
    }
    # If necessary, compute breaks from binwidth
    if (is.null(breaks)) {
      breaks <- fullseq(range(data$z), binwidth)
    }

    contour_lines(data, breaks, complete = complete)
  }


  default_geom <- function(.) GeomPath
  default_aes <- function(.) aes(order = ..level..)
  required_aes <- c("x", "y", "z")
})


# v3d <- reshape2::melt(volcano)
# names(v3d) <- c("x", "y", "z")
#
# breaks <- seq(95, 195, length = 10)
# contours <- contour_lines(v3d, breaks)
# qplot(x, y, data = contours, geom = "path") + facet_wrap(~ piece)
contour_lines <- function(data, breaks, complete = FALSE) {
  z <- tapply(data$z, data[c("x", "y")], identity)

  cl <- contourLines(
    x = sort(unique(data$x)), y = sort(unique(data$y)), z = z,
    levels = breaks)

  if (length(cl) == 0) {
    warning("Not possible to generate contour data", call. = FALSE)
    return(data.frame())
  }

  # Convert list of lists into single data frame
  lengths <- vapply(cl, function(x) length(x$x), integer(1))
  levels <- vapply(cl, "[[", "level", FUN.VALUE = double(1))
  xs <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
  pieces <- rep(seq_along(cl), lengths)
  # Add leading zeros so that groups can be properly sorted later
  groups <- paste(data$group[1], sprintf("%03d", pieces), sep = "-")

  data.frame(
    level = rep(levels, lengths),
    x = xs,
    y = ys,
    piece = pieces,
    group = groups
  )
}

# 1 = clockwise, -1 = counterclockwise, 0 = 0 area
# From http://stackoverflow.com/questions/1165647
# x <- c(5, 6, 4, 1, 1)
# y <- c(0, 4, 5, 5, 0)
# poly_dir(x, y)
poly_dir <- function(x, y) {
  xdiff <- c(x[-1], x[1]) - x
  ysum <- c(y[-1], y[1]) + y
  sign(sum(xdiff * ysum))
}

# To fix breaks and complete the polygons, we need to add 0-4 corner points.
#
# contours <- ddply(contours, "piece", mutate, dir = poly_dir(x, y))
# qplot(x, y, data = contours, geom = "path", group = piece,
#   colour = factor(dir))
# last_plot() + facet_wrap(~ level)

