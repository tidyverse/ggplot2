#' @inheritParams stat_identity
#' @export
#' @section Computed variables:
#' \describe{
#'  \item{level}{height of contour}
#' }
#' @rdname geom_contour
stat_contour <- function(mapping = NULL, data = NULL,
                         geom = "contour", position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatContour,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatContour <- ggproto("StatContour", Stat,
  required_aes = c("x", "y", "z"),
  default_aes = aes(order = ..level..),

  compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                           breaks = NULL, complete = FALSE, na.rm = FALSE) {
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

)


# v3d <- reshape2::melt(volcano)
# names(v3d) <- c("x", "y", "z")
#
# breaks <- seq(95, 195, length.out = 10)
# contours <- contourLines(v3d, breaks)
# ggplot(contours, aes(x, y)) +
#   geom_path() +
#   facet_wrap(~piece)
contour_lines <- function(data, breaks, complete = FALSE) {
  z <- tapply(data$z, data[c("x", "y")], identity)

  cl <- grDevices::contourLines(
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
# contours <- ddply(contours, "piece", mutate, dir = ggplot2:::poly_dir(x, y))
# ggplot(contours, aes(x, y)) +
#   geom_path(aes(group = piece, colour = factor(dir)))
# last_plot() + facet_wrap(~ level)

