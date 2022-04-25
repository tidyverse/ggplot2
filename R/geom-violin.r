#' Violin plot
#'
#' A violin plot is a compact display of a continuous distribution. It is a
#' blend of [geom_boxplot()] and [geom_density()]: a
#' violin plot is a mirrored density plot displayed in the same way as a
#' boxplot.
#'
#' @eval rd_orientation()
#'
#' @eval rd_aesthetics("geom", "violin")
#' @inheritParams layer
#' @inheritParams geom_bar
#' @param draw_quantiles If `not(NULL)` (default), draw horizontal lines
#'   at the given quantiles of the density estimate.
#' @param trim If `TRUE` (default), trim the tails of the violins
#'   to the range of the data. If `FALSE`, don't trim the tails.
#' @param geom,stat Use to override the default connection between
#'   `geom_violin()` and `stat_ydensity()`.
#' @export
#' @references Hintze, J. L., Nelson, R. D. (1998) Violin Plots: A Box
#' Plot-Density Trace Synergism. The American Statistician 52, 181-184.
#' @examples
#' p <- ggplot(mtcars, aes(factor(cyl), mpg))
#' p + geom_violin()
#'
#' # Orientation follows the discrete axis
#' ggplot(mtcars, aes(mpg, factor(cyl))) +
#'   geom_violin()
#'
#' \donttest{
#' p + geom_violin() + geom_jitter(height = 0, width = 0.1)
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
#' # Show quartiles
#' p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
#'
#' # Scales vs. coordinate transforms -------
#' if (require("ggplot2movies")) {
#' # Scale transformations occur before the density statistics are computed.
#' # Coordinate transformations occur afterwards.  Observe the effect on the
#' # number of outliers.
#' m <- ggplot(movies, aes(y = votes, x = rating, group = cut_width(rating, 0.5)))
#' m + geom_violin()
#' m +
#'   geom_violin() +
#'   scale_y_log10()
#' m +
#'   geom_violin() +
#'   coord_trans(y = "log10")
#' m +
#'   geom_violin() +
#'   scale_y_log10() + coord_trans(y = "log10")
#'
#' # Violin plots with continuous x:
#' # Use the group aesthetic to group observations in violins
#' ggplot(movies, aes(year, budget)) +
#'   geom_violin()
#' ggplot(movies, aes(year, budget)) +
#'   geom_violin(aes(group = cut_width(year, 10)), scale = "width")
#' }
#' }
geom_violin <- function(mapping = NULL, data = NULL,
                        stat = "ydensity", position = "dodge",
                        ...,
                        draw_quantiles = NULL,
                        trim = TRUE,
                        scale = "area",
                        na.rm = FALSE,
                        orientation = NA,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      trim = trim,
      scale = scale,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomViolin <- ggproto("GeomViolin", Geom,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  extra_params = c("na.rm", "orientation", "lineend", "linejoin", "linemitre"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    data <- dapply(data, "group", transform,
      xmin = x - width / 2,
      xmax = x + width / 2
    )
    flip_data(data, params$flipped_aes)
  },

  draw_group = function(self, data, ..., draw_quantiles = NULL, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    # Find the points for the line to go all the way around
    data <- transform(data,
      xminv = x - violinwidth * (x - xmin),
      xmaxv = x + violinwidth * (xmax - x)
    )

    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(
      transform(data, x = xminv)[order(data$y), ],
      transform(data, x = xmaxv)[order(data$y, decreasing = TRUE), ]
    )

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])
    newdata <- flip_data(newdata, flipped_aes)

    # Draw quantiles if requested, so long as there is non-zero y range
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      if (!(all(draw_quantiles >= 0) && all(draw_quantiles <= 1))) {
        cli::cli_abort("{.arg draw_quantiles} must be between 0 and 1")
      }

      # Compute the quantile segments and combine with existing aesthetics
      quantiles <- create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[
        rep(1, nrow(quantiles)),
        setdiff(names(data), c("x", "y", "group")),
        drop = FALSE
      ]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      both <- both[!is.na(both$group), , drop = FALSE]
      both <- flip_data(both, flipped_aes)
      quantile_grob <- if (nrow(both) == 0) {
        zeroGrob()
      } else {
        GeomPath$draw_panel(both, ...)
      }

      ggname("geom_violin", grobTree(
        GeomPolygon$draw_panel(newdata, ...),
        quantile_grob)
      )
    } else {
      ggname("geom_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  },

  draw_key = draw_key_polygon,

  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
    alpha = NA, linetype = "solid"),

  required_aes = c("x", "y")
)

# Returns a data.frame with info needed to draw quantile segments.
create_quantile_segment_frame <- function(data, draw_quantiles) {
  dens <- cumsum(data$density) / sum(data$density)
  ecdf <- stats::approxfun(dens, data$y, ties = "ordered")
  ys <- ecdf(draw_quantiles) # these are all the y-values for quantiles

  # Get the violin bounds for the requested quantiles.
  violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
  violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)

  # We have two rows per segment drawn. Each segment gets its own group.
  new_data_frame(list(
    x = interleave(violin.xminvs, violin.xmaxvs),
    y = rep(ys, each = 2),
    group = rep(ys, each = 2)
  ))
}

