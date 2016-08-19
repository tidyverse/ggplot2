#' Violin plot.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "violin")}
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param draw_quantiles If \code{not(NULL)} (default), draw horizontal lines
#'   at the given quantiles of the density estimate.
#' @param trim If \code{TRUE} (default), trim the tails of the violins
#'   to the range of the data. If \code{FALSE}, don't trim the tails.
#' @param geom,stat Use to override the default connection between
#'   \code{geom_violin} and \code{stat_ydensity}.
#' @param outliers If \code{not(NULL)} (default), consider any data values beyond
#'   \code{outliers * IQR} from the inter-quartile range (\code{IQR}) to be
#'   outliers. Outliers are drawn as points, and are not included in the computation
#'   of either the violin or its quantiles. For compatibility with the
#'   \code{geom_boxplot} notion of outliers, set \code{outliers=1.5}.
#' @param outlier.colour,outlier.color,outlier.shape,outlier.size,outlier.stroke
#'   Default aesthetics for outliers. Set to \code{NULL} to inherit from the
#'   aesthetics used for the violin.
#'
#'   In the unlikely event you specify both US and UK spellings of colour, the
#'   US spelling will take precedence.
#' @export
#' @references Hintze, J. L., Nelson, R. D. (1998) Violin Plots: A Box
#' Plot-Density Trace Synergism. The American Statistician 52, 181-184.
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
#' m + geom_violin() + scale_y_log10()
#' m + geom_violin() + coord_trans(y = "log10")
#' m + geom_violin() + scale_y_log10() + coord_trans(y = "log10")
#'
#' # Violin plots with continuous x:
#' # Use the group aesthetic to group observations in violins
#' ggplot(movies, aes(year, budget)) + geom_violin()
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
                        outliers = NULL,
                        outlier.colour = NULL,
                        outlier.color = NULL,
                        outlier.shape = 19,
                        outlier.size = 1.5,
                        outlier.stroke = 0.5,
                        na.rm = FALSE,
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
    params = list(
      trim = trim,
      scale = scale,
      draw_quantiles = draw_quantiles,
      outliers = outliers,
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomViolin <- ggproto("GeomViolin", Geom,
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    plyr::ddply(data, "group", transform,
      xmin = x - width / 2,
      xmax = x + width / 2
    )
  },

  draw_group = function(self, data, panel_scales, coord, draw_quantiles = NULL,
                        outlier.colour = NULL, outlier.shape = 19,
                        outlier.size = 1.5, outlier.stroke = 0.5) {

    # Extract any outilers
    outlier.data <- subset(data, is.outlier)
    data <- subset(data, !is.outlier) # just the inliers

    # Find the points for the line to go all the way around
    data <- transform(data,
      xminv = x - violinwidth * (x - xmin),
      xmaxv = x + violinwidth * (xmax - x)
    )

    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(
      plyr::arrange(transform(data, x = xminv), y),
      plyr::arrange(transform(data, x = xmaxv), -y)
    )

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])

    # Draw quantiles if requested, so long as there is non-zero y range
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))

      # Compute the quantile segments and combine with existing aesthetics
      quantiles <- create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[
        rep(1, nrow(quantiles)),
        setdiff(names(data), c("x", "y")),
        drop = FALSE
      ]
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, panel_scales, coord)
    } else {
      quantile_grob <- NULL
    }

    if (nrow(outlier.data) > 0) {
      outliers.frame <- data.frame(
        y = outlier.data$y,
        x = outlier.data$x,
        colour = outlier.colour %||% data$colour[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = NA,
        alpha = NA,
        stringsAsFactors = FALSE
      )
      outliers_grob <- GeomPoint$draw_panel(outliers.frame, panel_scales, coord)
    } else {
      outliers_grob <- NULL
    }

    ggname("geom_violin", grobTree(
      GeomPolygon$draw_panel(newdata, panel_scales, coord),
      quantile_grob,
      outliers_grob)
    )
  },

  draw_key = draw_key_polygon,

  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
    alpha = NA, linetype = "solid"),

  required_aes = c("x", "y")
)

# Returns a data.frame with info needed to draw quantile segments.
create_quantile_segment_frame <- function(data, draw_quantiles) {
  dens <- cumsum(data$density) / sum(data$density)
  ecdf <- stats::approxfun(dens, data$y)
  ys <- ecdf(draw_quantiles) # these are all the y-values for quantiles

  # Get the violin bounds for the requested quantiles.
  violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
  violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)

  # We have two rows per segment drawn. Each segment gets its own group.
  data.frame(
    x = interleave(violin.xminvs, violin.xmaxvs),
    y = rep(ys, each = 2),
    group = rep(ys, each = 2)
  )
}

