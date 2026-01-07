#' Violin plot
#'
#' A violin plot is a compact display of a continuous distribution. It is a
#' blend of [geom_boxplot()] and [geom_density()]: a
#' violin plot is a mirrored density plot displayed in the same way as a
#' boxplot.
#'
#' @inheritSection shared_layer_parameters Orientation
#'
#' @aesthetics GeomViolin
#' @inheritParams shared_layer_parameters
#' @param trim If `TRUE` (default), trim the tails of the violins
#'   to the range of the data. If `FALSE`, don't trim the tails.
#' @param geom,stat Use to override the default connection between
#'   `geom_violin()` and `stat_ydensity()`. For more information about
#'   overriding these connections, see how the [stat][layer_stats] and
#'   [geom][layer_geoms] arguments work.
#' @param bounds Known lower and upper bounds for estimated data. Default
#'   `c(-Inf, Inf)` means that there are no (finite) bounds. If any bound is
#'   finite, boundary effect of default density estimation will be corrected by
#'   reflecting tails outside `bounds` around their closest edge. Data points
#'   outside of bounds are removed with a warning.
#' @param quantile.colour,quantile.color,quantile.linewidth,quantile.linetype
#'   Default aesthetics for the quantile lines. Set to `NULL` to inherit from
#'   the data's aesthetics. By default, quantile lines are hidden and can be
#'   turned on by changing `quantile.linetype`. Quantile values can be set
#'   using the `quantiles` argument when using `stat = "ydensity"` (default).
#' @param draw_quantiles `r lifecycle::badge("deprecated")` Previous
#'   specification of drawing quantiles.
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
#' p + geom_violin(quantile.linetype = 'solid')
#'
#' # Show different quantiles
#' p + geom_violin(quantiles = c(0.2, 0.4, 0.6, 0.8), quantile.linetype = 'solid')
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
#'   coord_transform(y = "log10")
#' m +
#'   geom_violin() +
#'   scale_y_log10() + coord_transform(y = "log10")
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
                        trim = TRUE,
                        bounds = c(-Inf, Inf),
                        quantile.colour = NULL,
                        quantile.color = NULL,
                        quantile.linetype = 0L,
                        quantile.linewidth = NULL,
                        draw_quantiles = deprecated(),
                        scale = "area",
                        na.rm = FALSE,
                        orientation = NA,
                        show.legend = NA,
                        inherit.aes = TRUE) {

  extra <- list()
  if (lifecycle::is_present(draw_quantiles)) {
    deprecate(
      "4.0.0",
      what = "geom_violin(draw_quantiles)",
      with = "geom_violin(quantile.linetype)",
      details = "Quantiles can be changed with `geom_violin(quantiles)`"
    )
    check_numeric(draw_quantiles)

    # Pass on to stat when stat accepts 'quantiles'
    stat <- validate_subclass(stat, "Stat", current_call(), caller_env())
    if ("quantiles" %in% stat$parameters()) {
      extra$quantiles <- draw_quantiles
    }

    # Turn on quantile lines
    if (!is.null(quantile.linetype)) {
      quantile.linetype <- max(quantile.linetype, 1)
    }
  }

  quantile_gp <- list(
    colour    = quantile.color %||% quantile.colour,
    linetype  = quantile.linetype,
    linewidth = quantile.linewidth
  )

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
      na.rm = na.rm,
      orientation = orientation,
      bounds = bounds,
      quantile_gp = quantile_gp,
      !!!extra,
      ...
    )
  )
}

#' @rdname Geom
#' @format NULL
#' @usage NULL
#' @export
GeomViolin <- ggproto("GeomViolin", Geom,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  extra_params = c("na.rm", "orientation", "lineend", "linejoin", "linemitre"),

  setup_data = function(self, data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- compute_data_size(
      data, params$width,
      default = self$default_aes$width
    )
    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    data <- dapply(data, "group", transform,
      xmin = x - width / 2,
      xmax = x + width / 2
    )
    flip_data(data, params$flipped_aes)
  },

  draw_group = function(self, data, ..., quantile_gp = list(linetype = 0), flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    # Find the points for the line to go all the way around
    data <- transform(data,
      xminv = x - violinwidth * (x - xmin),
      xmaxv = x + violinwidth * (xmax - x)
    )

    # Make sure it's sorted properly to draw the outline
    newdata <- vec_rbind0(
      transform(data, x = xminv)[order(data$y), ],
      transform(data, x = xmaxv)[order(data$y, decreasing = TRUE), ]
    )

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- vec_rbind0(newdata, newdata[1,])
    newdata <- flip_data(newdata, flipped_aes)

    violin_grob <- GeomPolygon$draw_panel(newdata, ...)

    if (!"quantile" %in% names(newdata) ||
        all(quantile_gp$linetype == 0) ||
        all(quantile_gp$linetype == "blank")) {
      return(ggname("geom_violin", violin_grob))
    }

    # Draw quantiles if requested, so long as there is non-zero y range
    quantiles <- newdata[!is.na(newdata$quantile),]
    quantiles$group <- match(quantiles$quantile, unique(quantiles$quantile))
    quantiles$linetype  <- quantile_gp$linetype  %||% quantiles$linetype
    quantiles$linewidth <- quantile_gp$linewidth %||% quantiles$linewidth
    quantiles$colour    <- quantile_gp$colour    %||% quantiles$colour

    quantile_grob <- if (nrow(quantiles) == 0) {
      zeroGrob()
    } else {
      GeomPath$draw_panel(quantiles, ...)
    }

    ggname("geom_violin", grobTree(violin_grob, quantile_grob))
  },

  draw_key = draw_key_polygon,

  default_aes = aes(
    weight = 1,
    colour = from_theme(colour %||% col_mix(ink, paper, 0.2)),
    fill = from_theme(fill %||% paper),
    linewidth = from_theme(borderwidth),
    linetype = from_theme(bordertype),
    alpha = NA,
    width = 0.9
  ),

  required_aes = c("x", "y"),

  rename_size = TRUE
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
  data_frame0(
    x = vec_interleave(violin.xminvs, violin.xmaxvs),
    y = rep(ys, each = 2),
    group = rep(ys, each = 2)
  )
}

