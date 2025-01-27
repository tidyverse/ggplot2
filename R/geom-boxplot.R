#' A box and whiskers plot (in the style of Tukey)
#'
#' The boxplot compactly displays the distribution of a continuous variable.
#' It visualises five summary statistics (the median, two hinges
#' and two whiskers), and all "outlying" points individually.
#'
#' @eval rd_orientation()
#'
#' @section Summary statistics:
#' The lower and upper hinges correspond to the first and third quartiles
#' (the 25th and 75th percentiles). This differs slightly from the method used
#' by the [boxplot()] function, and may be apparent with small samples.
#' See [boxplot.stats()] for more information on how hinge
#' positions are calculated for [boxplot()].
#'
#' The upper whisker extends from the hinge to the largest value no further than
#' 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance
#' between the first and third quartiles). The lower whisker extends from the
#' hinge to the smallest value at most 1.5 * IQR of the hinge. Data beyond the
#' end of the whiskers are called "outlying" points and are plotted
#' individually.
#'
#' In a notched box plot, the notches extend `1.58 * IQR / sqrt(n)`.
#' This gives a roughly 95% confidence interval for comparing medians.
#' See McGill et al. (1978) for more details.
#'
#' @eval rd_aesthetics("geom", "boxplot")
#'
#' @seealso [geom_quantile()] for continuous `x`,
#'   [geom_violin()] for a richer display of the distribution, and
#'   [geom_jitter()] for a useful technique for small data.
#' @inheritParams layer
#' @inheritParams geom_bar
#' @param geom,stat Use to override the default connection between
#'   `geom_boxplot()` and `stat_boxplot()`. For more information about
#'   overriding these connections, see how the [stat][layer_stats] and
#'   [geom][layer_geoms] arguments work.
#' @param outliers Whether to display (`TRUE`) or discard (`FALSE`) outliers
#'   from the plot. Hiding or discarding outliers can be useful when, for
#'   example, raw data points need to be displayed on top of the boxplot.
#'   By discarding outliers, the axis limits will adapt to the box and whiskers
#'   only, not the full data range. If outliers need to be hidden and the axes
#'   needs to show the full data range, please use `outlier.shape = NA` instead.
#' @param outlier.colour,outlier.color,outlier.fill,outlier.shape,outlier.size,outlier.stroke,outlier.alpha
#'   Default aesthetics for outliers. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param whisker.colour,whisker.color,whisker.linetype,whisker.linewidth
#'   Default aesthetics for the whiskers. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param median.colour,median.color,median.linetype,median.linewidth
#'   Default aesthetics for the median line. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param staple.colour,staple.color,staple.linetype,staple.linewidth
#'   Default aesthetics for the staples. Set to `NULL` to inherit from the
#'   data's aesthetics. Note that staples don't appear unless the `staplewidth`
#'   argument is set to a non-zero size.
#' @param box.colour,box.color,box.linetype,box.linewidth
#'   Default aesthetics for the boxes. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param notch If `FALSE` (default) make a standard box plot. If
#'   `TRUE`, make a notched box plot. Notches are used to compare groups;
#'   if the notches of two boxes do not overlap, this suggests that the medians
#'   are significantly different.
#' @param notchwidth For a notched box plot, width of the notch relative to
#'   the body (defaults to `notchwidth = 0.5`).
#' @param staplewidth The relative width of staples to the width of the box.
#'   Staples mark the ends of the whiskers with a line.
#' @param varwidth If `FALSE` (default) make a standard box plot. If
#'   `TRUE`, boxes are drawn with widths proportional to the
#'   square-roots of the number of observations in the groups (possibly
#'   weighted, using the `weight` aesthetic).
#' @note In the unlikely event you specify both US and UK spellings of colour,
#'   the US spelling will take precedence.
#'
#' @export
#' @references McGill, R., Tukey, J. W. and Larsen, W. A. (1978) Variations of
#'     box plots. The American Statistician 32, 12-16.
#' @examples
#' p <- ggplot(mpg, aes(class, hwy))
#' p + geom_boxplot()
#' # Orientation follows the discrete axis
#' ggplot(mpg, aes(hwy, class)) + geom_boxplot()
#'
#' p + geom_boxplot(notch = TRUE)
#' p + geom_boxplot(varwidth = TRUE)
#' p + geom_boxplot(fill = "white", colour = "#3366FF")
#' # By default, outlier points match the colour of the box. Use
#' # outlier.colour to override
#' p + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
#' # Remove outliers when overlaying boxplot with original data points
#' p + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2)
#'
#' # Boxplots are automatically dodged when any aesthetic is a factor
#' p + geom_boxplot(aes(colour = drv))
#'
#' # You can also use boxplots with continuous x, as long as you supply
#' # a grouping variable. cut_width is particularly useful
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_boxplot()
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_boxplot(aes(group = cut_width(carat, 0.25)))
#' # Adjust the transparency of outliers using outlier.alpha
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_boxplot(aes(group = cut_width(carat, 0.25)), outlier.alpha = 0.1)
#'
#' \donttest{
#' # It's possible to draw a boxplot with your own computations if you
#' # use stat = "identity":
#' set.seed(1)
#' y <- rnorm(100)
#' df <- data.frame(
#'   x = 1,
#'   y0 = min(y),
#'   y25 = quantile(y, 0.25),
#'   y50 = median(y),
#'   y75 = quantile(y, 0.75),
#'   y100 = max(y)
#' )
#' ggplot(df, aes(x)) +
#'   geom_boxplot(
#'    aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
#'    stat = "identity"
#'  )
#' }
geom_boxplot <- function(mapping = NULL, data = NULL,
                         stat = "boxplot", position = "dodge2",
                         ...,
                         outliers = TRUE,
                         outlier.colour = NULL,
                         outlier.color = NULL,
                         outlier.fill = NULL,
                         outlier.shape = NULL,
                         outlier.size = NULL,
                         outlier.stroke = 0.5,
                         outlier.alpha = NULL,
                         whisker.colour    = NULL,
                         whisker.color     = NULL,
                         whisker.linetype  = NULL,
                         whisker.linewidth = NULL,
                         staple.colour     = NULL,
                         staple.color      = NULL,
                         staple.linetype   = NULL,
                         staple.linewidth  = NULL,
                         median.colour     = NULL,
                         median.color      = NULL,
                         median.linetype   = NULL,
                         median.linewidth  = NULL,
                         box.colour        = NULL,
                         box.color         = NULL,
                         box.linetype      = NULL,
                         box.linewidth     = NULL,
                         notch = FALSE,
                         notchwidth = 0.5,
                         staplewidth = 0,
                         varwidth = FALSE,
                         na.rm = FALSE,
                         orientation = NA,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  # varwidth = TRUE is not compatible with preserve = "total"
  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      cli::cli_warn("Can't preserve total widths when {.code varwidth = TRUE}.")
      position$preserve <- "single"
    }
  }

  outlier_gp <- list(
    colour = outlier.color %||% outlier.colour,
    fill   = outlier.fill,
    shape  = outlier.shape,
    size   = outlier.size,
    stroke = outlier.stroke,
    alpha  = outlier.alpha
  )

  whisker_gp <- list(
    colour    = whisker.color %||% whisker.colour,
    linetype  = whisker.linetype,
    linewidth = whisker.linewidth
  )

  staple_gp <- list(
    colour    = staple.color %||% staple.colour,
    linetype  = staple.linetype,
    linewidth = staple.linewidth
  )

  median_gp <- list(
    colour    = median.color %||% median.colour,
    linetype  = median.linetype,
    linewidth = median.linewidth
  )

  box_gp <- list(
    colour    = box.color %||% box.colour,
    linetype  = box.linetype,
    linewidth = box.linewidth
  )

  check_number_decimal(staplewidth)
  check_bool(outliers)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      outliers = outliers,
      outlier_gp = outlier_gp,
      whisker_gp = whisker_gp,
      staple_gp  = staple_gp,
      median_gp  = median_gp,
      box_gp     = box_gp,
      notch = notch,
      notchwidth = notchwidth,
      staplewidth = staplewidth,
      varwidth = varwidth,
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
GeomBoxplot <- ggproto("GeomBoxplot", Geom,

  extra_params = c("na.rm", "orientation", "outliers"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },

  setup_data = function(self, data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- compute_data_size(
      data, params$width,
      default = self$default_aes$width,
      zero = FALSE, discrete = TRUE
    )
    if (isFALSE(params$outliers)) {
      data$outliers <- NULL
    }

    if (!is.null(data$outliers)) {
      suppressWarnings({
        out_min <- vapply(data$outliers, min, numeric(1))
        out_max <- vapply(data$outliers, max, numeric(1))
      })

      data$ymin_final  <- pmin(out_min, data$ymin)
      data$ymax_final  <- pmax(out_max, data$ymax)
    }

    # if `varwidth` not requested or not available, don't use it
    if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(data$relvarwidth)) {
      data$xmin <- data$x - data$width / 2
      data$xmax <- data$x + data$width / 2
    } else {
      # make `relvarwidth` relative to the size of the largest group
      data$relvarwidth <- data$relvarwidth / max(data$relvarwidth)
      data$xmin <- data$x - data$relvarwidth * data$width / 2
      data$xmax <- data$x + data$relvarwidth * data$width / 2
    }
    data$width <- NULL
    if (!is.null(data$relvarwidth)) data$relvarwidth <- NULL

    flip_data(data, params$flipped_aes)
  },

  draw_group = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", fatten = 2, outlier_gp = NULL,
                        whisker_gp = NULL, staple_gp = NULL, median_gp = NULL,
                        box_gp = NULL, notch = FALSE, notchwidth = 0.5,
                        staplewidth = 0, varwidth = FALSE, flipped_aes = FALSE) {
    data <- fix_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)
    # this may occur when using geom_boxplot(stat = "identity")
    if (nrow(data) != 1) {
      cli::cli_abort(c(
        "Can only draw one boxplot per group.",
        "i"= "Did you forget {.code aes(group = ...)}?"
      ))
    }

    common <- list(fill = fill_alpha(data$fill, data$alpha), group = data$group)

    whiskers <- data_frame0(
      x = c(data$x, data$x),
      xend = c(data$x, data$x),
      y = c(data$upper, data$lower),
      yend = c(data$ymax, data$ymin),
      colour    = rep(whisker_gp$colour    %||% data$colour,    2),
      linetype  = rep(whisker_gp$linetype  %||% data$linetype,  2),
      linewidth = rep(whisker_gp$linewidth %||% data$linewidth, 2),
      alpha = c(NA_real_, NA_real_),
      !!!common,
      .size = 2
    )
    whiskers <- flip_data(whiskers, flipped_aes)

    box <- transform(
      data,
      y = middle,
      ymax = upper,
      ymin = lower,
      ynotchlower = ifelse(notch, notchlower, NA),
      ynotchupper = ifelse(notch, notchupper, NA),
      notchwidth = notchwidth
    )
    box <- flip_data(box, flipped_aes)

    if (!is.null(data$outliers) && length(data$outliers[[1]]) >= 1) {
      outliers <- data_frame0(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier_gp$colour %||% data$colour[1],
        fill   = outlier_gp$fill   %||% data$fill[1],
        shape  = outlier_gp$shape  %||% data$shape[1]  %||% 19,
        size   = outlier_gp$size   %||% data$size[1]   %||% 1.5,
        stroke = outlier_gp$stroke %||% data$stroke[1] %||% 0.5,
        fill = NA,
        alpha = outlier_gp$alpha %||% data$alpha[1],
        .size = length(data$outliers[[1]])
      )
      outliers <- flip_data(outliers, flipped_aes)

      outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    } else {
      outliers_grob <- NULL
    }

    if (staplewidth != 0) {
      staples <- data_frame0(
        x    = rep((data$xmin - data$x) * staplewidth + data$x, 2),
        xend = rep((data$xmax - data$x) * staplewidth + data$x, 2),
        y    = c(data$ymax, data$ymin),
        yend = c(data$ymax, data$ymin),
        linetype  = rep(staple_gp$linetype  %||% data$linetype, 2),
        linewidth = rep(staple_gp$linewidth %||% data$linewidth, 2),
        colour    = rep(staple_gp$colour    %||% data$colour, 2),
        alpha = c(NA_real_, NA_real_),
        !!!common,
        .size = 2
      )
      staples <- flip_data(staples, flipped_aes)
      staple_grob <- GeomSegment$draw_panel(
        staples, panel_params, coord,
        lineend = lineend
      )
    } else {
      staple_grob <- NULL
    }

    ggname("geom_boxplot", grobTree(
      outliers_grob,
      staple_grob,
      GeomSegment$draw_panel(whiskers, panel_params, coord, lineend = lineend),
      GeomCrossbar$draw_panel(
        box,
        fatten = fatten,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin,
        flipped_aes = flipped_aes,
        middle_gp = median_gp,
        box_gp = box_gp
      )
    ))
  },

  draw_key = draw_key_boxplot,

  default_aes = aes(
    weight = 1, colour = from_theme(col_mix(ink, paper, 0.2)),
    fill = from_theme(paper), size = from_theme(pointsize),
    alpha = NA, shape = from_theme(pointshape), linetype = from_theme(bordertype),
    linewidth = from_theme(borderwidth),
    width = 0.9
  ),

  required_aes = c("x|y", "lower|xlower", "upper|xupper", "middle|xmiddle", "ymin|xmin", "ymax|xmax"),

  rename_size = TRUE
)
