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
#'   `geom_boxplot()` and `stat_boxplot()`.
#' @param outliers Whether to display (`TRUE`) or discard (`FALSE`) outliers
#'   from the plot. Hiding or discarding outliers can be useful when, for
#'   example, raw data points need to be displayed on top of the boxplot.
#'   By discarding outliers, the axis limits will adapt to the box and whiskers
#'   only, not the full data range. If outliers need to be hidden and the axes
#'   needs to show the full data range, please use `outlier.shape = NA` instead.
#' @param outlier.colour,outlier.color,outlier.fill,outlier.shape,outlier.size,outlier.stroke,outlier.alpha
#'   Default aesthetics for outliers. Set to `NULL` to inherit from the
#'   aesthetics used for the box.
#'
#'   In the unlikely event you specify both US and UK spellings of colour, the
#'   US spelling will take precedence.
#'
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
                         outlier.shape = 19,
                         outlier.size = 1.5,
                         outlier.stroke = 0.5,
                         outlier.alpha = NULL,
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
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
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

  # need to declare `width` here in case this geom is used with a stat that
  # doesn't have a `width` parameter (e.g., `stat_identity`).
  extra_params = c("na.rm", "width", "orientation", "outliers"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

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
                        linejoin = "mitre", fatten = 2, outlier.colour = NULL,
                        outlier.fill = NULL, outlier.shape = 19,
                        outlier.size = 1.5, outlier.stroke = 0.5,
                        outlier.alpha = NULL, notch = FALSE, notchwidth = 0.5,
                        staplewidth = 0, varwidth = FALSE, flipped_aes = FALSE) {
    data <- check_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)
    # this may occur when using geom_boxplot(stat = "identity")
    if (nrow(data) != 1) {
      cli::cli_abort(c(
        "Can only draw one boxplot per group",
        "i"= "Did you forget {.code aes(group = ...)}?"
      ))
    }

    common <- list(
      colour = data$colour,
      linewidth = data$linewidth,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group
    )

    whiskers <- data_frame0(
      x = c(data$x, data$x),
      xend = c(data$x, data$x),
      y = c(data$upper, data$lower),
      yend = c(data$ymax, data$ymin),
      alpha = c(NA_real_, NA_real_),
      !!!common,
      .size = 2
    )
    whiskers <- flip_data(whiskers, flipped_aes)

    box <- data_frame0(
      xmin = data$xmin,
      xmax = data$xmax,
      ymin = data$lower,
      y = data$middle,
      ymax = data$upper,
      ynotchlower = ifelse(notch, data$notchlower, NA),
      ynotchupper = ifelse(notch, data$notchupper, NA),
      notchwidth = notchwidth,
      alpha = data$alpha,
      !!!common
    )
    box <- flip_data(box, flipped_aes)

    if (!is.null(data$outliers) && length(data$outliers[[1]]) >= 1) {
      outliers <- data_frame0(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier.colour %||% data$colour[1],
        fill = outlier.fill %||% data$fill[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = NA,
        alpha = outlier.alpha %||% data$alpha[1],
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
        flipped_aes = flipped_aes
      )
    ))
  },

  draw_key = draw_key_boxplot,

  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = NULL,
    alpha = NA, shape = 19, linetype = "solid", linewidth = 0.5),

  required_aes = c("x|y", "lower|xlower", "upper|xupper", "middle|xmiddle", "ymin|xmin", "ymax|xmax"),

  rename_size = TRUE
)
