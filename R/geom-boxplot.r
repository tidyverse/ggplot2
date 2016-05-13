#' Box and whiskers plot.
#'
#' The lower and upper "hinges" correspond to the first and third quartiles
#' (the 25th and 75th percentiles). This differs slightly from the method used
#' by the \code{boxplot} function, and may be apparent with small samples.
#' See \code{\link{boxplot.stats}} for for more information on how hinge
#' positions are calculated for \code{boxplot}.
#'
#' The upper whisker extends from the hinge to the highest value that is within
#' 1.5 * IQR of the hinge, where IQR is the inter-quartile range, or distance
#' between the first and third quartiles. The lower whisker extends from the
#' hinge to the lowest value within 1.5 * IQR of the hinge. Data beyond the
#' end of the whiskers are outliers and plotted as points (as specified by Tukey).
#'
#' In a notched box plot, the notches extend \code{1.58 * IQR / sqrt(n)}.
#' This gives a roughly 95% confidence interval for comparing medians.
#' See McGill et al. (1978) for more details.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "boxplot")}
#'
#' @seealso \code{\link{stat_quantile}} to view quantiles conditioned on a
#'   continuous variable, \code{\link{geom_jitter}} for another way to look
#'   at conditional distributions.
#' @inheritParams layer
#' @inheritParams geom_point
#' @param geom,stat Use to override the default connection between
#'   \code{geom_boxplot} and \code{stat_boxplot}.
#' @param outlier.colour,outlier.color,outlier.shape,outlier.size,outlier.stroke
#'   Default aesthetics for outliers. Set to \code{NULL} to inherit from the
#'   aesthetics used for the box.
#'
#'   In the unlikely event you specify both US and UK spellings of colour, the
#'   US spelling will take precedence.
#' @param notch if \code{FALSE} (default) make a standard box plot. If
#'   \code{TRUE}, make a notched box plot. Notches are used to compare groups;
#'   if the notches of two boxes do not overlap, this suggests that the medians
#'   are significantly different.
#' @param notchwidth for a notched box plot, width of the notch relative to
#'   the body (default 0.5)
#' @param varwidth if \code{FALSE} (default) make a standard box plot. If
#'   \code{TRUE}, boxes are drawn with widths proportional to the
#'   square-roots of the number of observations in the groups (possibly
#'   weighted, using the \code{weight} aesthetic).
#' @export
#' @references McGill, R., Tukey, J. W. and Larsen, W. A. (1978) Variations of
#'     box plots. The American Statistician 32, 12-16.
#' @examples
#' p <- ggplot(mpg, aes(class, hwy))
#' p + geom_boxplot()
#' p + geom_boxplot() + geom_jitter(width = 0.2)
#' p + geom_boxplot() + coord_flip()
#'
#' p + geom_boxplot(notch = TRUE)
#' p + geom_boxplot(varwidth = TRUE)
#' p + geom_boxplot(fill = "white", colour = "#3366FF")
#' # By default, outlier points match the colour of the box. Use
#' # outlier.colour to override
#' p + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
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
#'
#' \donttest{
#' # It's possible to draw a boxplot with your own computations if you
#' # use stat = "identity":
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
                         stat = "boxplot", position = "dodge",
                         ...,
                         outlier.colour = NULL,
                         outlier.color = NULL,
                         outlier.shape = 19,
                         outlier.size = 1.5,
                         outlier.stroke = 0.5,
                         notch = FALSE,
                         notchwidth = 0.5,
                         varwidth = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBoxplot <- ggproto("GeomBoxplot", Geom,
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

    if (!is.null(data$outliers)) {
      suppressWarnings({
        out_min <- vapply(data$outliers, min, numeric(1))
        out_max <- vapply(data$outliers, max, numeric(1))
      })

      data$ymin_final <- pmin(out_min, data$ymin)
      data$ymax_final <- pmax(out_max, data$ymax)
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

    data
  },

  draw_group = function(data, panel_scales, coord, fatten = 2,
                        outlier.colour = NULL, outlier.shape = 19,
                        outlier.size = 1.5, outlier.stroke = 0.5,
                        notch = FALSE, notchwidth = 0.5, varwidth = FALSE) {

    common <- data.frame(
      colour = data$colour,
      size = data$size,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      stringsAsFactors = FALSE
    )

    whiskers <- data.frame(
      x = data$x,
      xend = data$x,
      y = c(data$upper, data$lower),
      yend = c(data$ymax, data$ymin),
      alpha = NA,
      common,
      stringsAsFactors = FALSE
    )

    box <- data.frame(
      xmin = data$xmin,
      xmax = data$xmax,
      ymin = data$lower,
      y = data$middle,
      ymax = data$upper,
      ynotchlower = ifelse(notch, data$notchlower, NA),
      ynotchupper = ifelse(notch, data$notchupper, NA),
      notchwidth = notchwidth,
      alpha = data$alpha,
      common,
      stringsAsFactors = FALSE
    )

    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
      outliers <- data.frame(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier.colour %||% data$colour[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = NA,
        alpha = NA,
        stringsAsFactors = FALSE
      )
      outliers_grob <- GeomPoint$draw_panel(outliers, panel_scales, coord)
    } else {
      outliers_grob <- NULL
    }

    ggname("geom_boxplot", grobTree(
      outliers_grob,
      GeomSegment$draw_panel(whiskers, panel_scales, coord),
      GeomCrossbar$draw_panel(box, fatten = fatten, panel_scales, coord)
    ))
  },

  draw_key = draw_key_boxplot,

  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
    alpha = NA, shape = 19, linetype = "solid"),

  required_aes = c("x", "lower", "upper", "middle", "ymin", "ymax")
)
