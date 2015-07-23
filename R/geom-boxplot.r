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
#' @inheritParams geom_point
#' @param geom,stat Use to override the default connection between
#'   \code{geom_boxplot} and \code{stat_boxplot}.
#' @param outlier.colour,outlier.shape,outlier.size,outlier.stroke Override
#'   aesthetics used for the outliers. Defaults come from \code{geom_point()}.
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
#' p + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
#'
#' # Boxplots are automatically dodged when any aesthetic is a factor
#' p + geom_boxplot(aes(fill = drv))
#'
#' # You can also use boxplots with continuous x, as long as you supply
#' # a grouping variable. plyr::round_any is particularly useful
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_boxplot()
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_boxplot(aes(group = plyr::round_any(carat, 0.1)))
#'
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
geom_boxplot <- function(mapping = NULL, data = NULL, stat = "boxplot",
  position = "dodge", outlier.colour = "black", outlier.shape = 19,
  outlier.size = 2, outlier.stroke = 1, notch = FALSE, notchwidth = .5,
  varwidth = FALSE, show_guide = NA, inherit.aes = TRUE, ...)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    geom_params = list(
      outlier.colour = outlier.colour,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stoke = outlier.stroke,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth
    ),
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBoxplot <- ggproto("GeomBoxplot", Geom,
  reparameterise = function(df, params) {
    df$width <- df$width %||%
      params$width %||% (resolution(df$x, FALSE) * 0.9)

    if (!is.null(df$outliers)) {
      suppressWarnings({
        out_min <- vapply(df$outliers, min, numeric(1))
        out_max <- vapply(df$outliers, max, numeric(1))
      })

      df$ymin_final <- pmin(out_min, df$ymin)
      df$ymax_final <- pmax(out_max, df$ymax)
    }

    # if `varwidth` not requested or not available, don't use it
    if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(df$relvarwidth)) {
      df$xmin <- df$x - df$width / 2
      df$xmax <- df$x + df$width / 2
    } else {
      # make `relvarwidth` relative to the size of the largest group
      df$relvarwidth <- df$relvarwidth / max(df$relvarwidth)
      df$xmin <- df$x - df$relvarwidth * df$width / 2
      df$xmax <- df$x + df$relvarwidth * df$width / 2
    }
    df$width <- NULL
    if (!is.null(df$relvarwidth)) df$relvarwidth <- NULL

    df
  },

  draw = function(self, data, ..., fatten = 2, outlier.colour = "black", outlier.shape = 19,
                  outlier.size = 2, outlier.stroke = 1,
                  notch = FALSE, notchwidth = .5, varwidth = FALSE) {
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
      common
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
      common
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
      outliers_grob <- GeomPoint$draw(outliers, ...)
    } else {
      outliers_grob <- NULL
    }

    ggname("geom_boxplot", grobTree(
      outliers_grob,
      GeomSegment$draw(whiskers, ...),
      GeomCrossbar$draw(box, fatten = fatten, ...)
    ))
  },

  draw_key = draw_key_boxplot,

  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
    alpha = NA, shape = 19, linetype = "solid", outlier.colour = "black",
    outlier.shape = 19, outlier.size = 2, outlier.stroke = 1),

  required_aes = c("x", "lower", "upper", "middle", "ymin", "ymax")
)
