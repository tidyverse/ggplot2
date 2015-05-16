#' Tukey schematic plot
#'
#' The schematic plot is similar to the boxplot, but is designed as a more balanced
#' display, with a primary emphasis on the median and interquartile range.
#'
#' The upper and lower "hinges" correspond to the first and third quartiles
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
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "schematic")}
#'
#' @seealso \code{\link{geom_boxplot}} for comparison, \code{\link{stat_quantile}} to view quantiles
#'   conditioned on a continuous variable,  \code{\link{geom_jitter}} for another way to look
#'   at conditional distributions"
#' @inheritParams geom_point
#' @param middle.colour colour for the median point. Uses the default colour aesthetic
#'     if not specified.
#' @param outlier.colour colour for outlying points. Uses the default from geom_point().
#' @param outlier.shape shape of outlying points. Uses the default from geom_point().
#' @param outlier.size size of outlying points. Uses the default from geom_point().
#' @param varwidth if \code{FALSE} (default) make a standard box plot. If
#'    \code{TRUE}, boxes are drawn with widths proportional to the
#'    square-roots of the number of observations in the groups (possibly
#'    weighted, using the \code{weight} aesthetic).
#' @export
#'
#' @references McGill, R., Tukey, J. W. and Larsen, W. A. (1978) Variations of
#'     box plots. The American Statistician 32, 12-16.
#' @references Tukey J. W. (1990) Data-Based Graphics: Visual Display in the
#'     Decades to Come. Statistical Science 5(3), 327-339.
#' @references Cleveland W. S. (1993) Visualizing Data. p. 25
#'     Hobart Press, Summit, New Jersey
#'
#' @examples
#' \donttest{
#' p <- ggplot(mtcars, aes(factor(cyl), mpg))
#'
#' p + geom_schematic()
#' qplot(factor(cyl), mpg, data = mtcars, geom = "schematic")
#'
#' p + geom_schematic() + coord_flip()
#' qplot(factor(cyl), mpg, data = mtcars, geom = "schematic") +
#'   coord_flip()
#'
#' p + geom_schematic(outlier.colour = "green", outlier.size = 3)
#'
#' # Add aesthetic mappings
#' # Note that schematic plots are automatically dodged when any aesthetic is
#' # a factor
#' p + geom_schematic(aes(fill = cyl))
#' p + geom_schematic(aes(fill = factor(cyl)))
#' p + geom_schematic(aes(fill = factor(vs)))
#' p + geom_schematic(aes(fill = factor(am)))
#'
#' # Set aesthetics to fixed value
#' # The aesthetic whiskers.linetype can be used to change the whisker lines
#' p + geom_schematic(whiskers.linetype = "dotted")
#' # The aesthetic middle.size can be used change the size of the middle point
#' p + geom_schematic(middle.size = 6)
#' # Use the aesthetic shape to change the shape of the middle point
#' p + geom_schematic(shape = 22)
#' p + geom_schematic(fill = "grey80", colour = "#3366FF")
#' qplot(factor(cyl), mpg, data = mtcars, geom = "schematic",
#'   colour = I("#3366FF"))
#'
#' # Scales vs. coordinate transforms -------
#' # Scale transformations occur before the boxplot statistics are computed.
#' # Coordinate transformations occur afterwards.  Observe the effect on the
#' # number of outliers.
#' library(plyr) # to access round_any
#' m <- ggplot(movies, aes(y = votes, x = rating,
#'    group = round_any(rating, 0.5)))
#' m + geom_schematic()
#' m + geom_schematic() + scale_y_log10()
#' m + geom_schematic() + coord_trans(y = "log10")
#' m + geom_schematic() + scale_y_log10() + coord_trans(y = "log10")
#'
#' # Schematic plots with continuous x:
#' # Use the group aesthetic to group observations in schematic plots
#' qplot(year, budget, data = movies, geom = "schematic")
#' qplot(year, budget, data = movies, geom = "schematic",
#'   group = round_any(year, 10, floor))
#'
#' # Using precomputed statistics
#' # generate sample data
#' abc <- adply(matrix(rnorm(100), ncol = 5), 2, quantile, c(0, .25, .5, .75, 1))
#' b <- ggplot(abc, aes(x = X1, ymin = `0%`, lower = `25%`,
#'    middle = `50%`, upper = `75%`, ymax = `100%`))
#' b + geom_schematic(stat = "identity")
#' b + geom_schematic(stat = "identity") + coord_flip()
#' b + geom_schematic(aes(fill = X1), stat = "identity")
#'
#' # Using varwidth
#' p + geom_schematic(varwidth = TRUE)
#' qplot(factor(cyl), mpg, data = mtcars, geom = "schematic", varwidth = TRUE)
#'
#' # Update the defaults for the outliers by changing the defaults for geom_point
#'
#' p <- ggplot(mtcars, aes(factor(cyl), mpg))
#' p + geom_schematic()
#'
#' update_geom_defaults("point", list(shape = 1, colour = "red", size = 5))
#' p + geom_schematic()
#' }
geom_schematic <- function (mapping = NULL, data = NULL, stat = "boxplot",
                          position = "dodge", middle.colour = NULL,
                          outlier.colour = NULL, outlier.shape = NULL, outlier.size = NULL,
                          varwidth = FALSE,
                          ...) {

  outlier_defaults <- Geom$find('point')$default_aes()

  outlier.colour   <- outlier.colour %||% outlier_defaults$colour
  outlier.shape    <- outlier.shape  %||% outlier_defaults$shape
  outlier.size     <- outlier.size   %||% outlier_defaults$size

  GeomSchematic$new(mapping = mapping, data = data, stat = stat,
                  position = position, outlier.colour = outlier.colour,
                  outlier.shape = outlier.shape, outlier.size = outlier.size,
                  middle.colour = middle.colour, varwidth = varwidth, ...)
}

GeomSchematic <- proto(Geom, {
  objname <- "boxplot"

  reparameterise <- function(., df, params) {
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
  }

  draw <- function(., data, ..., fatten = 2, outlier.colour = NULL, outlier.shape = NULL, outlier.size = 2,
                   middle.colour = NULL, varwidth = FALSE) {
    common <- data.frame(
      colour = data$colour,
      size = data$size,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      stringsAsFactors = FALSE
    )

    whiskers <- data.frame(
      x    = c(data$x,     data$x,     data$xmin, data$xmin),
      xend = c(data$x,     data$x,     data$xmax, data$xmax),
      y    = c(data$upper, data$lower, data$ymin, data$ymax),
      yend = c(data$ymax,  data$ymin,  data$ymin, data$ymax),
      linetype = data$whiskers.linetype,
      alpha = NA,
      colour = data$colour,
      size = data$size,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      stringsAsFactors = FALSE)

    box <- data.frame(
      xmin = data$xmin,
      xmax = data$xmax,
      ymin = data$lower,
      y = data$middle,
      ymax = data$upper,
      alpha = data$alpha,
      common)

    midpt <- data.frame(
      x = data$x,
      y = data$middle,
      size = data$middle.size,
      shape = data$shape,
      alpha = data$alpha,
      colour = middle.colour %||% data$colour,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      stringsAsFactors = FALSE)

    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
      outliers <- data.frame(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier.colour %||% data$colour[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        fill = NA,
        alpha = NA,
        stringsAsFactors = FALSE)
      outliers_grob <- GeomPoint$draw(outliers, ...)
    } else {
      outliers_grob <- NULL
    }

    ggname(.$my_name(), grobTree(
      outliers_grob,
      GeomSegment$draw(whiskers, ...),
      GeomRect$draw(box, ...),
      GeomPoint$draw(midpt, ...)
    ))
  }

  guide_geom <- function(.) "schematic"
  draw_legend <- function(., data, ...)  {
    data <- aesdefaults(data, .$default_aes(), list(...))
    gp <- with(data, gpar(col=colour, fill=alpha(fill, alpha), lwd=size * .pt, lty = linetype))
    gTree(gp = gp, children = gList(
      linesGrob(0.5, c(0.1, 0.25)),
      linesGrob(0.5, c(0.75, 0.9)),
      linesGrob(c(0.125, 0.875), 0.9),
      linesGrob(c(0.125, 0.875), 0.1),
      rectGrob(height=0.5, width=0.75),
      pointsGrob(0.5, 0.5, size = unit(data$middle.size * 0.75, "mm"), pch = data$shape, gp = gpar(col = ...$middle.colour %||% data$colour))
    ))
  }

  default_stat <- function(.) StatBoxplot
  default_pos <- function(.) PositionDodge
  default_aes <- function(.) aes(weight=1, colour="grey20", fill="white", size=0.5, alpha = NA, shape = 16, linetype = "solid", whiskers.linetype = "dashed", middle.size = 4)
  required_aes <- c("x", "lower", "upper", "middle", "ymin", "ymax")

})
